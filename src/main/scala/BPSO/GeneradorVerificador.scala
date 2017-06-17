package BPSO

import scala.util.Random

/** Clase para manejar la generación aleatoria de poblaciones iniciales,
  * su evaluación y la factibilidad de las soluciones
  */
class GeneradorVerificador(var semilla: Int, val func: FuncionDeCosto) {
  val generador = new Random(semilla)

  /** Método para generar una nueva población aleatoria
    * Se evalúa cada nueva solución creada
    */
  def genera_poblacion(t: Int): Enjambre = {
    val p = func.poligono.puntos.length
    val poblacion = new Enjambre(t)
    var part = part_random(p)
    println(part)
    println(part.nV)
    poblacion.particulas(0) = part.clone
    poblacion.mejor = part.clone
    //poblacion.mejor.costo = Double.MaxValue
    for(i <- 1 to t-1) {
      part = part_random(p)
      if(part.costo < poblacion.mejor.costo)
        poblacion.mejor = part.clone
      poblacion.particulas(i) = part.clone 
    }
    return poblacion
  }

  /** Método para generar una partícula aleatoria, esta debe tener tanto su costo como su mejor 
    * posicion y su velocidad ya definidos
    * @param el tamaño de los vectores de la partícula, es decir, el número de vértices de la 
    * instancia
    * @return la partícula ya con todos sus atributos asignados
    */
  def part_random(p: Int): Particula = {
    var velocidad = new Array[Double](p)
    var posicion = new Array[Boolean](p)
    var vigilantes = new Array[Double](p)
    for(i <- 0 to p-1){
      velocidad(i) = generador.nextDouble()
      posicion(i) = generador.nextBoolean()
      if(posicion(i)){
        if(vigilantes(i) > p)
          vigilantes(i) = 1
        else
          vigilantes(i) = vigilantes(i)+1
        
        for(j <- 0 to p-1) {
          if(func.matrix(i)(j)){
            if(vigilantes(j) > p)
              vigilantes(j) = 1
            else
              vigilantes(j) = vigilantes(j) + 1
          }
        }
      } else if(vigilantes(i) == 0)
        vigilantes(i) = (p+1)*99
    }
    var particula = new Particula(posicion,velocidad,vigilantes)
    evalua(particula)    
    return particula
  }

  def updateP(p: Particula, i: Int, b: Boolean) {
    if(p.guardias(i) != b){
      p.guardias(i) = b
      if(p.nVigilantes(i) > p.length) {
        if(b)
          p.nVigilantes(i) = 1
      }
      else {
        if(b)
          p.nVigilantes(i) = p.nVigilantes(i) + 1
        else {
          p.nVigilantes(i) = p.nVigilantes(i) - 1
          if(p.nVigilantes(i) == 0)
            p.nVigilantes(i) = (p.length+1)*99
        }
      }
      for(j <- 0 to p.length-1) {
        var nv = p.nVigilantes(j)
        if(func.matrix(i)(j)){
          if(nv > p.length) {
            if(b) {
              p.nVigilantes(j) = 1
              p.nGuardias += 1
            }
            else {
              p.nGuardias -= 1
            }
          } else { //ya alguien le cuidaa
            if(b){
              p.nVigilantes(j) = nv + 1
              p.nGuardias += 1
            }
            else{
              p.nVigilantes(j) = nv - 1
              if(p.nVigilantes(j) == 0)
                p.nVigilantes(j) = (p.length+1)*99
              p.nGuardias -= 1
            }
          }
        }
      }
    }
  }

  /** Método para evaluar una partícula. Al término, se actualizará tanto su costo como su 
    * mejor posición de ser necesario
    * @param p la particula a evaluar
    */
  def evalua(p: Particula): Boolean = {
    var mejora = false
    var nuevoC = func.eval(p.nVigilantes, p.nGuardias)
    if(nuevoC._1 < p.costo) {
      mejora = true
      p.mejorPos = p.guardias.clone
    }
    p.costo = nuevoC._1
    p.factible = nuevoC._2
    return mejora
  }
}
