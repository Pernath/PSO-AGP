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
    poblacion.particulas(0) = part
    poblacion.mejor = part
    for(i <- 1 to t-1) {
      part = part_random(p)
      if(part.costo < poblacion.mejor.costo)
        poblacion.mejor = part
      poblacion.particulas(i) = part 
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
    for(i <- 0 to p-1){
      velocidad(i) = generador.nextDouble()
      posicion(i) = generador.nextBoolean()
    }
    var particula = new Particula(posicion,velocidad)
    //particula.costo = func.eval(posicion)    
    return particula
  }

  /** Método para evaluar una partícula. Al término, se actualizará tanto su costo como su 
    * mejor posición de ser necesario
    * @param p la particula a evaluar
    */
  def evalua(p: Particula): Boolean = {
    var mejora = false
    var nuevoC = func.eval(p.guardias)
    if(nuevoC._1 < p.costo) {
      mejora = true
      p.mejorPos = p.guardias.clone
    }
    p.costo = nuevoC._1
    p.factible = nuevoC._2
    return mejora
  }
}
