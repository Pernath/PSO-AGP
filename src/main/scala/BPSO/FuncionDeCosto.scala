package BPSO
import AGP.Poligono

/** Clase para modelar la función de costo a utilizar en la evaluacion de las partículas
  * 
  */
class FuncionDeCosto(var poligono: Poligono) {


  /** Método para evaluar una solución 
    * @param pos el arreglo que dice en qué vertices están posicionados los guardias
    */
  def eval(pos: Array[Boolean]): (Double,Boolean) = {
    var t = nVigilantes(pos)
    var nV = t._1
    var nG = t._2
    var suma = 0.0
    var factible = true
    for(i <- 0 to nV.length-1) {
      if(nV(i) > nV.length)
        factible = false
      suma = suma + nV(i)
    }
    if(nG > 0)
      return (normalizado((suma*nG)/nV.length),factible)
    return (Double.MaxValue,false)
  }

  /** Método para normalizar el costo entre [0,1]
    * @param c el valor a normalizar
    * @return double el valor normalizado
    */
  def normalizado(c: Double): Double = {
    var size = poligono.puntos.length
    var max: Double = size*size
    var min = 0.0
    var costo = (c-min)/(max-min)
    //println(costo)
    return costo
  }

  /** Método para generar el arreglo de vigilantes por punto y el número de guardias totales
    * @param pos el arreglo de posición de la partícula (si un vigilante está posicionado sobre un i)
    * @return tupla con el arreglo de vigilantes por punto y el número total de vigilantes
    */
  def nVigilantes(pos: Array[Boolean]): (Array[Double],Double) = {
    var nV = new Array[Double](poligono.puntos.length)
    var nG = 0.0
    for(i <- 0 to nV.length-1)
      nV(i) = nV.length+1
    
    for(i <- 0 to nV.length-1)
      if(pos(i)) {
        nG = nG + 1.0
        for(j <- 0 to nV.length-1) {
          if(poligono.segmento_dentro(poligono.puntos(i),poligono.puntos(j))) {
            if(nV(j) > nV.length)
              nV(j) = 0.0
            nV(j) = nV(j) + 1.0
          }
        }
      }
    return (nV, nG)
  }
}
