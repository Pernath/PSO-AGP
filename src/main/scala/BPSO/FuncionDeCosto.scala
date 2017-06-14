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
    var suma = 0
    var factible = true
    for(i <- 0 to nV.length-1) {
      if(nV(i) > nV.length)
        factible = false
      suma += nV(i)
    }
    return (normalizado((suma*nG)/nV.length),factible)
  }

  /** Método para normalizar el costo entre [0,1]
    * @param c el valor a normalizar
    * @return double el valor normalizado
    */
  def normalizado(c: Double): Double = {
    var size = poligono.puntos.length
    var max = size*size + size
    var min = 0
    return (c-min)/(max-min)
  }

  /** Método para generar el arreglo de vigilantes por punto y el número de guardias totales
    * @param pos el arreglo de posición de la partícula (si un vigilante está posicionado sobre un i)
    * @return tupla con el arreglo de vigilantes por punto y el número total de vigilantes
    */
  def nVigilantes(pos: Array[Boolean]): (Array[Int],Int) = {
    var nV = new Array[Int](poligono.puntos.length)
    var nG = 0
    for(i <- 0 to nV.length-1)
      nV(i) = nV.length+1
    for(i <- 0 to nV.length-1)
      if(pos(i)) {
        nG += 1
        for(j <- 0 to nV.length-1) {
          if(poligono.segmento_dentro(poligono.puntos(i),poligono.puntos(j))) {
            if(nV(i) > nV.length)
              nV(i) = 0
            nV(i) += 1
          }
        }
      }            
    return (nV, nG)
  }
}
