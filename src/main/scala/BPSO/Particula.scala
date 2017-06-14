package BPSO

/** Clase para modelar una particula en el BPSO
  * 
  */
class Particula(var guardias: Array[Boolean], var velocidad: Array[Double]) {
  var costo: Double = 0
  var mejorPos: Array[Boolean] = guardias.clone()
  var factible = false

  def length(): Int = { return guardias.length }

  def get(i: Int): Double = {
    if(guardias(i))
      return 1
    return 0
  }

  def getM(i: Int): Double = {
    if(mejorPos(i))
      return 1
    return 0
  }


  /** Configuración de los guardias en los vertices del polígono en cadena
    * 
    */
  override def toString(): String = {
    var s: String = "Factible: "+factible+"\nCosto: "+costo+"\n["
    for(i <- 0 to guardias.length-2)
      s += guardias(i)+", "
    return s+guardias(guardias.length-1)+"]"
  }
}
