package BPSO

/** Clase para modelar una particula en el BPSO
  * 
  */
class Particula(var guardias: Array[Boolean], velocidad: Double) {
  var costo: Double = 0
  var mejorPos: Array[Boolean] = guardias.clone()



  /** Configuración de los guardias en los vertices del polígono en cadena
    * 
    */
  override def toString(): String = {
    var s: String = "["
    for(i <- 0 to guardias.length-2)
      s += guardias(i)+", "
    return s+guardias(guardias.length-1)+"]"
  }
}
