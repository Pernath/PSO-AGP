package BPSO

/** Clase para modelar una particula en el BPSO
  * 
  */
class Particula(var guardias: Array[Boolean], var velocidad: Array[Double], var nVigilantes: Array[Double]) {
  var costo: Double = 0
  var mejorPos: Array[Boolean] = guardias.clone()
  var factible = false
  var nGuardias = getG(guardias)

  override def clone(): Particula = {
    var p = new Particula(this.guardias.clone, this.velocidad, this.nVigilantes.clone)
    p.costo = this.costo + 0.0 //necesario?
    p.factible = this.factible || false //necesario?
    return p
  }

  def getG(ga: Array[Boolean]): Int = {
    var n = 0
    for(i <- 0 to ga.length-1)
      if(ga(i))
        n += 1
    return n
  }

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
    var s: String = "Factible: "+factible+"\nCosto: "+costo+"\n"
    return s
  }

  def g(): String = {
    var s: String = "["
    for(i <- 0 to guardias.length-2)
      s += guardias(i)+", "
    return s+guardias(guardias.length-1)+"]"
  }

  def nV(): String = {
    var s: String = "["
    for(i <- 0 to nVigilantes.length-2)
      s += nVigilantes(i)+", "
    return s+nVigilantes(nVigilantes.length-1)+"]"
  }
}
