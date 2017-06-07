package AGP

/** Clase para modelar puntos del polígono
  * @param x la coordenada x en el plano
  * @param y la coordenada y en el plano
  */
class Punto(val x: Double, val y: Double) {

  def resta(punto: Punto): Punto = {
    return new Punto(x-punto.x, y-punto.y)
  }


  override def equals(o: Any): Boolean = {    
    var otro = o.asInstanceOf[Punto];
    if(otro.x == x && otro.y == y)
      return true
    return false
  }

  override def toString(): String = {
    return "("+x+","+y+")"
  }
}
