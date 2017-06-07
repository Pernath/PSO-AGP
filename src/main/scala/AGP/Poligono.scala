package AGP

/** Clase para modelar un polígono
  * Suponemos que tanto (i,i+1) como (i,i-1) son segmentos del polígono. 
  */
class Poligono(val puntos: Array[Punto]) {


  /** Método para decidir si un punto está dentro del polígono
    * Se supone que están los puntos en orden con las manecillas del reloj
    * (por ángulo, tomando el primero como referencia)
    * @param punto el punto a verificar
    * @return boolean true si el punto se encuentra dentro del polígono, false en caso contrario
    */
  def dentro(punto: Punto) : Boolean = {
    val l = puntos.length
    for(i <- 0 to l-1)
      if(prod_cruz(puntos((i+1)%l).resta(puntos(i)),punto.resta(puntos(i))) > 0 )
        return false
    return true
  }

  /** Método para decidir si el segmento de línea que intersecta a y b está completamente dentro de P
    * @param a el extremo del segmento
    * @param b el otro extremo del segmento
    * @return true si se encuentra dentro de todo el polígono, false en caso contrario
    */
  def dentro(a: Punto, b:Punto) : Boolean = {
    return false
  }


  /** Metodo para calcular el producto cruz entre dos puntos coordenados
    * 
    */
  def prod_cruz(a: Punto, b: Punto): Double = {
    return (a.x-b.y)*(a.y-b.x)
  }
}
