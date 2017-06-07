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

  /** Método para decidir si un punto q yace sobre un segmento pr
    * @param p el primer punto del segmento
    * @param q el punto a verificar
    * @param r el segundo punto del segmento
    * @return true si el punto q está sobre pr, false en caso contrario
    */
  def enSegmento(p: Punto, q: Punto, r: Punto): Boolean = {
    if (q.x <= math.max(p.x, r.x) && q.x >= math.min(p.x, r.x) &&
      q.y <= math.max(p.y, r.y) && q.y >= math.min(p.y, r.y))
      return true
    return false
  }

  // To find orientation of ordered triplet (p, q, r).
  // The function returns following values
  // 0 --> p, q and r are colinear
  // 1 --> Clockwise
  // 2 --> Counterclockwise
  def orientacion(p: Punto, q: Punto, r: Punto): Int = {
    val v = (q.y - p.y) * (r.x - q.x) -
      (q.x - p.x) * (r.y - q.y)
    if (v == 0) // colineales
      return 0  
    if(v > 0)
      return 1
    else
      return 2
  }

  def comparar(p1: Punto, p2: Punto, p3: Punto, p4: Punto): Boolean = {
    var n = new Array[Punto](4)
    n(0) = p1
    n(1) = p2
    n(2) = p3
    n(3) = p4
    for(i <- 0 to 3)
      for(j <- 0 to 3)
        if(i != j)
          if(n(i).equals(n(j)))
            return true
    return false

  }


  // The main function that returns true if line segment 'p1q1'
  // and 'p2q2' intersect.
  def interseccion(p1: Punto, q1: Punto, p2: Punto, q2: Punto): Boolean = {
    // Find the four orientations needed for general and
    // special cases
    if(comparar(p1,q1,p2,q2))
      return false;
    val o1 = orientacion(p1, q1, p2)
    val o2 = orientacion(p1, q1, q2)
    val o3 = orientacion(p2, q2, p1)
    val o4 = orientacion(p2, q2, q1)
    
    // General case
    if (o1 != o2 && o3 != o4)
      return true
    
    // Special Cases
    // p1, q1 and p2 are colinear and p2 lies on segment p1q1
    if (o1 == 0 && enSegmento(p1, p2, q1)) return true
    
    // p1, q1 and p2 are colinear and q2 lies on segment p1q1
    if (o2 == 0 && enSegmento(p1, q2, q1)) return true
    
    // p2, q2 and p1 are colinear and p1 lies on segment p2q2
    if (o3 == 0 && enSegmento(p2, p1, q2)) return true
    
    // p2, q2 and q1 are colinear and q1 lies on segment p2q2
    if (o4 == 0 && enSegmento(p2, q1, q2)) return true
    
     //
    return false // Doesn't fall in any of the above cases
  }




  /** Metodo para calcular el producto cruz entre dos puntos coordenados
    * 
    */
  def prod_cruz(a: Punto, b: Punto): Double = {
    return (a.x-b.y)*(a.y-b.x)
  }
}
