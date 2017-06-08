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
    * @param a punto del guardia
    * @return true si no intersecta a ninguna arista del polígono y su punto medio está dentro, false en otro caso 
    */
  def segmento_dentro(a: Punto, b: Punto) : Boolean = {
    var in = false
    for(i <- 0 to puntos.length-1) {
      if(i < puntos.length-1)
        in = in || interseccion(a,b,puntos(i),puntos(i+1))
      else
        in = in || interseccion(a,b,puntos(i),puntos(0))
    }    
    return !in && dentro(new Punto((a.x+b.x)/2,(a.y+b.y)/2))
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

  /** Método para decidir la orientación de tres vertices
    * @param p el primer punto en la tripleta
    * @param q el segundo punto en la tripleta
    * @param r el tercer punto en la tripleta
    * @return 0 si p y q son colineales, 1 si estan en sentido horario, 2 sentido contrario
    */
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

  /** Método para decidir si un conjunto de cuatro vertices son todos distintos
    * @return true si al menos un par son iguales, false si todos son distintos
    */
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


  /** Método para decidir si hay una intersección entre los segmentos de línea p1q1 y p2q2
    * @param p1 extremo del segmento p1q1
    * @param q1 extremo del segmento p1q1
    * @param p2 extremo del segmento p2q2
    * @param q2 extremo del segmento p2q2
    * @return true en caso de que p1q1 intersecte a p2q2
    */
  def interseccion(p1: Punto, q1: Punto, p2: Punto, q2: Punto): Boolean = {
    //Verificación de compartición de puntos
    if(comparar(p1,q1,p2,q2))
      return false;
    // orientaci[on para los casos especiales
    val o1 = orientacion(p1, q1, p2)
    val o2 = orientacion(p1, q1, q2)
    val o3 = orientacion(p2, q2, p1)
    val o4 = orientacion(p2, q2, q1)
    
    // Caso general, intersección 
    if (o1 != o2 && o3 != o4)
      return true
    
    /* Casos especiales */
    // p1, q1 y p2 son colineales y p2 esta sobre el segmento p1q1
    if (o1 == 0 && enSegmento(p1, p2, q1)) return true
    
    // p1, q1 and p2 son colineales y q2 esta sobre el segmento p1q1
    if (o2 == 0 && enSegmento(p1, q2, q1)) return true
    
    // p2, q2 and p1 son colineales y p1 esta sobre el segmento p2q2
    if (o3 == 0 && enSegmento(p2, p1, q2)) return true
    
    // p2, q2 and q1 son colineales y q1 esta sobre el segmento p2q2
    if (o4 == 0 && enSegmento(p2, q1, q2)) return true
    
    return false // Ninguno de los casos especiales ni el general, no intersectan
  }




  /** Metodo para calcular el producto cruz entre dos puntos coordenados
    * 
    */
  def prod_cruz(a: Punto, b: Punto): Double = {
    return (a.x-b.y)*(a.y-b.x)
  }
}
