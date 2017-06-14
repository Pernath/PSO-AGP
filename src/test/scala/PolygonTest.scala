import org.scalatest.FlatSpec
import AGP._

class FirstSpec extends FlatSpec {
  // tests go here...
  val p1 = new Punto(0,0)
  val p2 = new Punto(0,2)
  val p3 = new Punto(2,2)
  val p4 = new Punto(2,0)
  var puntos = new Array[Punto](4)
  puntos(0) = p1
  puntos(1) = p2
  puntos(2) = p3
  puntos(3) = p4
  val p = new Poligono(puntos)

  puntos = new Array[Punto](3)
  puntos(0) = new Punto(1,1)
  puntos(1) = new Punto(2,3)
  puntos(2) = new Punto(3,2)
  //puntos(3) = new Punto(4,2)
 // puntos(4) = new Punto(5,3)
  //puntos(5) = new Punto(6,2)
  //puntos(6) = new Punto(7,2)
  //puntos(7) = new Punto(8,3)
  //puntos(8) = new Punto(9,2)
  //puntos(9) = new Punto(10,2)
  //puntos(10) = new Punto(11,3)
  //puntos(11) = new Punto(12,1)
  val w = new Poligono(puntos)

  /** Para verificar la conexion a la base de datos
    * 
    */
  "dentro()" should "decide if point is inside polygon" in {
    val prueba = new Punto(0.5,0)
    assert(p.dentro(p1))
    assert(p.dentro(p2))
    assert(p.dentro(p3))
    assert(p.dentro(p4))
    assert(p.dentro(prueba))
    assert(!p.dentro(new Punto(3,3)))

    for(i <- 0 to w.puntos.length-1)
      assert(w.dentro(w.puntos(i)))
  }

  "interseccion()" should "decide if line segment intersects edge of polygon" in {
    var pruebaA =  new Punto(-1,-1)
    var pruebaB = new Punto(-2,-2)
    assert(!p.interseccion(pruebaA,pruebaB,p.puntos(0),p.puntos(1)))
    pruebaA = new Punto(0,0)
    pruebaB = new Punto(0,2)
    assert(!p.interseccion(pruebaA,pruebaB,p.puntos(0),p.puntos(1)))
    assert(!p.interseccion(pruebaA,p.puntos(2),p.puntos(1), p.puntos(2)))
  }

  "segmento_dentro()" should "decide if line segment lies inside the polygon" in {
    var extremoA = new Punto(0.25,0.25)
    var extremoB = new Punto(0.5,0.5)
    assert(p.segmento_dentro(extremoA,extremoB))
    for(i <- 0 to p.puntos.length-1){
      for(j <- 0 to p.puntos.length-1)
        assert(p.segmento_dentro(p.puntos(i),p.puntos(j)))
    }
    extremoA = new Punto(-13,-12)
    assert(!p.segmento_dentro(extremoA,extremoB))

    
    for(i <- 0 to p.puntos.length-1){
      for(j <- 0 to p.puntos.length-1){
        //println("i: "+i+",j: "+j)
        if(i != j){}
          //assert(w.dentro(new Punto((w.puntos(i).x+w.puntos(j).x)/2,(w.puntos(i).y+w.puntos(j).y)/2)))
      }
    }
    //assert(w.segmento_dentro(w.puntos(11),w.puntos(0)))
    //assert(w.segmento_dentro(w.puntos(11),w.puntos(10)))
    //assert(w.segmento_dentro(w.puntos(11),w.puntos(9)))
    //assert(w.segmento_dentro(w.puntos(11),w.puntos(8)))
  }

  "referencia()" should "update object" in {
    var p = new Punto(5,0)
    var q = 1+2
    var w = q+1.0
    assert(w == 4)
  }
}
