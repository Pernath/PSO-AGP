import org.scalatest.FlatSpec
import AGP._

class FirstSpec extends FlatSpec {
  // tests go here...
  val p1 = new Punto(0,0)
  val p2 = new Punto(0,2)
  val p3 = new Punto(2,2)
  val p4 = new Punto(2,0)
  val puntos = new Array[Punto](4)
  puntos(0) = p1
  puntos(1) = p2
  puntos(2) = p3
  puntos(3) = p4
  val p = new Poligono(puntos)

  /** Para verificar la conexion a la base de datos
    * 
    */
  "dentro()" should "decide if point is inside polygon" in {
    val prueba = new Punto(0.5,0.5)
    assert(p.dentro(p1))
    assert(p.dentro(p2))
    assert(p.dentro(p3))
    assert(p.dentro(p4))
    assert(p.dentro(prueba))
    assert(!p.dentro(new Punto(3,3)))
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
}
