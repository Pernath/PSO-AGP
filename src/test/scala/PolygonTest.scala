import org.scalatest.FlatSpec
import AGP._

class FirstSpec extends FlatSpec {
  // tests go here...
  /** Para verificar la conexion a la base de datos
    * 
    */
  "dentro()" should "decide if point is inside polygon" in {
    val p1 = new Punto(0,0)
    val p2 = new Punto(0,2)
    val p3 = new Punto(2,2)
    val p4 = new Punto(2,0)
    val prueba = new Punto(0.5,0.5)
    val puntos = new Array[Punto](4)
    puntos(0) = p1
    puntos(1) = p2
    puntos(2) = p3
    puntos(3) = p4
    val p = new Poligono(puntos)
    assert(p.dentro(p1))
    assert(p.dentro(p2))
    assert(p.dentro(p3))
    assert(p.dentro(p4))
    assert(p.dentro(prueba))
  }
}
