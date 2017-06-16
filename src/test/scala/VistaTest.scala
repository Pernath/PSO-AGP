import org.scalatest.FlatSpec
import AGP._

class VistaTest extends FlatSpec {
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
  var l = List(List(0.0,0.0),List(0.0,2.0),List(2.0,2.0), List(2.0,0.0))
  //val v = new Vista(1)
  val c = new Controlador(l)
  


  "run()" should "be multithreaded" in {
    /*
    c.exec(1,100,100,2.0,2.0,2.0)
    c.run()
    v.run()
    try{
      c.join()
      v.join()
    } catch {
      case ie: InterruptedException => println("Error en barrera")
    }*/

  }
}
