import org.scalatest.FlatSpec
import AGP._
import BPSO._

class SecondSpec extends FlatSpec {
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
  "run()" should "resolve for square" in {
    var func = new FuncionDeCosto(p)
    var gen = new GeneradorVerificador(1,func)
    var cterm = new CTerminacion(100)
    var bpso = new BPSO(2,100,2.0,2.0,2.0,gen,cterm)
    bpso.run()
    println(bpso.enjambre.mejor)
  }

}
