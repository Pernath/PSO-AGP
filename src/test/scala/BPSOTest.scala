import org.scalatest.FlatSpec
import AGP._
import BPSO._

class SecondSpec extends FlatSpec {
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


  puntos = new Array[Punto](12)
  puntos(0) = new Punto(1,1)
  puntos(1) = new Punto(2,3)
  puntos(2) = new Punto(3,2)
  puntos(3) = new Punto(4,2)
  puntos(4) = new Punto(5,3)
  puntos(5) = new Punto(6,2)
  puntos(6) = new Punto(7,2)
  puntos(7) = new Punto(8,3)
  puntos(8) = new Punto(9,2)
  puntos(9) = new Punto(10,2)
  puntos(10) = new Punto(11,3)
  puntos(11) = new Punto(12,1)
  val w = new Poligono(puntos) 


  /** Para verificar la conexion a la base de datos
    * 
    */
  "run()" should "resolver para rectángulo" in {
    var func = new FuncionDeCosto(p)
    var gen = new GeneradorVerificador(1,func)
    var cterm = new CTerminacion(100)
    var bpso = new BPSO(100,2.0,2.0,2.0,gen,cterm)
    //var bpso = new BPSO(2,1000,0.1,0.1,0.1,gen,cterm)    
    bpso.run()
    println(bpso.enjambre.mejor)
    println(toS(func.nVigilantes(bpso.enjambre.mejor.guardias)._1))
    assert(bpso.enjambre.mejor.factible)
    //println(toS(func.nVigilantes(a)._1))
    //println(func.eval(a))
  }

  "run()" should "resolver para peine" in {
    var func = new FuncionDeCosto(w)
    var gen = new GeneradorVerificador(1,func)
    var cterm = new CTerminacion(100)
    var bpso = new BPSO(100,1.0,1.0,1.0,gen,cterm)
    //var bpso = new BPSO(2,1000,0.1,0.1,0.1,gen,cterm)
    bpso.run()
    println(bpso.enjambre.mejor)
    println(toS(func.nVigilantes(bpso.enjambre.mejor.guardias)._1))
    assert(bpso.enjambre.mejor.factible)
    //println(toS(func.nVigilantes(a)._1))
    //println(func.eval(a))
  }

  def toS(a: Array[Double]): String = {
    var s: String = "["
    for(i <- 0 to a.length-2)
      s += a(i)+", "
    return s+a(a.length-1)+"]"
  }

  def randomPolygon(seed: Int, s: Int): Poligono = {
//    var gen = new math.Random(seed)
    return null
  }

}
