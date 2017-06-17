import BPSO._
import AGP._

class Controlador(instance: List[List[Double]]) extends Thread {
  var bpso: BPSO = null
  var poligono = genPoligono(instance)

  def genPoligono(instance: List[List[Double]]): Poligono =  {
    var p = new Array[Punto](instance.size)
    for(i <- 0 to instance.size-1){
      var t = instance(i)
      p(i) = new Punto(t(0),t(1))
    }
    return new Poligono(p)
  }

  def exec(s: Int, t: Int, mi: Int, w: Double, p1: Double, p2: Double) {
    var func = new FuncionDeCosto(poligono)
    var gen = new GeneradorVerificador(s,func)
    var cterm = new CTerminacion(mi)
    bpso = new BPSO(t,w,p1,p2,gen,cterm)
    //var bpso = new BPSO(2,1000,0.1,0.1,0.1,gen,cterm)    
  }


  def toS(a: Array[Double]): String = {
    var s: String = "["
    for(i <- 0 to a.length-2)
      s += a(i)+", "
    return s+a(a.length-1)+"]"
  }

  override def run() {
    bpso.run()
    println(bpso.enjambre.mejor)
    println(bpso.enjambre.mejor.nV)
    //println(toS(func.nVigilantes(bpso.enjambre.mejor.guardias)._1))
  }


}
