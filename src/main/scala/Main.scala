import java.io.{FileNotFoundException, IOException}
import scala.io.Source
import scala.collection.mutable.ListBuffer
import javafx.application.Application
import scala.util.control.Breaks._

object Main{

  val help = "Modo de empleo: <run> <archivo>"
  val bienvenida = "\n\n| Heurísticas de optimización combinatoria  |\n|\t\tProyecto 2\t\t    |\n|\t     Semestre 2017-2\t\t    |\n|\tFacultad de Ciencias UNAM\t    |\n| Autor: Carlos Gerardo Acosta Hernández    |\n\n"
  val fileHelp = ""
  var tupleConf: (List[List[Double]], List[Double]) = null

  def conf(filename: String) {
    try{
      val file = Source.fromFile(filename)
      var tuple = pars(file)
      tupleConf = tuple
    }
    catch{
      case ex: FileNotFoundException =>
        println("No se encuentra el archivo de configuración "+filename)
      case iox: IOException =>
        println("No se puede leer el archivo "+filename)
      case df: Exception =>
        println("Error en el formato del archivo.")
    }
  }

  def pars(file: Source): (List[List[Double]], List[Double]) = {
    val iter = file.getLines()
    val params = iter.next().split(",").map(_.toDouble).toList
    var agp: ListBuffer[List[Double]] = ListBuffer()
    while(iter.hasNext) {
      var t = iter.next().split(",").map(_.toDouble).toList
      agp += t
    }
    //println(agp)
    return (agp.toList,params)
  }

  /*
  def toTuple(s: String): List[Double] = {
    if(s(0) != "(")
      throw Exception


   }*/
  def main(args: Array[String]): Unit = {
    println(bienvenida)
    if(args.length < 1){
      println("Error de uso")
      println(help)
      return
    }

    conf(args(0))//establecemos los datos del archivo de config

    if(tupleConf == null || tupleConf._2.size < 6){
      println(tupleConf == null)
      println("Error en el archivo de configuración."+fileHelp)
      return
    }
    val cont = new Controlador(tupleConf._1)
    var l = tupleConf._2
    val s = l(0).toInt //semilla
    val t = l(1).toInt//tamaño enjambre
    val mi = l(2).toInt//maximo iteraciones
    val w = l(3)//omega
    val p1 = l(4)//phi1
    val p2 = l(5)//phi2

    var i = 0
    var n = 100
    /*while(n > 26 && i < 100) {      
      cont.exec(i,t,mi,w,p1,p2)
      cont.run
      n = cont.bpso.enjambre.mejor.nGuardias
      i += 1
  }*/
    cont.exec(s,t,mi,w,p1,p2)
    cont.run
    println("Semilla:"+s)
    var draw = new Draw2(cont.bpso.gen.func.poligono, cont.bpso.enjambre.mejor.guardias)
    //var draw = new Draw(cont.bpso.gen.func.poligono, null)
    draw.write(args(0))
  }
}
