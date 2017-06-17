import AGP._
import java.io.{PrintWriter,File}

class Draw2(var pol: Poligono, var guardias: Array[Boolean]){
  var svg = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE svg>"
  var max = pol.maxXY()
  var min = pol.minXY()
  var aug = 100
  var l = pol.puntos.length

  def draw() {
    var ancho = max._1 - min._1
    var alto = max._2 - min._2
    var s1: Double = 0
    var s2: Double = 0
    
    if(ancho > alto){
      var prop = ancho/alto
      s1 = 500*prop
      s2 = 500
    } else {
      var prop = alto/ancho
      s2 = 500*prop
      s1 = 500
    }

    var delta1 = s1/(max._1-min._1)
    var delta2 = s2/(max._2-min._2)

    svg += "\n<svg width=\""+s1+"\" height=\""+s2+"\">"
    svg += "<rect width=\""+s1+"\" height=\""+s2+"\" style=\"fill:white\" />"

    for(i <- 0 to l-1){
      svg += "\n<line x1=\""+(delta1*(pol.puntos(i).x-min._1))+"\" y1=\""+(s2-(delta2*(pol.puntos(i).y-min._2)))+"\" x2=\""+(delta1*(pol.puntos((i+1)%l).x-min._1))+"\" y2=\""+(s2-(delta2*(pol.puntos((i+1)%l).y-min._2)))+"\" style=\"stroke:black;stroke-width:4\" />"

      if(guardias(i))
        svg += "\n<ellipse cx=\""+(delta1*(pol.puntos(i).x-min._1))+"\" cy=\""+(s2-(delta2*(pol.puntos(i).y-min._2)))+"\" rx=\"5\" ry=\"5\" style=\"fill:red;stroke:black;stroke-width:1\" />"	

    }

    svg += "\n</svg>"//ultima accion
  }


  def write(fNombre: String): Boolean = {
    draw()
    var pattern = "([A-Za-z/]*)([/]+)([A-Za-z]+).([A-Za-z]+)".r    
    var nombre = ""
    try {
      val pattern(dir,last,nom,ext) = fNombre
      nombre = nom
    } catch {
      case e: Exception =>
        pattern = "([A-Za-z]+).([A-Za-z]+)".r
        val pattern(nom,ext) = fNombre
        nombre = nom
    }
    val pw = new PrintWriter(new File("instances/solutions/"+nombre+".svg"))
    //var fine = false
    pw.write(this.svg)
    pw.close()
    return true
  }


}
