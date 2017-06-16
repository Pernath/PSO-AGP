import AGP._
import java.awt.image.BufferedImage
import java.awt.{Graphics2D,Color,Font,BasicStroke}
import java.awt.geom._


class Draw(var pol: Poligono, var p: Array[Boolean]){
  var l = pol.puntos.length
  //var xs = new Array[Double](l)
  //var ys = new Array[Double](l)

  def set(){
  
    var path = new Path2D.Double();

    path.moveTo(pol.puntos(0).x, pol.puntos(0).y);
    for(i <- 1 to l-1) {
      path.lineTo(pol.puntos(i).x, pol.puntos(i).y);
    }
    

  }

  def draw() {
    set()
    // Size of image
    var max = pol.maxXY()
    var min = pol.minXY()

    println(max)
    println(min)
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

    var aug = 100
    println("nuevo ancho:"+s1)
    var delta1 = s1/(max._1-min._1)
    var delta2 = s2/(max._2-min._2)
    // create an image
    //val canvas = new BufferedImage(size._1.toInt, size._2.toInt, BufferedImage.TYPE_INT_RGB)
    val canvas = new BufferedImage(s1.toInt, s2.toInt, BufferedImage.TYPE_INT_RGB)

    // get Graphics2D for the image
    val g = canvas.createGraphics()

    // clear background
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)

    // enable anti-aliased rendering (prettier lines and circles)
    // Comment it out to see what this does!
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING,
      java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    // draw a filled and an unfilled Rectangle
    g.setColor(Color.CYAN)
    g.setStroke(new BasicStroke(5));
    var path = new Path2D.Double();
    //path.moveTo(50+pol.puntos(0).x/delta1, 50+s2-(pol.puntos(0).y/delta2));
    
    for(i <- 0 to l-1) {
      g.draw(new Line2D.Double(delta1*(pol.puntos(i).x-min._1), s2-(delta2*(pol.puntos(i).y-min._2)),
        delta1*(pol.puntos((i+1)%l).x-min._1) , s2-(delta2*(pol.puntos((i+1)%l).y-min._2))));
    }
    
    //path.lineTo(50+pol.puntos(0).x*500, 50+alto-(pol.puntos(0).y*500));
    //g.draw(path)
    g.setColor(Color.RED)
    println("nuevo alto:"+s2)
    println("proporcion alto:"+delta2)
    println("proporcion ancho:"+delta1)

    println("jai"+delta1*(2-min._1))
    println(s2-(delta2*3))
    g.draw(new Line2D.Double(s1*(2-min._1),s2-(s2*(3-min._2)),0,0 ))

    //g.fill(new Ellipse2D.Double(0,450, 70.0, 70.0))
//     done with drawing
    g.dispose()

    // write image to a file
    javax.imageio.ImageIO.write(canvas, "png", new java.io.File("drawing.png"))
  }
}
