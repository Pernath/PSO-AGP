package BPSO

/** Clase para modelar la heurística de Optimización por Enjambre de Partículas versión Binaria
  * 
  */
class BPSO(var size: Int, var omega: Double, var phi: Double ,var phi2: Double, val gen: GeneradorVerificador, val cTerminacion: CTerminacion) {
  var enjambre: Enjambre = null

  /** Método de ejecucion
    * 
    * 
    */
  def run() {
    enjambre = gen.genera_poblacion(size)
    while (cTerminacion.continua) {
      println(cTerminacion.iter)
      for(i <- 0 to size-1) {
        actualizaV(i)
        actualizaP(i) //aqui ocurrirá la actualización de la mejorPos y la global
      }
      cTerminacion.progress
    }
  }

  /** Método para actualizar la velocidad de la partícula
    * @param i el índice correspondiente a la partícula en el enjambre
    */
  def actualizaV(i: Int) {
    var particula = enjambre.particulas(i)
    for(i <- 0 to particula.length-1) {
      var vi = particula.velocidad(i)
      particula.velocidad(i) = omega*vi + phi*gen.generador.nextDouble()*(particula.getM(i)-particula.get(i)) + phi2*gen.generador.nextDouble()*(enjambre.mejor.get(i)-particula.get(i))
    }
    enjambre.particulas(i) = particula
  }

  /** Método para actualizar la posición de una partícula
    * @param i el índice correspondiente a la partícula en el enjambre
    */
  def actualizaP(i: Int) {
    var particula = enjambre.particulas(i)
    for(i <- 0 to particula.length-1){
      var vi = particula.velocidad(i)
      var sig = 1 / (1+math.pow(math.E,-vi)) //normalizacion
      if(gen.generador.nextDouble() < sig)
        gen.updateP(particula,i,true)
      else
        gen.updateP(particula,i,false)
    }
    var mejora = gen.evalua(particula) //condicion de mejora

    if(mejora && particula.costo < enjambre.mejor.costo) { // agregar condicion de mejora ??
      println("Mejora costo: "+particula.costo)
      enjambre.mejor = particula.clone
    }
    enjambre.particulas(i) = particula
  }


}
