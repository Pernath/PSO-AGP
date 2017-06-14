package BPSO

/** Clase para modelar la heurística de Optimización por Enjambre de Partículas versión Binaria
  * 
  */
class BPSO(var seed: Int, var size: Int, var omega: Double, var phi: Double ,var phi2: Double, val gen: GeneradorVerificador, val cTerminacion: CTerminacion) {
  var enjambre: Enjambre = null

  /** Método de ejecucion
    * 
    * 
    */
  def run() {
    enjambre = gen.genera_poblacion(size)
    while (cTerminacion.continua) {
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
        particula.guardias(i) = true
      else
        particula.guardias(i) = false
    }
    gen.evalua(particula) //condicion de mejora
    if(particula.costo < enjambre.mejor.costo) // agregar condicion de mejora ??
      enjambre.mejor = particula
    enjambre.particulas(i) = particula
  }


}
