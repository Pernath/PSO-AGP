package BPSO

/** Clase para modelar la condicion de terminacion de la heurística
  * 
  */
class CTerminacion(val maxIter: Int) {
  var continua = true //por omisión puede continuar
  var iter = 1

  def progress() {
    iter += 1
    if(iter >= maxIter)
      continua = false
  }
}
