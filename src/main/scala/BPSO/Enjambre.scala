package BPSO

/** Clase para modelar un enjambre o población de partículas para BPSO
  * 
  */
class Enjambre(val tamano: Int) {
  var particulas = new Array[Particula](tamano)
  var mejor: Particula = null
}
