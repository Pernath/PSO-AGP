package BPSO
import AGP.Poligono

/** Clase para modelar la función de costo a utilizar en la evaluacion de las partículas
  * 
  */
class FuncionDeCosto(var poligono: Poligono) {


  /** Método para evaluar una solución 
    * @param pos el arreglo que dice en qué vertices están posicionados los guardias
    */
  def eval(pos: Array[Boolean]): Double = {
    return 0.0
  }
}
