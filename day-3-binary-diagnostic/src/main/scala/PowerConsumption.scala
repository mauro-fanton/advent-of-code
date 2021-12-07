

class PowerConsumption(val gamma: Int, val epsilon: Int) {

  def calculate(): Int = {
    gamma * epsilon
  }
}

object PowerConsumption {
  def apply(gamma: Int, epsilon: Int): PowerConsumption = {
    new PowerConsumption(gamma, epsilon)
  }

}
