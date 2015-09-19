package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
      Signal(scala.math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def inner(): Set[Double] = {
      val currentDelta = delta()
      if (currentDelta < 0) Set()
      else if (currentDelta == 0) Set((-b()) / (2 * a()))
      else Set((-b() - scala.math.sqrt(currentDelta)) / (2 * a()), (-b() + scala.math.sqrt(currentDelta)) / (2 * a()))
    }

    Signal(inner())
  }
}
