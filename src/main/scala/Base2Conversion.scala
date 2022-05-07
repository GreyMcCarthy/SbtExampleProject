import scala.annotation.tailrec

object Base2Conversion {

  def numDigits(n: Int, base: Int): Int = {
    @tailrec
    def numDigitsLoop(n: Double, base: Int, digitsSoFar: Int): Int = {
      if (n / base >= 1) numDigitsLoop(n / base, base, digitsSoFar + 1)
      else digitsSoFar
    }

    numDigitsLoop(n.toDouble, base, 1)
  }

  def numDigits2(n: Int, base: Int): Int = {
    val guess = (math.log(n)/math.log(base) + 1).toInt
    if (n >= math.pow(base, guess)) {
      guess + 1
    } else guess
  }

  def baseConversion(n: Int, base: Int): String = {
    val a = numDigits(n ,base)

    val (_, qs) = (0.to(a-1)).reverse.foldLeft((n, "")) { case ((m, qsSoFar), index) =>
      val b = math.pow(base, index).toInt
      val q = m / b
      val r = n % b
      (r, qsSoFar ++ q.toString)
    }

    qs
  }
}
