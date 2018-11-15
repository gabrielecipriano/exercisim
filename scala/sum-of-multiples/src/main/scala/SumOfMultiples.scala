object SumOfMultiples {

  def sum(factors: Set[Int], limit: Int): Int = {
    def loop(factors: Set[Int], range: List[Int], acc: Int): Int = range match {
      case Nil => acc
      case x :: xs if factors.exists(x % _ == 0) => loop(factors, xs, acc + x)
      case _ :: xs => loop(factors, xs, acc)
    }
    loop(factors, (1 until limit).toList, 0)
  }
}

