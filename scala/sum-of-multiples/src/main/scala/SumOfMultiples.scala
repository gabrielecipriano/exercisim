object SumOfMultiples {


  def sumAcc(factors: Set[Int], range: List[Int], acc: Int): Int = range match {
    case Nil => acc
    case x :: xs if factors.exists(x % _ == 0) => sumAcc(factors, xs, acc + x)
    case _ :: xs => sumAcc(factors, xs, acc)
  }

  def sum(factors: Set[Int], limit: Int): Int = {
    sumAcc(factors, (1 to limit).toList, 0)
  }
}

