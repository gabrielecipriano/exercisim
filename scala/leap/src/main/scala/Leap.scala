
object Leap {

  def leapYear(value: Int): Boolean =
    isYearOf(value) {
      divisibleBy(4) and
        not(divisibleBy(100) and not(divisibleBy(400)))
    }

  private def isYearOf(year: Int)(condition: Int => Boolean) = {
    condition(year)
  }

  private def divisibleBy(divisor: Int): Int => Boolean = _ % divisor == 0

  private implicit class Condition(condition: Int => Boolean) {
    def and(other: Int => Boolean): Int => Boolean = i => {condition(i) && other(i)}
  }

  private def not(condition: Int => Boolean): Int => Boolean = !condition(_)
}

