
object Leap {

  def leapYear(value: Int): Boolean =
    isYearOf(value) {
      divisibleBy(4) and
        not(divisibleBy(100) and not(divisibleBy(400)))
    }

  implicit class Condition(condition: Int => Boolean) {
    def and(other: Int => Boolean): Int => Boolean = i => {condition(i) && other(i)}

    def or(other: Int => Boolean): Int => Boolean = int => { condition(int) || other(int)}
  }

  def not(condition: Int => Boolean): Int => Boolean = !condition(_)

  def divisibleBy(divisor: Int): Int => Boolean = dividend => {
    dividend % divisor == 0
  }

  private def isYearOf(year: Int)(condition: Int => Boolean) = {
    condition(year)
  }
}
