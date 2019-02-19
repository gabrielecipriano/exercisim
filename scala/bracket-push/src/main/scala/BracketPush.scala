import scala.collection.breakOut

object BracketPush {

  val openings: List[Char] = List('(', '[', '{')
  val closing: List[Char] = List(')', ']', '}')
  val bracketRelation: Map[Char, Char] = (openings zip closing)(breakOut)

  def isPaired(brackets: String): Boolean = {
    def loop(brackets: String, expectedClosings: List[Char]): Boolean = {
      if(finishedProcessing(brackets)) areSatisfied(expectedClosings) else {
        brackets.charAt(0) match {
          case x if isAnOpeningBracket(x) => loop(brackets.substring(1), bracketRelation(x) :: expectedClosings)
          case x if isAClosingBracket(x) => expectedClosings.headOption.contains(x) && loop(brackets.substring(1), expectedClosings.drop(1))
          case _ => loop(brackets.substring(1), expectedClosings)
        }
      }
    }
    loop(brackets, List.empty)
  }


  private def finishedProcessing(brackets: String): Boolean = {
    brackets.isEmpty
  }

  private def areSatisfied(expectedClosings: List[Char]) = {
    expectedClosings.isEmpty
  }

  private def isAClosingBracket(x: Char) = {
    closing.contains(x)
  }

  private def isAnOpeningBracket(x: Char) = {
    openings.contains(x)
  }
}
