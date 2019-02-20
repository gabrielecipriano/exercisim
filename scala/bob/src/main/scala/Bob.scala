object Bob {

  def response(statement: String): String = statement match {
    case s if isAskingShouting(s) => "Calm down, I know what I'm doing!"
    case s if isAsking(s) => "Sure."
    case s if isShouting(s) => "Whoa, chill out!"
    case s if isSilent(s) => "Fine. Be that way!"
    case _ => "Whatever."
  }

  private def isAskingShouting(s: String) = isShouting(s) && isAsking(s)

  private def isAsking(statement: String): Boolean = statement.trim.endsWith("?")

  private def isShouting(statement: String): Boolean = {
    statement.toUpperCase == statement && statement.toLowerCase() != statement
  }

  private def isSilent(statement: String): Boolean = statement.replaceAll("\\s", "").isEmpty
}
