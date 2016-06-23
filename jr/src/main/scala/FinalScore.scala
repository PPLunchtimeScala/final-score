object FinalScore {

/*
   Assumptions: Valid list will always start with "KICKOFF" and terminate with "FINAL WHISTLE"
                Only other valid items in list are "GOALAWAY", "GOALHOME"
                Case insensitive
 */

  class MatchScore(val homeScore: Int, val awayScore: Int) {
    override def toString(): String = "(Home Score: " + homeScore + ", Away Score:" + awayScore + ")";
  }

  def processMatchEvents(matchEvents: List[String]): MatchScore = {
    if (isMatchEventListValid(matchEvents.map(_.toUpperCase()))) {
      calculateScore(matchEvents.map(_.toUpperCase()))
    } else {
      throw sys.error("Unsupported match event detected")
    }
  }

  def isMatchEventListValid(matchEvents: List[String]): Boolean = {
    if (matchEvents.head == "KICKOFF" && matchEvents.last == "FINAL WHISTLE") {
      matchEvents.
        withFilter(_ != "KICKOFF").
        withFilter(_ != "FINAL WHISTLE").
        withFilter(_ != "GOALAWAY").
        withFilter(_ != "GOALHOME") map identity isEmpty
    } else {
      false
    }
  }


  def calculateScore(matchEvent: List[String]): MatchScore = {
    def process(matchEvent: List[String], home: Int, away: Int): MatchScore = matchEvent match {
      case "KICKOFF" :: tail => process(tail, home, away)
      case "GOALHOME" :: tail => process(tail, home + 1, away)
      case "GOALAWAY" :: tail => process(tail, home, away + 1)
      case "FINAL WHISTLE" :: Nil => new MatchScore(home, away)
    }
    process(matchEvent, 0, 0)
  }
}
