val validList = List("Kickoff", "GoalHome", "GoalAway", "GoalHome", "GoalHome", "FinalWhistle")
val noKickoffElement = List("GoalHome", "GoalAway", "Final Whistle")
val typos = List("KICKOFF", "GOALAWAY", "GOALHOME", "FINAL WHISTLE")
val noFinalWhistle = List("Kickoff", "GoalHome", "GoalHome")


sealed trait Event

case object Kickoff extends Event

case object GoalHome extends Event

case object GoalAway extends Event

case object FinalWhistle extends Event

case class Game(scoreHome: Int, scoreAway: Int, ended: Boolean)


object LunchtimeProblem {
  def getScore(listOfEvents: List[String]): String = {
    def eventMapper(x: String): Option[Event] = {
      x match {
        case "Kickoff" => Some(Kickoff)
        case "GoalHome" => Some(GoalHome)
        case "GoalAway" => Some(GoalAway)
        case "FinalWhistle" => Some(FinalWhistle)
        case _ => None
      }
    }

    def gameMapper(events: List[Option[Event]]): Option[Game] = {
      def map(currentGameState: Option[Game])(events: List[Option[Event]]): Option[Game] = {
        currentGameState match {
          case None => events.headOption match {
            case Seq() => None
            case None => None
            case Some(Some(Kickoff)) => map(Some(Game(0, 0, false)))(events.tail)
            case _ => map(None)(events.tail)
          }
          case Some(Game(scoreHome, scoreAway, ended)) => events.headOption match {
            case Seq() => Some(Game(scoreHome, scoreAway, ended))
            case None => None
            case Some(Some(Kickoff)) => None
            case Some(Some(GoalHome)) => map(Some(Game(scoreHome + 1, scoreAway, false)))(events.tail)
            case Some(Some(GoalAway)) => map(Some(Game(scoreHome, scoreAway + 1, false)))(events.tail)
            case Some(Some(FinalWhistle)) => Some(Game(scoreHome, scoreAway, true))
          }
        }
      }
      map(None)(events)
    }
    gameMapper(listOfEvents.map((x) => eventMapper(x))) match {
      case None => "Invalid"
      case Some(Game(_, _, false)) => "Invalid"
      case Some(Game(scoreHome, scoreAway, _)) => scoreHome + "-" + scoreAway
    }
  }
}
LunchtimeProblem.getScore(validList)
LunchtimeProblem.getScore(noKickoffElement)
LunchtimeProblem.getScore(typos)
LunchtimeProblem.getScore(noFinalWhistle)
LunchtimeProblem.getScore(List())