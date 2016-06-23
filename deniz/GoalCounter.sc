object GameState extends Enumeration {
  type state = Value
  val PreMatch, Running, Ended = Value
}

def count(gameEvents:List[String]):Option[Tuple2[Int,Int]] = {

  def countGoals(events:List[String], currentState:GameState.state, home:Int, away:Int):Option[Tuple2[Int,Int]] = events match {
    case Nil =>  
      if (currentState != GameState.Ended) None
      else Some(Tuple2(home, away))

    case x :: xs => x match {

      case "Goalhome" =>
        if (currentState != GameState.Running) None
        else countGoals(xs, GameState.Running, home+1, away)

      case "GoalAway" =>
        if (currentState != GameState.Running) None
        else countGoals(xs, GameState.Running, home, away+1)

      case "Kickoff" =>
        if (currentState != GameState.PreMatch) None
        else countGoals(xs, GameState.Running, home, away)

      case "Final Whistle" =>
        if (currentState != GameState.Running) None
        else countGoals(xs, GameState.Ended, home, away)

      case _ => None
    }
  }
  
  countGoals(gameEvents, GameState.PreMatch, 0, 0)
}


def RunGame(gameEvents:List[String]): Unit = {
  
    count(gameEvents) match {
      case Some(Tuple2(home, away)) => println(home + " - " + away)
      case None => println("Invalid")
    }
}

val validList                 = List("Kickoff", "Goalhome", "GoalAway", "Goalhome", "Goalhome", "Final Whistle")
val validListNoGoal           = List("Kickoff", "Final Whistle")
val validListNonNilDraw       = List("Kickoff", "Goalhome", "GoalAway", "Final Whistle")
val noKickoffElement          = List("Goalhome", "Goalaway", "Final Whistle")
val typos                     = List("KICKOFF", "GOALAWAY", "GOALHOME", "FINAL WHISTLE")
val noFinalWhistle            = List("Kickoff", "Goalhome", "Goalhome")
val goalAfterWhistle          = List("Kickoff", "Goalhome", "Final Whistle", "Goalhome")
val goalAfterSecondKickOff    = List("Kickoff", "Goalhome", "Kickoff", "Goalhome", "Final Whistle")
val emptyList                 = List()

RunGame(validList)
RunGame(validListNoGoal)
RunGame(validListNonNilDraw)
RunGame(noKickoffElement)
RunGame(typos)
RunGame(noFinalWhistle)
RunGame(goalAfterSecondKickOff)
RunGame(emptyList)