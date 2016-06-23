object GameState extends Enumeration {
  type state = Value
  val PreMatch, Running, Ended = Value
}

def count(gameEvents:List[String]):Tuple2[Int,Int] = {

  def countGoals(events:List[String], currentState:GameState.state):Tuple2[Int,Int] = events match {
    case Nil =>
      if (currentState != GameState.Ended) throw new Error("Stop the game, please!")
      else (0, 0)

    case x :: xs => x match {

      case "Goalhome" =>
        if (currentState != GameState.Running) throw new Error("No Kickoff")
        else {
          val t = countGoals(xs, GameState.Running)
          (t._1 + 1, t._2)
        }

      case "GoalAway" =>
        if (currentState != GameState.Running) throw new Error("No Kickoff")
        else {
          val t = countGoals(xs, GameState.Running)
          (t._1, t._2+1)
        }

      case "Kickoff" =>
        if (currentState != GameState.PreMatch) throw new Error("How you can start that game")
        else {
          countGoals(xs, GameState.Running)
        }

      case "Final Whistle" =>
        if (currentState != GameState.Running) throw new Error("You cannot end a non-running game")
        else {
          countGoals(xs, GameState.Ended)
        }
    }
  }
  
  countGoals(gameEvents, GameState.PreMatch)
}


def RunGame(gameEvents:List[String]): Unit = {
  try {
    count(gameEvents) match {
      case (home, away) => println(home + " - " + away)
    }
  }
  catch {
    case err:Error => println("Invalid : Reason " + err.getMessage)
    case x:Throwable => println("Invalid : Reason " + x.getMessage)
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

println(RunGame(validList))
println(RunGame(validListNoGoal))
println(RunGame(validListNonNilDraw))
println(RunGame(noKickoffElement))
println(RunGame(typos))
println(RunGame(noFinalWhistle))
println(RunGame(goalAfterSecondKickOff))