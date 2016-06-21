
import Events._


object Game extends App {
  val validList         = List("Kickoff", "Goalhome", "GoalAway", "Goalhome", "Goalhome", "Final Whistle")
  val noKickoffElement  = List("Goalhome", "Goalaway", "Final Whistle")
  val typos             = List("KICKOFF", "GOALAWAY", "GOALHOME", "FINAL WHISTLE")
  val noFinalWhistle    = List("Kickoff", "Goalhome", "Goalhome")

  val eventAfterEnd    = List("Kickoff",  "Goalaway", "Final Whistle", "Goalhome")
  val eventBeforeStart = List("Goalaway", "Kickoff",  "Goalaway", "Final Whistle")

  println("FinalScore")
  printForFinalScore(validList)
  printForFinalScore(noKickoffElement)
  printForFinalScore(typos)
  printForFinalScore(noFinalWhistle)
  printForFinalScore(eventAfterEnd)
  printForFinalScore(eventBeforeStart)

  println("FinalScore2")
  printForFinalScore2(validList)
  printForFinalScore2(noKickoffElement)
  printForFinalScore2(typos)
  printForFinalScore2(noFinalWhistle)
  printForFinalScore2(eventAfterEnd)
  printForFinalScore2(eventBeforeStart)

  println("FinalScore3")
  printForFinalScore3(validList)
  printForFinalScore3(noKickoffElement)
  printForFinalScore3(typos)
  printForFinalScore3(noFinalWhistle)
  printForFinalScore3(eventAfterEnd)
  printForFinalScore3(eventBeforeStart)

  def printResult(events: List[String])(f: List[String] => String) = {
    val start = System.nanoTime()
    println(s"events: ${events.mkString(",")}\t\tresult: ${f(events)}\t\ttime: ${System.nanoTime() - start}")
  }

  def printForFinalScore(events: List[String]) = printResult(events)(FinalScore.process)
  def printForFinalScore2(events: List[String]) = printResult(events)(FinalScore2.process)
  def printForFinalScore3(events: List[String]) = printResult(events)(FinalScore3.process)

}

object Events {
  val startGame = "Kickoff"
  val endGame = "Final Whistle"
  val goalHome = "Goalhome"
  val goalAway = "GoalAway"
}

sealed trait Result {
  def show: String
  def process(e: String): Result
}
case object Invalid extends Result {
  def show = "Invalid"
  def process(e: String) = this
}
case class Score(home: Int = 0, away: Int = 0) extends Result {
  def process(event: String) = event match {
    case `goalHome` => this.copy(home = home + 1)
    case `goalAway` => this.copy(away = away + 1)
    case `startGame` | `endGame` => this
    case _ => Invalid
  }

  def show = s"$home - $away"
}


object FinalScore {

  def process(events: List[String]): String = {
    validate(events) match {
      case score: Score => sum(events, score).show
      case invalid => invalid.show
    }
  }
  def sum(events: List[String], currentResult: Result): Result = (events, currentResult) match {
    case (_, Invalid) => Invalid
    case (Nil, score) => score
    case (events, score: Score) => sum(events.tail, score.process(events.head))
  }

  def validate(events: List[String]): Result = {
    if(events.head != startGame) Invalid
    else if(events.last != endGame) Invalid
    else Score()
  }

}

object FinalScore2 {

  def process(events: List[String]): String = {
    def go(e: List[String], result: Result): Result = e match {
      case Nil => result
      case x :: xs => go(xs, rule(result, x))
    }
    go(events, validate(events)).show
  }

  def validate(events: List[String]): Result = {
    if(events.head != startGame) Invalid
    else if(events.last != endGame) Invalid
    else Score()
  }

  val rule = new PartialFunction[(Result, String), Result] {
    override def isDefinedAt(state: (Result, String)): Boolean = state._2 match {
      case `startGame` | `endGame` | `goalHome` | `goalAway` => true
      case _ => false
    }

    override def apply(state: (Result, String)): Result = state match {
      case (result, event) => result.process(event)
    }
  }

}

object FinalScore3 {

  def process(events: List[String]): String = {
    events.foldLeft(validate(events))((r, e) => r.process(e)).show
  }

  def validate(events: List[String]): Result = {
    if(events.head != startGame) Invalid
    else if(events.last != endGame) Invalid
    else Score()
  }

}