
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


  def printResult(events: List[String])(f: List[String] => String) = {
    val start = System.nanoTime()
    println(s"events: ${events.mkString(",")}\t\tresult: ${f(events)}\t\ttime: ${System.nanoTime() - start}")
  }

  def printForFinalScore(events: List[String]) = printResult(events)(FinalScore.process)

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

  def process(events: List[String]): String = sum(events).show

  def sum(l: List[String], result: Result = Score(), idx: Int = 0): Result = l match {
    case x :: Nil if x != `endGame` => Invalid
    case x :: xs if idx == 0 && x != `startGame` => Invalid
    case Nil => result
    case x :: xs => sum(xs, result.process(x), idx + 1)
  }

}
