import com.paddypower.soccer.hub.app.config.Solver.InRunning

import scala.annotation.tailrec

object FinalScore extends App {

  val validList         = List("Kickoff", "Goalhome", "Goalaway", "Goalhome", "Goalhome", "Final Whistle")
  val noKickoffElement  = List("Goalhome", "Goalaway", "Final Whistle")
  val typos             = List("KICKOFF", "GOALAWAY", "GOALHOME", "FINAL WHISTLE")
  val noFinalWhistle    = List("Kickoff", "Goalhome", "Goalhome")

  val eventAfterEnd    = List("Kickoff",  "Goalaway", "Final Whistle", "Goalhome")
  val eventBeforeStart = List("Goalaway", "Kickoff",  "Goalaway", "Final Whistle")

  println(Solver.solve(validList))
  println(Solver.solve(noKickoffElement))
  println(Solver.solve(typos))
  println(Solver.solve(noFinalWhistle))
  println(Solver.solve(eventAfterEnd))
  println(Solver.solve(eventBeforeStart))
}

object Solver {
  sealed trait GameState
  case object Prematch extends GameState
  case class InRunning(home: Int, away: Int) extends GameState
  case object Invalid extends GameState

  object Events {
    val KickOff = "Kickoff"
    val GoalHome = "Goalhome"
    val GoalAway = "Goalaway"
    val Finished = "Final Whistle"
  }

  def solve(events: List[String]): GameState = {
    import Events._
    @tailrec
    def go(s: GameState, l: List[String]): GameState = {
      (s, l) match {
        case (res: InRunning, Finished :: Nil) => res
        case (Prematch, KickOff::tail) => go(InRunning(0, 0), tail)
        case (InRunning(home, away), GoalHome::tail) => go(InRunning(home + 1, away), tail)
        case (InRunning(home, away), GoalAway::tail) => go(InRunning(home, away + 1), tail)
        case _ => Invalid
      }
    }
    go(Prematch, events)
  }
}