package io.rob

import io.rob.algebra._

import scala.util.{Failure, Success, Try}

case class Score(home: Int, away: Int)

object FinalScore extends App {

  def getScore(incidents: List[Incident], score: Score = Score(0, 0)): Try[Score] = {
    incidents match {
      case Nil                   => Failure(new IllegalArgumentException("Last element should be Final Whistle"))
      case Goal(Home)   :: t     => getScore(t, Score(score.home + 1, score.away))
      case Goal(Away)   :: t     => getScore(t, Score(score.home, score.away + 1))
      case FinalWhistle :: Nil   => Success(score)
      case unexpected   :: _     => Failure(new IllegalArgumentException(s"Expected Goal instead of $unexpected"))
    }
  }

  val incidents = List(KickOff, Goal(Home), Goal(Away), Goal(Home), FinalWhistle)

  val score = incidents match {
    case h :: t if h == KickOff => getScore(t)
    case _                      => Failure(new IllegalArgumentException("First element should be KickOff"))
  }

  score match {
    case Success(s)  => println (s"Final score is $s")
    case Failure(th) => println (th.getMessage)
  }

}

