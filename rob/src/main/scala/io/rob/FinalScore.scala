package io.rob

import io.rob.algebra._

import scala.util.{Failure, Success, Try}

case class Score(home: Int, away: Int)

object FinalScore {

  private def getScore(incidents: List[Incident], score: Score): Try[Score] = {
    incidents match {
      case Nil => Failure(new IllegalArgumentException("Last element should be Final Whistle"))
      case Goal(Home) :: t => getScore(t, Score(score.home + 1, score.away))
      case Goal(Away) :: t => getScore(t, Score(score.home, score.away + 1))
      case FinalWhistle :: Nil => Success(score)
      case unexpected :: _ => Failure(new IllegalArgumentException(s"Expected Goal instead of $unexpected"))
    }
  }

  private def toIncident(str: String): Try[Incident] = {
    str match {
      case "KickOff" => Success(KickOff)
      case "GoalHome" => Success(Goal(Home))
      case "GoalAway" => Success(Goal(Away))
      case "FinalWhistle" => Success(FinalWhistle)
      case unknown => Failure(new IllegalArgumentException(s"Failed to match $unknown to a known incident type"))
    }
  }

  private def sequence[A](as: List[Try[A]]): Try[List[A]] = {
    as match {
      case Nil => Success(Nil)
      case h :: t => h flatMap { hh => sequence(t) map { hh :: _ } }
    }
  }

  private def validate(incidents: List[Try[Incident]]): Try[List[Incident]] = {
    incidents match {
      case Success(KickOff) :: t => sequence(t)
      case _                     => Failure(new IllegalArgumentException("First element should be KickOff"))
    }
  }

  def getScore(input: List[String]): String = {
    val incidents = input map toIncident

    val score = validate(incidents) flatMap (i => getScore(i, Score(0, 0)))

    score match {
      case Success(s) => s"${s.home}-${s.away}"
      case Failure(th) => th.getMessage
    }
  }
}

