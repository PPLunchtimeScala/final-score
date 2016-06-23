package io.cheng

import scala.util.Failure

trait Incidents
trait Goals extends Incidents
case object KickOff extends Incidents
case object FinalWhistle extends Incidents
case object GoalHome extends Goals
case object GoalAway extends Goals



class MatchResults() {
	var validStarting:Boolean = false
	var validEnding:Boolean = false
	var homeScore = 0
	var awayScore = 0
	var allExpected = true
}

object FinalScore {


	def processSoccerMatch(aMatch:List[Incidents]):MatchResults = {
		val result = new MatchResults()
		aMatch.foreach( _ match {
			case GoalHome if (result.allExpected && result.validStarting && !result.validEnding) => result.homeScore += 1
			case GoalAway if (result.allExpected && result.validStarting && !result.validEnding) => result.awayScore += 1
			case KickOff if !result.validStarting => result.validStarting = true
			case FinalWhistle if (result.validStarting && !result.validEnding) => result.validEnding = true
			case _ => result.allExpected = false
		}
		)
		result
	}

	def main(Args: Array[String])= {
		val aSoccerMatch:List[Incidents] = List(KickOff, GoalHome, GoalHome, GoalAway,GoalHome,GoalAway, FinalWhistle)
		val result = processSoccerMatch(aSoccerMatch)


		if (result.validStarting && result.validEnding && result.allExpected)
			println(s"HOME:AWAY is ${result.homeScore},${result.awayScore}")
		else
			println(s"not a valid match anyway, NO. Detailed error can be specified with the booleans.")
	}

}


