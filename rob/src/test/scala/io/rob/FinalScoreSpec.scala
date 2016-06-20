package io.rob

import io.rob.algebra._
import org.scalatest.{Matchers, WordSpec}

/**
  * Created by rob on 19/06/16.
  */
class FinalScoreSpec extends WordSpec with Matchers {

  "My FinalScore app" should {
    "calculate the score of a valid list of incidents" in {
      val incidents = List("KickOff", "GoalHome", "GoalAway", "GoalHome", "FinalWhistle")
      FinalScore.getScore(incidents) shouldBe "2-1"
    }

    "reject a list that does not begin with KickOff" in {
      val incidents = List("GoalHome", "GoalAway", "GoalHome", "FinalWhistle")
      FinalScore.getScore(incidents) shouldBe "First element should be KickOff"
    }

    "reject a list that does not end with FinalWhistle" in {
      val incidents = List("KickOff", "GoalHome", "GoalAway", "GoalHome")
      FinalScore.getScore(incidents) shouldBe "Last element should be Final Whistle"
    }

    "reject a list that has KickOff in the middle of the list" in {
      val incidents = List("KickOff", "GoalHome", "KickOff", "GoalHome")
      FinalScore.getScore(incidents) shouldBe "Expected Goal instead of KickOff"
    }

    "reject a list that has an unknown incident in the list" in {
      val incidents = List("KickOff", "GoalHome", "Foul", "GoalHome")
      FinalScore.getScore(incidents) shouldBe "Failed to match Foul to a known incident type"
    }
  }
}

