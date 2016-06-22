package score

import org.scalatest.{Matchers, WordSpec}

class FinalScoreSpec extends WordSpec with Matchers {

   "My FinalScore app" should {
     "calculate the score of a valid list of incidents" in {
       val incidents = List("Kickoff", "Goalhome", "GoalAway", "Goalhome", "Final Whistle")
       FinalScore.getScore(incidents) shouldBe "2-1"
     }

     "reject a list that does not begin with KickOff" in {
       val incidents = List("GoalHome", "GoalAway", "GoalHome", "FinalWhistle")
       FinalScore.getScore(incidents) shouldBe "Invalid"
     }

     "reject a list that does not end with FinalWhistle" in {
       val incidents = List("KickOff", "GoalHome", "GoalAway", "GoalHome")
       FinalScore.getScore(incidents) shouldBe "Invalid"
     }

     "reject a list that has KickOff in the middle of the list" in {
       val incidents = List("KickOff", "GoalHome", "KickOff", "GoalHome")
       FinalScore.getScore(incidents) shouldBe "Invalid"
     }

     "reject a list that has an unknown incident in the list" in {
       val incidents = List("KickOff", "GoalHome", "Foul", "GoalHome")
       FinalScore.getScore(incidents) shouldBe "Invalid"dddddd
     }
   }
 }

