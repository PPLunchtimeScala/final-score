package main.scala.score

object FinalScore {

  def getScore(input: List[String]): String = {
    if(input.isEmpty || input.head != "Kickoff" || input.last != "Final Whistle"){
      return "Invalid"
    }

    var goalHome, goalAway = 0
    for (incident <- input.slice(1, input.length - 1)) {
      if(incident == "Goalhome"){
        goalHome += 1
      } else if(incident == "GoalAway"){
        goalAway += 1
      } else {
        return "Invalid"
      }
    }
    return goalHome + "-" + goalAway
  }
}


