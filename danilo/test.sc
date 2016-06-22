import scala.annotation.tailrec

val validList = List("Kickoff", "Goalhome", "GoalAway", "Goalhome", "Goalhome", "Final Whistle")
val noKickoffElement = List("Goalhome", "Goalaway", "Final Whistle")
val typos = List("KICKOFF", "GOALAWAY", "GOALHOME", "FINAL WHISTLE")
val noFinalWhistle = List("Kickoff", "Goalhome", "Goalhome")

def showScore(list: List[String]) : String= {
  if (list.isEmpty || list.size < 3 ||
    list.head != "Kickoff" || list.last != "Final Whistle" ||
    !checkValidValues(list.drop(1).dropRight(1)))
      "Invalid"
  else{
    def result = returnScore(list)
    s"${result.home}-${result.away}"
  }
}

def checkValidValues(list: List[String]):Boolean ={
  if (list.isEmpty) return true
  else if (list.head == "Goalhome" || list.head == "GoalAway")
    return checkValidValues(list.drop(1))
  else
    return false;
}

def returnScore(list: List[String]): ResultScore = {
  @tailrec
  def sumAccumulator(list: List[String], accum: ResultScore): ResultScore = {
    if (list.isEmpty) return accum
    else if (list.head == "Goalhome") return sumAccumulator(list.drop(1), new ResultScore(accum.home + 1, accum.away))
    else if (list.head == "GoalAway") return sumAccumulator(list.drop(1), new ResultScore(accum.home, accum.away + 1))
    else return sumAccumulator(list.drop(1),accum)
  }
  return sumAccumulator(list, new ResultScore(0,0))
}

case class ResultScore(var home: Int, var away: Int)

println(showScore(validList))
println(showScore(noKickoffElement))
println(showScore(typos))
println(showScore(noFinalWhistle))
