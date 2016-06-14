#PP Lunchtime Scala - Final Score

##Puzzle
Print out the final score of a game

##Description
Model the following - A collection of Football Incidents where each incident is either a "KickOff", "Goal" or "Final Whistle".
The collection will start with Kickoff and end with Final Whistle.
Goal may be either a "home goal" or an "away goal".

Your task is to sum the goals and print out a final score.

##Suggestions
You may model this any way you wish.
E.g.  A list of Strings, Am array of Integers, a stream of Case Classes.

The list may be created from the command line, a file or hard-coded.

The main logic may optionally validate if the list is correctly structured (i.e. begins with Kick off and ends with Final Whistle).

You may optionally include tests.

##Example
```scala
val list = List("Kickoff", "Goalhome", "GoalAway", "Final Whistle")
validate(list)
val output = sumGoals(list)
println (output)
```
