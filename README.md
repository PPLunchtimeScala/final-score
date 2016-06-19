#PP Lunchtime Scala - Final Score

##Puzzle
Print out the final score of a game

##Description
Model the following - A collection of Football Incidents where each incident is either a "KickOff", "Goal" or "Final Whistle".
The collection will start with Kickoff and end with Final Whistle.
Goal may be either a "home goal" or an "away goal".

Your task is to sum the goals and print out a final score.

You may optionally include tests.

##Input
The input is a ```List``` of ```String```s where each String is one of the following:
* Kickoff
* GoalHome
* GoalAway
* FinalWhistle

Your program should reject errors in your lists.

```scala
val valid-list         = List("Kickoff", "Goalhome", "GoalAway", "Goalhome", Goalhome", "Final Whistle")

val no-kickoff-element = List("Goalhome", "Goalaway", "Final Whistle")

val typos              = List("KICKOFF", "GOALAWAY", "GOALHOME", "FINAL WHISTLE")

val no-final-whistle   = List("Kickoff", "Goalhome", "Goalhome")
```

Feel free to hardcode your input lists in your solution.

##Output
Given the above examples, the output should be:
```
3-1

Invalid

Invalid

Invalid
```
