package io.rob.algebra

sealed trait Side
case object Home extends Side
case object Away extends Side

sealed trait Incident
case class Goal(side: Side) extends Incident
case object KickOff extends Incident
case object FinalWhistle extends Incident
