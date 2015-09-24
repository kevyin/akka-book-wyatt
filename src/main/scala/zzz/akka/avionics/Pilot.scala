package zzz.akka.avionics

/**
 * Kevin Ying 2015
 */

import akka.actor.{Actor, ActorRef}

trait PilotProvider {
  def pilot: Actor = new Pilot
  def copilot: Actor = new CoPilot
  def autopilot: Actor = new AutoPilot
}

object Pilots {

  case object ReadyToGo

  case object RelinquishControl

}

class Pilot extends Actor {

  import Pilots._

  var controls: ActorRef = context.system.deadLetters
  var copilot: ActorRef = context.system.deadLetters
  var autopilot: ActorRef = context.system.deadLetters
  val copilotName = context.system.settings.config.getString(
    "zzz.akka.avionics.flightcrew.copilotName")

  def receive = {
    case ReadyToGo =>
      context.parent ! Plane.GiveMeControl
      copilot = context.actorFor("../" + copilotName)
      autopilot = context.actorFor("../AutoPilot")

    case Controls(controlSurfaces) =>
      controls = controlSurfaces
  }
}

class CoPilot extends Actor {
  import Pilots._
  var controls: ActorRef = context.system.deadLetters
  var pilot: ActorRef = context.system.deadLetters
  var autopilot: ActorRef = context.system.deadLetters
  val pilotName = context.system.settings.config.getString(
    "zzz.akka.avionics.flightcrew.pilotName")
  def receive = {
    case ReadyToGo =>
      pilot = context.actorFor("../" + pilotName)
      autopilot = context.actorFor("../AutoPilot")
  }
}