package zzz.akka.avionics

/**
 * Kevin Ying 2015
 */

import akka.actor.{Actor, ActorRef}

trait PilotProvider {
  def newPilot(plane: ActorRef,
                 autopilot: ActorRef,
                 controls: ActorRef,
                 altimeter: ActorRef): Actor = new Pilot(plane, autopilot, controls, altimeter)
  def newCoPilot(plane: ActorRef,
                 autopilot: ActorRef,
                 altimeter: ActorRef): Actor = new CoPilot
  def newAutopilot: Actor = new AutoPilot
}

object Pilots {

  case object ReadyToGo

  case class Controls(controlSurfaces: ActorRef)

  case object RelinquishControl

}

class Pilot(plane: ActorRef,
             autopilot: ActorRef,
             var controls: ActorRef,
             altimeter: ActorRef) extends Actor {

  import Pilots._

  var copilot: ActorRef = context.system.deadLetters
  val copilotName = context.system.settings.config.getString(
    "zzz.akka.avionics.flightcrew.copilotName")

  def receive = {
    case ReadyToGo =>
      context.parent ! Plane.GiveMeControl
      copilot = context.actorFor("../" + copilotName)
//      autopilot = context.actorFor("../AutoPilot")

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

class AutoPilot extends Actor {
  import Pilots._

  def receive = {
    case ReadyToGo =>

  }
}
