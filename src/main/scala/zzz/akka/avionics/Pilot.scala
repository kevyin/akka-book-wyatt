package zzz.akka.avionics

/**
 * Kevin Ying 2015
 */

import akka.actor.{Terminated, Actor, ActorRef}
import zzz.akka.avionics.Plane.{RequestCoPilot, GiveMeControl}

trait PilotProvider {
  def newPilot(plane: ActorRef,
                 autopilot: ActorRef,
                 controls: ActorRef ): Actor = new Pilot(plane, autopilot, controls)
  def newCoPilot(plane: ActorRef,
                 autopilot: ActorRef,
                 controls: ActorRef): Actor = new CoPilot(plane, autopilot, controls)
  def newAutoPilot(plane: ActorRef,
                  controls: ActorRef): Actor = new AutoPilot(plane, controls)
}

object Pilots {

  case object ReadyToGo

  case class Controls(controlSurfaces: ActorRef)

  case object RelinquishControl

  case class CoPilotReference(coPilot: ActorRef)

}

class Pilot(plane: ActorRef,
             autopilot: ActorRef,
             var controls: ActorRef) extends Actor {

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

class CoPilot(plane: ActorRef,
             autoPilot: ActorRef,
             var controls: ActorRef) extends Actor {
  import Pilots._
  var pilot: ActorRef = context.system.deadLetters
  var autopilot: ActorRef = context.system.deadLetters
  val pilotName = context.system.settings.config.getString(

    "zzz.akka.avionics.flightcrew.pilotName")
  def receive = {
    case ReadyToGo =>
      pilot = context.actorFor("../" + pilotName)
      autopilot = context.actorFor("../AutoPilot")
      context.watch(pilot)

    case Controls(controlSurfaces) =>
      controls = controlSurfaces

    case Terminated(_) =>
      // Pilot died
      plane ! GiveMeControl

  }
}

class AutoPilot(plane: ActorRef,
               var controls: ActorRef) extends Actor {
  import Pilots._

  def receive = {
    case ReadyToGo =>
      val copilot = plane ! RequestCoPilot

    case CoPilotReference(copilot) =>
      context.watch(copilot)

    case Controls(controlSurfaces) =>
      controls = controlSurfaces

    case Terminated(_) =>
      // CoPilot died
      plane ! GiveMeControl
  }
}
