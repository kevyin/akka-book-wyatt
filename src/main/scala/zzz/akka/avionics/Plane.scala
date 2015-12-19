package zzz.akka.avionics

import akka.actor.{Props, Actor, ActorLogging}
//import zzz.akka.avionics.LeadFlightAttendant
import scala.concurrent.Await
import zzz.akka.avionics.IsolatedLifeCycleSupervisor.WaitForStart
import akka.util.Timeout
import akka.pattern.ask
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

object Plane {

  // Returns the control surface to the Actor that
  // asks for them
  case object GiveMeControl

}

// We want the Plane to own the Altimeter and we're going to
// do that by passing in a specific factory we can use to
// build the Altimeter

class Plane extends Actor with ActorLogging {
  this: AltimeterProvider
        with PilotProvider
        with LeadFlightAttendantProvider =>

  import Altimeter._
  import Plane._
  import EventSource._

  val cfgstr = "zzz.akka.avionics.flightcrew"
  override val newAltimeter = context.actorOf(
    Props(Altimeter()), "Altimeter")
  val controls = context.actorOf(Props(new ControlSurfaces(newAltimeter)), "ControlSurfaces")
  val config = context.system.settings.config
  override val newPilot = context.actorOf(Props[Pilot],
    config.getString(s"$cfgstr.pilotName"))
  override val newCoPilot = context.actorOf(Props[CoPilot],
    config.getString(s"$cfgstr.copilotName"))
  override val newAutoPilot = context.actorOf(
    Props[AutoPilot], "AutoPilot")
  val flightAttendant = context.actorOf(
    Props(LeadFlightAttendant()),
    config.getString(s"$cfgstr.leadAttendantName"))

  override def preStart() {
    // Register ourself with the Altimeter to receive updates
    // on our altitude
    newAltimeter ! EventSource.RegisterListener(self)
    List(newPilot, newCoPilot) foreach { _ ! Pilots.ReadyToGo }
  }

  // There's going to be a couple of asks below and
  // a timeout is necessary for that.
  implicit val askTimeout = Timeout(1, TimeUnit.SECONDS)
  def startEquipment() {
    val controls = context.actorOf(
      Props(new IsolatedResumeSupervisor
        with OneForOneStrategyFactory {
        def childStarter() {
          val alt = context.actorOf(
            Props(new Altimeter with ProductionEventSource), "Altimeter")
          // These children get implicitly added to the
          // hierarchy
          context.actorOf(Props(new AutoPilot), "AutoPilot")
          context.actorOf(Props(new ControlSurfaces(alt)),
            "ControlSurfaces")
        }
      }), "Equipment")
    Await.result(controls ? WaitForStart, Duration(1, TimeUnit.SECONDS))
  }

  def startPeople() {
    val plane = self


    val people = context.actorOf(
      Props(new IsolatedStopSupervisor
        with OneForOneStrategyFactory {
        def childStarter() {
          // These children get implicitly added to
          // the hierarchy
          context.actorOf(Props(new Pilot), pilotName)
          context.actorOf(Props(new CoPilot), copilotName)
        }
      }), "Pilots")
    // Use the default strategy here, which
    // restarts indefinitely
    context.actorOf(Props(newFlightAttendant), attendantName)
    Await.result(people ? WaitForStart, 1.second)
  }

  def receive = {
    case AltitudeUpdate(altitude) =>
      log info(s"Altitude is now: $altitude")
    case GiveMeControl =>
      log info ("Plane giving control.")
      sender ! controls
  }
}

