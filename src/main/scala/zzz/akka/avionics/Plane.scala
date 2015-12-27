package zzz.akka.avionics

import akka.actor.{Props, Actor, ActorLogging}
import zzz.akka.avionics.Pilots.{Controls, CoPilotReference}

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

  case object LostControl

  case object RequestCoPilot
}

// We want the Plane to own the Altimeter and we're going to
// do that by passing in a specific factory we can use to
// build the Altimeter

class Plane extends Actor with ActorLogging {
  this: AltimeterProvider
        with HeadingIndicatorProvider
        with PilotProvider
        with LeadFlightAttendantProvider =>

  import Altimeter._
  import Plane._
  import EventSource._

  val cfgstr = "zzz.akka.avionics.flightcrew"
//  override val newAltimeter = context.actorOf(
//    Props(Altimeter()), "Altimeter")
//  val controls = context.actorOf(Props(new ControlSurfaces(newAltimeter), "ControlSurfaces")
  val config = context.system.settings.config
  val pilotName = config.getString(s"$cfgstr.pilotName")
  val copilotName = config.getString(s"$cfgstr.copilotName")
  val attendantName = config.getString(s"$cfgstr.leadAttendantName")
//  override val newPilot = context.actorOf(Props[Pilot],
//    config.getString(s"$cfgstr.pilotName"))
//  override val newCoPilot = context.actorOf(Props[CoPilot],
//    config.getString(s"$cfgstr.copilotName"))
//  override val newAutopilot = context.actorOf(
//    Props[AutoPilot], "AutoPilot")
  val flightAttendant = context.actorOf(
    Props(LeadFlightAttendant()),
    config.getString(s"$cfgstr.leadAttendantName"))

  // Helps us look up Actors within the "Pilots" Supervisor
  def actorForPilots(name: String) =
    context.actorFor("Pilots/" + name)
  override def preStart() {
    import EventSource.RegisterListener
    import Pilots.ReadyToGo
    // Get our children going. Order is important here.
    startEquipment()
    startPeople()
    // Bootstrap the system
    actorForControls("Altimeter") ! RegisterListener(self)
    actorForPilots(pilotName) ! ReadyToGo
    actorForPilots(copilotName) ! ReadyToGo
    actorForControls("AutoPilot") ! ReadyToGo
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
            Props(newAltimeter), "Altimeter")
          val head = context.actorOf(
            Props(newHeadingIndicator), "HeadingIndicator")
          // These children get implicitly added to the
          // hierarchy
          context.actorOf(Props(newAutoPilot(self, context.parent)), "AutoPilot")
          context.actorOf(Props(new ControlSurfaces(alt, head)),
            "ControlSurfaces")
        }
      }), "Equipment")
    Await.result(controls ? WaitForStart, Duration(1, TimeUnit.SECONDS))
  }

  // Helps us look up Actors within the "Equipment" Supervisor
  def actorForControls(name: String) =
    context.actorFor("Equipment/" + name)

  def startPeople() {
    val plane = self
    // Note how we depend on the Actor structure beneath
    // us here by using actorFor(). This should be
    // resilient to change, since we'll probably be the
    // ones making the changes
    val controls = actorForControls("ControlSurfaces")
    val autopilot = actorForControls("AutoPilot")
    val people = context.actorOf(
      Props(new IsolatedStopSupervisor
        with OneForOneStrategyFactory {
        def childStarter() {
          // These children get implicitly added
          // to the hierarchy
          context.actorOf(
            Props(newCoPilot(plane, autopilot, controls)),
            copilotName)
          context.actorOf(
            Props(newPilot(plane, autopilot,
              controls)),
            pilotName)
        }
      }), "Pilots")
    // Use the default strategy here, which
    // restarts indefinitely
    context.actorOf(Props(newLeadFlightAttendant), attendantName)
    Await.result(people ? WaitForStart, Duration(1, TimeUnit.SECONDS))
  }


//    val leadAttendantName = context.system.settings.config.getString(
//      "zzz.akka.avionics.flightcrew.leadAttendantName")


  def receive = {
    case AltitudeUpdate(altitude) =>
      log info(s"Altitude is now: $altitude")
    case GiveMeControl =>
      val controls = actorForControls("ControlSurfaces")
      log info ("Plane giving control.")
      sender ! Controls(controls)
    case RequestCoPilot =>
      val coPilot = actorForPilots(copilotName)
      sender ! CoPilotReference(coPilot)
  }
}

