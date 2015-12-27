package zzz.akka.avionics

import akka.actor.{ActorSystem, Actor, ActorRef, Props, PoisonPill}
import akka.testkit.{TestKit, ImplicitSender, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.{WordSpecLike, WordSpec}
import org.scalatest.matchers.MustMatchers
import zzz.akka.avionics.Pilots.{ReadyToGo, CoPilotReference}

class FakeCoPilot extends Actor {
  override def receive = {
    case _ =>
  }
}

object CoPilotsSpec {
  val autopilotName = "Marvin"
  val copilotName = "Mary"
  val pilotName = "Mark"
  val configStr =
    s"""
    zzz.akka.avionics.flightcrew.autopilotName = "$autopilotName"
    zzz.akka.avionics.flightcrew.copilotName = "$copilotName"
    zzz.akka.avionics.flightcrew.pilotName = "$pilotName""""
}

class AutoPilotsSpec extends TestKit(ActorSystem("AutoPilotsSpec",
  ConfigFactory.parseString(CoPilotsSpec.configStr)))
with ImplicitSender
with WordSpecLike
with MustMatchers {

  import CoPilotsSpec._
  import Plane._

  // These paths are going to prove useful
  val autopilotPath = s"/user/$autopilotName"

  def autopilotsReadyToGo(): ActorRef = {
    val a = system.actorOf(
      Props(new AutoPilot(testActor)), autopilotName)
    a
  }

  // The Test code
  "AutoPilot" should {
    "take control when the CoPilot dies" in {
      val fakeCoPilot = system.actorOf(Props(new FakeCoPilot()), copilotName)
      val autoPilot = autopilotsReadyToGo()
      autoPilot ! ReadyToGo
      expectMsg(RequestCoPilot)
      autoPilot ! CoPilotReference(fakeCoPilot)
      // Kill the CoPilot
      system.stop(fakeCoPilot)

      expectMsg(GiveMeControl)
      lastSender must be (system.actorFor(autopilotPath))
    }
  }
}
