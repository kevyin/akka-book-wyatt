package zzz.akka.avionics

import akka.actor.{ActorSystem, Actor, ActorRef, Props, PoisonPill}
import akka.pattern.ask
import akka.testkit.{TestKit, ImplicitSender, TestProbe}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.scalatest.{WordSpecLike, WordSpec}
import org.scalatest.matchers.MustMatchers
import scala.concurrent.Await
import scala.concurrent.duration._

class FakePilot extends Actor {
  override def receive = {
    case _ =>
  }
}

object PilotsSpec {
  val copilotName = "Mary"
  val pilotName = "Mark"
  val configStr =
    s"""
    zzz.akka.avionics.flightcrew.copilotName = "$copilotName"
    zzz.akka.avionics.flightcrew.pilotName = "$pilotName""""
}

class PilotsSpec extends TestKit(ActorSystem("PilotsSpec",
  ConfigFactory.parseString(PilotsSpec.configStr)))
    with ImplicitSender
    with WordSpecLike
    with MustMatchers {

  import PilotsSpec._
  import Plane._


  // We're going to care an "empty" Actor as a TestProbe
  // instance Calling it nilActor just makes it really clear
  // what we mean
  def nilActor: ActorRef = TestProbe().ref

  // These paths are going to prove useful
  val pilotPath = s"/user/TestPilots/$pilotName"
  val copilotPath = s"/user/TestPilots/$copilotName"

  // Helper function to construct the hierarchy we need
  // and ensure that the children are good to go by the
  // time we're done
  def pilotsReadyToGo(): ActorRef = {
    // The 'ask' below needs a timeout value
    implicit val askTimeout = Timeout(4.seconds)
    // Much like the creation we're using in the Plane
    val a = system.actorOf(
      Props(new IsolatedStopSupervisor
        with OneForOneStrategyFactory {
        def childStarter() {
          context.actorOf(Props[FakePilot], pilotName)
          context.actorOf(
            Props(new CoPilot(testActor, nilActor,
              nilActor)), copilotName)
        }
      }), "TestPilots")
    // Wait for the mailboxes to be up and running for the
    // children
    Await.result(
      a ? IsolatedLifeCycleSupervisor.WaitForStart, 3.seconds)
    // Tell the CoPilot that it's ready to go
    system.actorFor(copilotPath) ! Pilots.ReadyToGo
    a
  }

  // The Test code
  "CoPilot" should {
    "take control when the Pilot dies" in {
      pilotsReadyToGo()
      // Kill the Pilot
      system.actorFor(pilotPath) ! PoisonPill
      // Since the test class is the "Plane" we can
      // expect to see this request
      expectMsg(GiveMeControl)
      // The girl who sent it had better be Mary
      lastSender must be (system.actorFor(copilotPath))
    }
  }
}

