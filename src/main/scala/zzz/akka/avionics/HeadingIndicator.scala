package zzz.akka.avionics

import akka.actor.{Actor, ActorLogging}
import scala.concurrent.duration._

trait HeadingIndicatorProvider {
  def newHeadingIndicator: Actor = new HeadingIndicator with ProductionEventSource
}

object HeadingIndicator {
  // Indicates that something has changed how fast
  // we're changing direction
  case class BankChange(amount: Float)
  case class BankChangeOffset(amount: Float)
  // The event published by the HeadingIndicator to
  // listeners that want to know where we're headed
  case class HeadingUpdate(heading: Float)

}

trait HeadingIndicator extends Actor with ActorLogging {
  this: EventSource =>
  import HeadingIndicator._
  import context._
  // Internal message we use to recalculate our heading
  case object Tick
  // Maximum degrees-per-second that our plane can move
  val maxDegPerSec = 5
  // Our timer that schedules our updates
  val ticker = system.scheduler.schedule(100.millis,
    100.millis,
    self, Tick)
  // The last tick which we can use to calculate our changes
  var lastTick: Long = System.currentTimeMillis
  // The current rate of our bank
  var rateOfBank = 0f
  var rateOfBankOffset= 0f
  // Holds our current direction
  var heading = 0f
  def headingIndicatorReceive: Receive = {
    // Keeps the rate of change within [-1, 1]
    case BankChange(amount) =>
      rateOfBank = (amount + rateOfBankOffset).min(1.0f).max(-1.0f)
    // Calculates our heading delta based on the current
    // rate of change, the time delta from our last
    // calculation, and the max degrees per second
    case Tick =>
      val tick = System.currentTimeMillis
      val timeDelta = (tick - lastTick) / 1000f
      val degs = rateOfBank * maxDegPerSec
      heading = (heading + (360 + (timeDelta * degs))) % 360
      lastTick = tick
      // Send the HeadingUpdate event to our listeners
      sendEvent(HeadingUpdate(heading))

    case BankChangeOffset(amount) =>
      rateOfBankOffset = amount

  }
  // Remember that we're mixing in the EventSource and thus
  // have to compose our receive partial function
  // accordingly
  def receive = eventSourceReceive orElse
    headingIndicatorReceive
  // Don't forget to cancel our timer when we shut down
  override def postStop(): Unit = ticker.cancel
}

