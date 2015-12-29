package zzz.akka.avionics

import akka.actor.{FSM, Actor, ActorRef}
import scala.concurrent.duration._

object WeatherBehaviour {

  sealed trait State

  case object Windy extends State

  case object Idle extends State

  case class WindyOffset(alt: Double, head: Float)

  case class WindChanged(alt: Double, head: Float)

  sealed trait Data

  case class WeatherData(controls: ActorRef,
                         wind: WindyOffset) extends Data

}

trait WeatherResolution {
  import scala.util.Random
  def windInterval(): FiniteDuration = Random.nextInt(300).seconds
}

class WeatherBehaviour(alt: ActorRef) extends Actor
  with FSM[WeatherBehaviour.State, WeatherBehaviour.Data] {
  this: WeatherResolution =>
  import WeatherBehaviour._

  // Just provides shorter access to the scheduler
  val scheduler = context.system.scheduler

  def windChange() = scheduler.scheduleOnce(windInterval(),
    self, WindChanged(0,0.005f))

  onTransition {
    case _ -> Windy =>
      windChange()
      plane ! Windy
  }


}
