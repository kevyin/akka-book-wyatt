package zzz.akka.avionics

import akka.actor.{FSM, Actor, ActorRef}
import scala.concurrent.duration._

object WeatherBehaviour {

  sealed trait State

  case object LeftWind extends State

  case object RightWind extends State

  case object UpWind extends State

  case object DownWind extends State

  case object Idle extends State

  case class WindOffset(alt: Double, head: Float)

  sealed trait Data

  case class WindData(wind: WindOffset) extends Data

}

class WeatherBehaviour(plane: ActorRef) extends Actor
  with FSM[WeatherBehaviour.State, WeatherBehaviour.Data] {
  import WeatherBehaviour._

  when(Idle) {
    // chance of direction change
    transform {
      case _ =>
        goto (Idle)
    } using {
      case s =>
        s.copy(stateName = LeftWind)
    }
  }

}
