package zzz.akka.avionics

import akka.actor.{FSM, Actor, ActorRef}
import zzz.akka.avionics.Altimeter.RateChangeOffset
import zzz.akka.avionics.HeadingIndicator.BankChangeOffset
import scala.concurrent.duration._

object WeatherBehaviour {

  sealed trait State

  case object Windy extends State

  case object Idle extends State

  case object StartWeather extends State
  case object StopWeather extends State

  sealed trait Data
  case class WindyOffset(alt: Float, head: Float)
  case object Uninitialized extends Data
  case class WeatherData(altimeter: ActorRef,
                         headingIndicator: ActorRef,
                         wind: WindyOffset) extends Data

}

trait WeatherResolution {
  import scala.util.Random
  def windInterval(): FiniteDuration = Random.nextInt(300).seconds
}

class WeatherBehaviour(heading: ActorRef, altimeter: ActorRef) extends Actor
  with FSM[WeatherBehaviour.State, WeatherBehaviour.Data] {
  this: WeatherResolution =>

  import FSM._
  import WeatherBehaviour._
  import scala.util.Random

  case object ApplyWeather
  case object WindChange

  startWith(Idle, Uninitialized)

  // Just provides shorter access to the scheduler
  val scheduler = context.system.scheduler

  def applyWeather(weather: WeatherData): WeatherData = {
    val WeatherData(alt, head, wind) = weather
    val WindyOffset(alto, heado) = wind
    alt ! RateChangeOffset(alto)
    head ! BankChangeOffset(heado)
    weather
  }

  def randomWind(): WindyOffset = {
//    val alt = 0.001f
    val alt = if (Random.nextBoolean()) Random.nextFloat() else -1 * Random.nextFloat()
    val head = if (Random.nextBoolean()) Random.nextFloat() else -1 * Random.nextFloat()
    WindyOffset(alt, head)
  }

  when(Idle) {
    case Event(StartWeather, _) =>
      goto(Windy) using WeatherData(heading, altimeter, WindyOffset(0, 0))

  }

  onTransition {
    case Idle -> Windy =>
      setTimer("ApplyWeather", ApplyWeather, 600.milliseconds,
        repeat = true)
      setTimer("ChangeWeather", WindChange, 800.milliseconds,
        repeat = true)
//      windChange()

    case Windy -> Idle =>
      cancelTimer("ApplyWeather")
      cancelTimer("ChangeWeather")
  }

  when(Windy) {
    case Event(ApplyWeather, weather: WeatherData) =>
      stay using applyWeather(weather)
    case Event(WindChange, weather: WeatherData) =>
      stay using weather.copy(wind = randomWind())
  }

  whenUnhandled {
    case Event(StopWeather, _) =>
      goto(Idle)
  }

  initialize

}
