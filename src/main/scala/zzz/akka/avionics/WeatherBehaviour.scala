package zzz.akka.avionics

import akka.actor.{Props, FSM, Actor, ActorRef}
import zzz.akka.avionics.Altimeter.RateChangeOffset
import zzz.akka.avionics.HeadingIndicator.BankChangeOffset
import scala.concurrent.duration._

trait WeatherBehaviourProvider {
  def newWeatherBehaviour(altimeter: ActorRef, heading: ActorRef): Props =
    Props(WeatherBehaviour(altimeter, heading))
}

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

  // Factory method to instantiate it with the production
  // timer resolution
  def apply(altimeter: ActorRef, heading: ActorRef) =
    new WeatherBehaviour(altimeter, heading) with WeatherResolution
}

trait WeatherResolution {
  import scala.util.Random
  def windInterval(): FiniteDuration = Random.nextInt(300).seconds
}

class WeatherBehaviour(altimeter: ActorRef, heading: ActorRef) extends Actor
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
      goto(Windy) using WeatherData(altimeter, heading, WindyOffset(0, 0))

  }

  onTransition {
    case Idle -> Windy =>
      setTimer("ApplyWeather", ApplyWeather, 300.milliseconds,
        repeat = true)
      setTimer("ChangeWeather", WindChange, windInterval(),
        repeat = true)

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
