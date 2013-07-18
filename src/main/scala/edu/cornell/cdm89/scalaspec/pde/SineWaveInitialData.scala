package edu.cornell.cdm89.scalaspec.pde

import akka.actor.{Actor, ActorRef}
import breeze.numerics._

import edu.cornell.cdm89.scalaspec.domain.GllElement.{Coords, InitialData}
import edu.cornell.cdm89.scalaspec.ode.OdeState

class SineWaveInitialData(subdomain: ActorRef) extends Actor {
  def receive = {
    case 'ProvideId =>
      //log.info("ID asking for coords")
      subdomain ! 'GetCoordsForId
    case Coords(x) =>
      val t0 = 0.0
      val u0 = Vector(sin(x))
      sender ! InitialData(OdeState(t0, u0))
  }
}
