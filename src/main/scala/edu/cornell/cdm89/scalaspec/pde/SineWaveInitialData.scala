package edu.cornell.cdm89.scalaspec.pde

import akka.actor.{Actor, ActorRef}
import breeze.numerics._

import edu.cornell.cdm89.scalaspec.domain.GllElement.{Coords, InitialData}
import edu.cornell.cdm89.scalaspec.domain.Subdomain
import edu.cornell.cdm89.scalaspec.ode.OdeState

class SineWaveInitialData(subdomain: ActorRef) extends Actor {
  // TODO: Refactor into base class
  def receive = {
    case 'ProvideId =>
      subdomain ! 'GetLocalElements
    case Subdomain.ElementsList(elements) =>
      elements foreach { _ ! 'GetCoords }
      context.become(idProvider(elements))
  }

  def idProvider(elements: Seq[ActorRef]): Receive = {
    // TODO: Count responses
    case Coords(x) =>
      val t0 = 0.0
      val u0 = Vector(sin(x))
      sender ! InitialData(OdeState(t0, u0))
  }
}
