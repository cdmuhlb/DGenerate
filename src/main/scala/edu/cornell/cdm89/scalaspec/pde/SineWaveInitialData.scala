package edu.cornell.cdm89.scalaspec.pde

import akka.actor.Actor
import breeze.numerics._

import edu.cornell.cdm89.scalaspec.domain.GllElement.{Coords, InitialData}
import edu.cornell.cdm89.scalaspec.ode.OdeState

class SineWaveInitialData extends Actor {
  override def preStart = {
    val elements = context.actorSelection("/user/domain0/interval*")
    elements ! 'GetCoords
  }
  
  def receive = {
    case Coords(x) =>
      val t0 = 0.0
      val u0 = Vector(sin(x))
      sender ! InitialData(OdeState(t0, u0))
  }
}
