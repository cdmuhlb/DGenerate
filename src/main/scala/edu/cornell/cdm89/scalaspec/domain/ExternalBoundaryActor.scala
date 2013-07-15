package edu.cornell.cdm89.scalaspec.domain

import akka.actor.{Actor, ActorLogging}

import edu.cornell.cdm89.scalaspec.pde.BoundaryCondition
import edu.cornell.cdm89.scalaspec.pde.LaxFriedrichsFlux
import LaxFriedrichsFlux.BoundaryValues

class ExternalBoundaryActor(x: Double, bc: BoundaryCondition) extends Actor with ActorLogging {
  //override def preStart(): Unit = log.info(s"Starting external boundary at $x")

  def receive = {
    case bv1: BoundaryValues =>
      val bv2 = bc.boundaryValues(bv1.t, x)
      val flux = LaxFriedrichsFlux.flux(bv1, bv2)
      sender ! flux
  }
}