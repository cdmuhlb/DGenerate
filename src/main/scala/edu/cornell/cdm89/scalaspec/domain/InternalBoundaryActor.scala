package edu.cornell.cdm89.scalaspec.domain

import akka.actor.{Actor, ActorLogging, ActorRef}

import edu.cornell.cdm89.scalaspec.pde.LaxFriedrichsFlux
import LaxFriedrichsFlux.BoundaryValues

class InternalBoundaryActor(x: Double) extends Actor with ActorLogging {
  //override def preStart(): Unit = log.info(s"Starting internal boundary at $x")
  
  def receive = state0

  def state0: Receive = {
    case bv1: BoundaryValues =>
      context.become(state1(bv1, sender))
  }
  
  def state1(bv1: BoundaryValues, element1: ActorRef): Receive = {
    case bv2: BoundaryValues =>
      require(bv1.t == bv2.t)
      require(bv1.norm*bv2.norm < 0.0)
      val element2 = sender

      val flux = LaxFriedrichsFlux.flux(bv1, bv2)
      
      //println(self.path.name + s": $bv1 $bv2 -> ${flux}")
      
      element1 ! flux
      element2 ! flux

      context.become(state0)
  }
}