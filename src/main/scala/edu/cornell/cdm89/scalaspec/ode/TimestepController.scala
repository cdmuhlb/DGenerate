package edu.cornell.cdm89.scalaspec.ode

import akka.actor.{Actor, ActorLogging}

import BogackiShampineStepper.ErrorEstimate

class TimestepController extends Actor with ActorLogging {
  def receive = {
    case ErrorEstimate(err) => log.info(s"Error estimate: $err")
  }
}