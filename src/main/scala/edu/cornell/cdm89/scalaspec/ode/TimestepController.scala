package edu.cornell.cdm89.scalaspec.ode

import akka.actor.{Actor, ActorLogging, ActorRef}
import edu.cornell.cdm89.scalaspec.domain.GllElement

object TimestepController {
  case class TakeStep(dt: Double)
  case class ErrorMeasurement(t: Double, err: Double)
  // 'StepApproved
  // 'StepRejected
  case class SpeculativeYes(dt: Double)
}

class TimestepController(domain: ActorRef, t0: Double, dt: Double, tf: Double)
    extends Actor with ActorLogging {
  import TimestepController._

  override def preStart = {
    domain ! 'GetStepper
  }

  def receive = {
    case GllElement.HaveTimeStepper(ts) =>
      ts ! TakeStep(dt)
    case ErrorMeasurement(t, err) =>
      sender ! 'StepApproved
      if (t < tf) sender ! TakeStep(dt)
  }
}
