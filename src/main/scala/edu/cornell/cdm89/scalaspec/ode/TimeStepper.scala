package edu.cornell.cdm89.scalaspec.ode

import akka.actor.{Actor, ActorRef, Props}

object TimeStepper {
  case class InitializeState(state: OdeState, rhs: FieldVec)
  case class TimeChunk(lastState: OdeState, lastRhs: FieldVec,
    currentState: OdeState, currentRhs: FieldVec)
}

class TimeStepper(ode: Ode) extends Actor {
  import TimeStepper._
  import edu.cornell.cdm89.scalaspec.domain.GllElement.AdvanceState
  
  var tsWorker = context.system.deadLetters
  val element = context.parent

  override def preStart = {
    tsWorker = context.system.actorOf(
        Props(classOf[BogackiShampineStepper], ode))
  }

  def receive = {
    case InitializeState(state, rhs) =>
      sender ! 'Initialized
      context.become(ready(state, rhs))
  }

  def ready(state: OdeState, rhs: FieldVec): Receive = {
    case TimestepController.TakeStep(dt) =>
      tsWorker ! BogackiShampineStepper.TakeStep(dt, state, rhs)
      context.become(stepping(sender))
    case 'DoneStepping =>
      element ! 'DoneStepping
  }

  def stepping(controller: ActorRef): Receive = {
    case BogackiShampineStepper.StepResult(chunk, err) =>
      controller ! TimestepController.ErrorMeasurement(
          chunk.currentState.t, err)
      context.become(stateCi(chunk))
  }

  def stateCi(chunk: TimeChunk): Receive = {
    case 'StepApproved =>
      element ! AdvanceState(chunk)
      context.become(ready(chunk.currentState, chunk.currentRhs))
    case 'StepRejected =>
      context.become(ready(chunk.lastState, chunk.lastRhs))
    case TimestepController.SpeculativeYes(dt) =>
      // TODO: Let's get the easy stuff working first...
  }
}
