package edu.cornell.cdm89.scalaspec.ode

import akka.actor.{Actor, ActorRef, Props}

import edu.cornell.cdm89.scalaspec.spectral.GllBasis

object TimeStepper {
  case class InitializeState(state: ElementState, rhs: FieldVec)

  trait TimeChunk {
    def lastState: ElementState
    def lastRhs: FieldVec
    def currentState: ElementState
    def currentRhs: FieldVec
    def interpolate(t: Double): ElementState

    def lastTime: Double = lastState.t
    def currentTime: Double = currentState.t
  }
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

  def ready(state: ElementState, rhs: FieldVec): Receive = {
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
      //val filteredChunk = filterChunk(chunk)
      val filteredChunk = chunk
      element ! AdvanceState(filteredChunk)
      context.become(ready(filteredChunk.currentState, filteredChunk.currentRhs))
    case 'StepRejected =>
      context.become(ready(chunk.lastState, chunk.lastRhs))
    case TimestepController.SpeculativeYes(dt) =>
      // TODO: Let's get the easy stuff working first...
  }

  def filterChunk(chunk: TimeChunk): TimeChunk = {
    val alpha = 36
    val sTilde = 8
    val basis = GllBasis(chunk.currentState.u(0).length-1)
    val filteredState = chunk.currentState.u map { ui =>
      val spec = basis.spectralCoefficients(ui)
      for (i <- 0 until spec.length) {
        val eta = i / spec.length
        spec(i) *= math.exp(-alpha * math.pow(eta, 2.0*sTilde))
      }
      basis.sumCoefficients(spec)
    }
    BogackiShampineStepper.RK3TimeChunk(chunk.lastState, chunk.lastRhs,
        ElementState(chunk.currentState.t, chunk.currentState.x, filteredState), chunk.currentRhs)
  }
}
