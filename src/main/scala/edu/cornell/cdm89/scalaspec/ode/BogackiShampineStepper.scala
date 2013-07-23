package edu.cornell.cdm89.scalaspec.ode

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import breeze.linalg.DenseVector
import breeze.numerics._

object BogackiShampineStepper {
  case class TakeStep(dt: Double, y1: OdeState, k1: FieldVec)
  case class StepResult(chunk: TimeStepper.TimeChunk, err: Double)

  case class Stage2Result(k2: FieldVec)
  case class Stage3Result(k3: FieldVec)
  case class Stage4Result(state4: OdeState, k4: FieldVec)

  def measureError(y: FieldVec, z: FieldVec): Double = {
    val alpha = 1000.0
    val errVec = y.zip(z) map { case (yy, zz) =>
      (abs(yy - zz) :/ ((abs(yy):*alpha) + 1.0)).max
    }
    errVec.max
  }
}

class BogackiShampineStepper(ode: Ode) extends Actor {
  import BogackiShampineStepper._

  // All expensive computations get done in this Actor's thread pool!
  // Consider specifying execution service in ODE's constructor; then this
  // actor's dispatcher will only be used for the callbacks
  import context.dispatcher

  def receive = idle

  def idle: Receive = {
    case TakeStep(dt, state1, k1) =>
      val t2 = state1.t + 0.5*dt
      val y2 = state1.u.zip(k1) map { case(u, k) =>
        u + (k:*(0.5*dt))
      }
      val state2 = OdeState(t2, y2)
      val k2Future = ode.rhs(state2)
      k2Future onSuccess { case k2 => self ! Stage2Result(k2) }
      context.become(stage2(sender, dt, state1, k1))
  }

  def stage2(controller: ActorRef, dt: Double, state1: OdeState,
      k1: FieldVec): Receive = {
    case Stage2Result(k2) =>
      val t3 = state1.t + 0.75*dt
      val y3 = state1.u.zip(k2) map { case(u, k) =>
        u + (k:*(0.75*dt))
      }
      val state3 = OdeState(t3, y3)
      val k3Future = ode.rhs(state3)
      k3Future onSuccess { case k3 => self ! Stage3Result(k3) }
      context.become(stage3(controller, dt, state1, k1, k2))
  }

  def stage3(controller: ActorRef, dt: Double, state1: OdeState, k1: FieldVec,
      k2: FieldVec): Receive = {
    case Stage3Result(k3) =>
      val t4 = state1.t + dt
      val y4 = state1.u.zip(k1.zip(k2.zip(k3))) map {
        case (u, (kk1, (kk2, kk3))) =>
          u + (kk1:*(2.0*dt/9.0)) + (kk2:*(dt/3.0)) + (kk3:*(4.0*dt/9.0))
      }
      val state4 = OdeState(t4, y4)
      val k4Future = ode.rhs(state4)
      k4Future onSuccess { case k4 => self ! Stage4Result(state4, k4) }
      context.become(stage4(controller, dt, state1, k1, k2, k3))
  }

  def stage4(controller: ActorRef, dt: Double, state1: OdeState, k1: FieldVec,
      k2: FieldVec, k3: FieldVec): Receive = {
    case Stage4Result(state4, k4) =>
      val t4 = state1.t + dt
      val z4 = state1.u.zip(k1.zip(k2.zip(k3.zip(k4)))) map {
        case (u, (kk1, (kk2, (kk3, kk4)))) =>
          u + (kk1:*(7.0*dt/24.0)) + (kk2:*(dt/4.0)) + (kk3:*(dt/3.0)) +
          (kk4:*(dt/8.0))
      }
      val err = measureError(state4.u, z4)
      controller ! StepResult(TimeStepper.TimeChunk(state1, k1, state4, k4), err)
      context.become(idle)
  }

}
