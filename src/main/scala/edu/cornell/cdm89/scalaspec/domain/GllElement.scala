package edu.cornell.cdm89.scalaspec.domain

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.actor.{ActorIdentity, Identify}
import breeze.linalg.DenseVector

import edu.cornell.cdm89.scalaspec.spectral.GllBasis
import edu.cornell.cdm89.scalaspec.ode.{Ode, OdeState, TakeStep, FieldVec, BogackiShampineStepper}
import edu.cornell.cdm89.scalaspec.ode.FluxConservativeMethodOfLines
import edu.cornell.cdm89.scalaspec.pde.FluxConservativePde

object GllElement {
  case class InitialData(state: OdeState)
  case class StepTo(t: Double)
  case class AdvanceState(newState: OdeState, rhs: FieldVec)
  case class RhsResult(state: OdeState, rhs: FieldVec)
  case class Interpolate(t: Double)
  case class Interpolation(state: OdeState, x: DenseVector[Double])
}

class GllElement(basis: GllBasis, map: AffineMap,
    pde: FluxConservativePde) extends Actor with ActorLogging {
  val controller = context.parent
  val name = self.path.name
  val coords = basis.nodes map map.mapX
  
  class SetupTracker {
    private var leftBoundary = Option.empty[ActorRef]
    private var rightBoundary = Option.empty[ActorRef]
    private var id = Option.empty[OdeState]
    private var ode = Option.empty[Ode]
    
    def haveLeft(left: ActorRef): Unit = {
      leftBoundary = Some(left)
      checkBoundaries()
    }
    
    def haveRight(right: ActorRef): Unit = {
      rightBoundary = Some(right)
      checkBoundaries()
    }
    
    def haveId(state: OdeState): Unit = {
      id = Some(state)
      checkCompletion()
    }
    
    private def checkBoundaries(): Unit = {
      ode = for (left <- leftBoundary; right <- rightBoundary) yield
          new FluxConservativeMethodOfLines(pde, basis, map.jacobian, left, right)
      checkCompletion()
    }
    
    private def checkCompletion(): Unit = {
      if (ode.nonEmpty) {
        context.become(uninitialized(ode.get))
        if (id.nonEmpty) {
          self ! GllElement.InitialData(id.get)
        }
      }
    }
  }

  override def preStart = {
    //log.info(s"Starting element '$name' of order ${basis.order}")
    // look up boundaries
    assert(name.startsWith("interval"))
    val index = name.substring(8).toInt
    context.actorSelection(s"../boundary$index") ! Identify('Left)
    context.actorSelection(s"../boundary${index+1}") ! Identify('Right)
  }
  
  import GllElement._
  
  def receive = setup(new SetupTracker)
  
  def setup(tracker: SetupTracker): Receive = {
    case ActorIdentity('Left, Some(actor)) =>
      tracker.haveLeft(actor)
    case ActorIdentity('Right, Some(actor)) =>
      tracker.haveRight(actor)
    case InitialData(state) =>
      tracker.haveId(state)
  }
  
  def uninitialized(ode: Ode): Receive = {
    case InitialData(state) =>
      import context.dispatcher
      ode.rhs(state) onSuccess { case rhs => self ! RhsResult(state, rhs) }
    case RhsResult(state, rhs) =>
      //log.info("Got initial RHS")
      val stepper = context.system.actorOf(Props(classOf[BogackiShampineStepper], ode, self, controller))
      controller ! 'Ready
      context.become(initialized(stepper, state, rhs))
  }
  
  def initialized(stepper: ActorRef, state: OdeState, rhs: FieldVec): Receive = {
    case StepTo(t) =>
      stepper ! TakeStep(t - state.t, state, rhs)
    case AdvanceState(newState, newRhs) =>
      controller ! 'Advanced
      context.become(active(stepper, state, rhs, newState, newRhs))
  }
  
  def active(stepper: ActorRef, lastState: OdeState, lastRhs: FieldVec,
      currentState: OdeState, currentRhs: FieldVec): Receive = {
    case StepTo(t) =>
      stepper ! TakeStep(t - currentState.t, currentState, currentRhs)
    case AdvanceState(newState, newRhs) =>
      controller ! 'Advanced
      context.become(active(stepper, currentState, currentRhs, newState, newRhs))
    case Interpolate(t) =>
      require(t >= lastState.t && t <= currentState.t)
      if (t == lastState.t) sender ! Interpolation(lastState, coords)
      else if (t == currentState.t) sender ! Interpolation(currentState, coords)
      else sender ! 'NYI
  }
}