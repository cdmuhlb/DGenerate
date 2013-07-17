package edu.cornell.cdm89.scalaspec.domain

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.actor.{Address, ActorIdentity, Identify}
import akka.cluster.Cluster
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
  case class Coords(x: DenseVector[Double])
  case class Interpolate(t: Double)
  case class Interpolation(state: OdeState, x: DenseVector[Double])
  case class FindBoundary(index: Int, messageId: Any)
  case class CreateElements(domain: ActorRef)
}

class GllElement(basis: GllBasis, map: AffineMap,
    pde: FluxConservativePde, domain: ActorRef) extends Actor with ActorLogging {
  import GllElement._
  
  val controller = context.parent
  val name = self.path.name
  val coords = basis.nodes map map.mapX
  val minDx = (coords.toArray.sliding(2) map {p => p(1) - p(0)}).min
  
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
    domain ! FindBoundary(index, 'Left)
    domain ! FindBoundary(index+1, 'Right)
  }
  
  def receive = setup(new SetupTracker)
  
  def setup(tracker: SetupTracker): Receive = {
    case ActorIdentity('Left, Some(actor)) =>
      //log.info("Got left")
      tracker.haveLeft(actor)
    case ActorIdentity('Right, Some(actor)) =>
      //log.info("Got right")
      tracker.haveRight(actor)
    case ActorIdentity(lr, None) =>
      //log.error(s"No actor for boundary at $lr")
    case InitialData(state) =>
      //log.info("Got early ID")
      tracker.haveId(state)
    case 'GetCoords =>
      //log.info("Giving early coords")
      sender ! Coords(coords)
  }
  
  def uninitialized(ode: Ode): Receive = {
    case 'GetCoords =>
      //log.info("Giving late coords")
      sender ! Coords(coords)
    case InitialData(state) =>
      //log.info("Got late ID")
      import context.dispatcher
      ode.rhs(state) onSuccess { case rhs => self ! RhsResult(state, rhs) }
    case RhsResult(state, rhs) =>
      //log.info("Got RHS")
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
    case Interpolate(t) =>
      require(t == state.t)
      sender ! Interpolation(state, coords)
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