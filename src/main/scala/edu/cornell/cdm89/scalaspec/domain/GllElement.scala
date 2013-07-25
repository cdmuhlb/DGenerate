package edu.cornell.cdm89.scalaspec.domain

import scala.concurrent.duration._
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.actor.{Address, ActorIdentity, Identify}
import akka.cluster.Cluster
import akka.pattern.ask
import akka.util.Timeout
import breeze.linalg.DenseVector

import edu.cornell.cdm89.scalaspec.spectral.GllBasis
import edu.cornell.cdm89.scalaspec.ode.{Ode, OdeState, FieldVec}
import edu.cornell.cdm89.scalaspec.ode.{TimeStepper, BogackiShampineStepper, TimestepController}
import edu.cornell.cdm89.scalaspec.ode.FluxConservativeMethodOfLines
import edu.cornell.cdm89.scalaspec.pde.FluxConservativePde
import edu.cornell.cdm89.scalaspec.ode.TimeStepper.{InitializeState, TimeChunk}

object GllElement {
  case class InitialData(state: OdeState)
  case class StepTo(t: Double)
  case class AdvanceState(chunk: TimeChunk)
  case class RhsResult(state: OdeState, rhs: FieldVec)
  case class Coords(x: DenseVector[Double])
  case class FindBoundary(index: Int, messageId: Any)
  case class CreateElements(domain: ActorRef)
  case class SetObserver(obs: ActorRef)
  case class StateChanged(name: String, coords: DenseVector[Double], chunk: TimeChunk)
  case class HaveTimeStepper(stepper: ActorRef)
}

class GllElement(basis: GllBasis, map: AffineMap,
    pde: FluxConservativePde) extends Actor with ActorLogging {
  import GllElement._

  val controller = context.parent
  val subdomain = context.parent
  val name = self.path.name
  val coords = basis.nodes map map.mapX
  val minDx = (coords.toArray.sliding(2) map {p => p(1) - p(0)}).min
  
  var stepper = context.system.deadLetters
  var observer = context.system.deadLetters

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
    // look up boundaries
    assert(name.startsWith("interval"))
    val index = name.substring(8).toInt
    subdomain ! FindBoundary(index, 'Left)
    subdomain ! FindBoundary(index+1, 'Right)
  }

  def receive = setup(new SetupTracker)

  def setup(tracker: SetupTracker): Receive = {
    case ActorIdentity('Left, Some(actor)) =>
      tracker.haveLeft(actor)
    case ActorIdentity('Right, Some(actor)) =>
      tracker.haveRight(actor)
    case ActorIdentity(lr, None) =>
      log.error(s"Could not find $lr boundary for element $name")
    case InitialData(state) =>
      tracker.haveId(state)
    case 'GetCoords =>
      sender ! Coords(coords)
  }

  def uninitialized(ode: Ode): Receive = {
    case 'GetCoords =>
      sender ! Coords(coords)
    case InitialData(state) =>
      import context.dispatcher
      ode.rhs(state) onSuccess { case rhs => self ! RhsResult(state, rhs) }
    case RhsResult(state, rhs) =>
      stepper = context.actorOf(Props(classOf[TimeStepper], ode))
      // TODO: I don't like this pattern
      import context.dispatcher
      implicit val timeout = Timeout(1.minute)
      (stepper ? InitializeState(state, rhs)) onSuccess { case 'Initialized =>
        controller ! 'Ready }
      context.become(initialized(state, rhs))
  }

  def initialized(state: OdeState, rhs: FieldVec): Receive = {
    case 'GetStepper => sender ! HaveTimeStepper(stepper)
    case SetObserver(obs) =>
      observer = obs
      sender ! 'Ack
    case AdvanceState(chunk) =>
      require(chunk.lastState.t == state.t)
      observer ! StateChanged(name, coords, chunk)
      context.become(active(chunk))
  }

  def active(state: TimeChunk): Receive = {
    case AdvanceState(chunk) =>
      require(chunk.lastState.t == state.currentState.t)
      observer ! StateChanged(name, coords, chunk)
      //log.info(s"Stepped to ${chunk.currentState.t}")
      context.become(active(chunk))
    case 'DoneStepping =>
      controller ! 'DoneStepping
  }
}
