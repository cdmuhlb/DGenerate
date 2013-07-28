package edu.cornell.cdm89.scalaspec.domain

import java.io._

import scala.collection.{mutable, immutable}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.actor.Identify
import akka.cluster.Cluster
import akka.remote.RemoteScope
import breeze.linalg.DenseVector

import edu.cornell.cdm89.scalaspec.pde.FluxConservativePde
import edu.cornell.cdm89.scalaspec.spectral.GllBasis
import edu.cornell.cdm89.scalaspec.driver.{YgraphObserver, YgraphInterpObserver}
import edu.cornell.cdm89.scalaspec.util.ResponseTracker

object Subdomain {
  case class FindRemoteBoundary(index: Int, messageId: Any)
  case class ElementsList(elements: immutable.Seq[ActorRef])
}

class Subdomain(grid: GridDistribution, pde: FluxConservativePde,
    nodeId: Int) extends Actor with ActorLogging {
  val boundaries = mutable.Map.empty[Int, ActorRef]
  val elements = mutable.Map.empty[Int, ActorRef]

  override def preStart = {
    createBoundaries()
  }

  def receive = setup1

  def setup1: Receive = {
    case GllElement.CreateElements(domainRouter) =>
      createElements(domainRouter)
      sender ! 'ElementsCreated
      context.become(setup2(sender, domainRouter,
          new ResponseTracker[Any, ActorRef](elements.values.toSet)))
    case Subdomain.FindRemoteBoundary(index, messageId) =>
      boundaries.get(index) foreach { _ forward Identify(messageId) }
  }

  def setup2(controller: ActorRef, domainRouter: ActorRef,
      tracker: ResponseTracker[Any, ActorRef]): Receive = {
    case 'GetLocalElements =>
      sender ! Subdomain.ElementsList(elements.values.toList)
    case GllElement.FindBoundary(index, messageId) =>
      if (boundaries.contains(index)) boundaries(index) forward Identify(messageId)
      else domainRouter forward Subdomain.FindRemoteBoundary(index, messageId)
    case Subdomain.FindRemoteBoundary(index, messageId) =>
      boundaries.get(index) foreach { _ forward Identify(messageId) }
    case 'Ready =>
      tracker.register('Ready, sender)
      if (tracker.keyCompleted('Ready)) {
        controller ! 'AllReady
        context.become(ready(controller))
      }
  }

  def ready(controller: ActorRef): Receive = {
    case 'GetLocalElements =>
      sender ! Subdomain.ElementsList(elements.values.toList)
    case 'GetStepper =>
      elements.values foreach { _ forward 'GetStepper }
    case 'DoneStepping =>
      // Do nothing; let Observer dictate shutdown
    case 'DoneObserving =>
      controller ! 'AllDone
      context.become(finished)
  }

  def finished: Receive = {
    case 'Shutdown =>
      context.system.shutdown()
  }

  private def createBoundaries(): Unit = {
    // Periodic BC HACK
    //boundaries(0) = context.actorOf(Props(classOf[InternalBoundaryActor],
    //  -1.0), "boundary0")

    for ((b, bc) <- grid.myExternalBoundaries(nodeId)) {
      boundaries(b.index) = context.actorOf(Props(classOf[ExternalBoundaryActor],
          b.x, bc), s"boundary${b.index}")

      // Periodic BC HACK
      //if (b.index != 0) {
      //  boundaries(b.index) = context.actorOf(Props(classOf[Forwarder],
      //      boundaries(0)), s"boundary${b.index}")
      //}
    }
    for (b <- grid.myInternalBoundaries(nodeId)) {
      boundaries(b.index) = context.actorOf(Props(classOf[InternalBoundaryActor],
            b.x), s"boundary${b.index}")
    }
  }

  private def createElements(domainRouter: ActorRef): Unit = {
    val bases = mutable.Map.empty[Int, GllBasis]
    for (e <- grid.myElements(nodeId)) {
      val map = new AffineMap(e.xL, e.xR)
      val basis = bases.getOrElseUpdate(e.order, GllBasis(e.order))
      elements(e.index) = context.actorOf(Props(classOf[GllElement],
          basis, map, pde), s"interval${e.index}")
    }
  }
}

class Forwarder(receiver: ActorRef) extends Actor {
  def receive = {
    case msg => receiver forward msg
  }
}
