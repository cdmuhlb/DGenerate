package edu.cornell.cdm89.scalaspec.domain

import java.io._

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.actor.Identify
import akka.cluster.Cluster
import akka.remote.RemoteScope
import scala.collection.mutable
import breeze.linalg.DenseVector

import edu.cornell.cdm89.scalaspec.ode.OdeState
import edu.cornell.cdm89.scalaspec.pde.FluxConservativePde
import edu.cornell.cdm89.scalaspec.spectral.GllBasis
import edu.cornell.cdm89.scalaspec.driver.YgraphObserver

class Subdomain(grid: GridDistribution, pde: FluxConservativePde,
    nodeId: Int) extends Actor with ActorLogging {
  val boundaries = mutable.Map.empty[Int, ActorRef]
  val elements = mutable.Map.empty[Int, ActorRef]
  val obs = context.actorOf(Props(classOf[YgraphObserver], 0.1), "obs")

  override def preStart = {
    createBoundaries()
  }

  def receive = setup1

  def setup1: Receive = {
    case GllElement.CreateElements(domainRouter) =>
      createElements(domainRouter)
      sender ! 'ElementsCreated
      context.become(setup2(sender, emptyResponses))
    case GllElement.FindBoundary(index, messageId) =>
      boundaries.get(index) foreach { _ forward Identify(messageId) }
  }

  def setup2(controller: ActorRef,
      responses: mutable.Map[ActorRef, Boolean]): Receive = {
    case 'GetCoordsForId =>
      elements.values foreach { _ forward 'GetCoords }
    case GllElement.FindBoundary(index, messageId) =>
      boundaries.get(index) foreach { _ forward Identify(messageId) }
    case 'Ready =>
      responses(sender) = true
      if (responses.forall(_._2)) {
        elements.values foreach { _ ! GllElement.SetObserver(obs) }
        context.become(setup3(controller, emptyResponses))
      }
  }

  def setup3(controller: ActorRef,
      responses: mutable.Map[ActorRef, Boolean]): Receive = {
    case 'Ack =>
      responses(sender) = true
      if (responses.forall(_._2)) {
        controller ! 'AllReady
        context.become(ready(controller, emptyResponses))
      }
  }

  def ready(controller: ActorRef,
      responses: mutable.Map[ActorRef, Boolean]): Receive = {
    case 'GetStepper =>
      elements.values foreach { _ forward 'GetStepper }
    case 'DoneStepping =>
      responses(sender) = true
      if (responses.forall(_._2)) {
        controller ! 'AllDone
        context.become(finished)
      }
  }

  def finished: Receive = {
    case 'Shutdown =>
      context.system.shutdown()
  }

  def emptyResponses: mutable.Map[ActorRef, Boolean] = {
    val responses = mutable.Map.empty[ActorRef, Boolean]
    elements.values foreach { responses(_) = false }
    responses
  }

  private def createBoundaries(): Unit = {
    for ((b, bc) <- grid.myExternalBoundaries(nodeId)) {
      boundaries(b.index) = context.actorOf(Props(classOf[ExternalBoundaryActor],
          b.x, bc), s"boundary${b.index}")
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
          basis, map, pde, domainRouter), s"interval${e.index}")
    }
  }
}
