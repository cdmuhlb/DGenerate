package edu.cornell.cdm89.scalaspec.domain

import java.io._

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.actor.Identify
import akka.cluster.Cluster
import akka.remote.RemoteScope
import scala.collection.mutable
import breeze.linalg.DenseVector

import edu.cornell.cdm89.scalaspec.ode.OdeState
import edu.cornell.cdm89.scalaspec.ode.BogackiShampineStepper.ErrorEstimate
import edu.cornell.cdm89.scalaspec.pde.FluxConservativePde
import edu.cornell.cdm89.scalaspec.spectral.GllBasis

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
        controller ! 'AllReady
        context.become(ready)
      }
  }

  def ready: Receive = {
    case step: GllElement.StepTo =>
      //log.info("Stepping")
      elements.values foreach { _ ! step }
      context.become(stepping(sender, emptyResponses))
    case interp: GllElement.Interpolate =>
      //log.info("Interpolating")
      elements.values foreach { _ ! interp }
      context.become(observing(sender, emptyResponses))
  }

  def stepping(controller: ActorRef, responses: mutable.Map[ActorRef, Boolean]): Receive = {
    case 'Advanced =>
      responses(sender) = true
      if (responses.forall(_._2)) {
        controller ! 'AllAdvanced
        context.become(ready)
      }
    case ErrorEstimate(err) => ;
  }

  def observing(controller: ActorRef, responses: mutable.Map[ActorRef, Boolean]): Receive = {
    case GllElement.Interpolation(state, x) =>
      responses(sender) = true
      val name = sender.path.name
      printState(name, state, x)
      if (responses.forall(_._2)) {
        controller ! 'AllObserved
        context.become(ready)
      }
  }

  def emptyResponses: mutable.Map[ActorRef, Boolean] = {
    val responses = mutable.Map.empty[ActorRef, Boolean]
    elements.values foreach { responses(_) = false }
    responses
  }

  def printState(name: String, state: OdeState, x: DenseVector[Double]): Unit = {
    val filename = "/tmp/harvest/" + name + ".yg"
    val out = new PrintWriter(new FileWriter(filename, true))
    //println(s"State @ t=${state.t}")
    out.println(f""""Time = ${state.t}%.6f""")
    for (i <- 0 until state.u.head.length) {
      //for (j <- state.u.indices) {
      //  print(state.u(j)(i) + "    ")
      //}
      //println
      out.println(f"${x(i)}%.6f    ${state.u(0)(i)}%.6f")
    }
    out.println
    out.close()
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
