package edu.cornell.cdm89.scalaspec.domain

import java.io._

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.actor.{Address, Deploy}
import akka.cluster.Cluster
import akka.remote.RemoteScope
import scala.collection.mutable
import breeze.linalg.DenseVector

import edu.cornell.cdm89.scalaspec.ode.OdeState
import edu.cornell.cdm89.scalaspec.ode.BogackiShampineStepper.ErrorEstimate
import edu.cornell.cdm89.scalaspec.pde.BoundaryCondition
import edu.cornell.cdm89.scalaspec.pde.LaxFriedrichsFlux.BoundaryValues
import edu.cornell.cdm89.scalaspec.pde.FluxConservativePde
import edu.cornell.cdm89.scalaspec.spectral.GllBasis

class DomainSubset(dom: DomainInfo, pde: FluxConservativePde,
    controller: ActorRef) extends Actor with ActorLogging {
  val width = (dom.xR - dom.xL) / dom.nElems
  val basis = GllBasis(dom.order)
  val elements = mutable.Map.empty[Int, ActorRef]
  
  // HACK
  val elemsOnNode1 = 4
  val nodeNum = if (Cluster(context.system).selfAddress.port == Some(2551)) 1 else 2

  // TODO: Inject BCs
  val nVars = 3
  val leftBc = new BoundaryCondition {
    def boundaryValues(t: Double, x: Double) = {
      //val a = 2.0*math.Pi
      //val u = math.sin(x - a*t)
      BoundaryValues(t,
          Vector.fill[Double](nVars)(0.0), Vector.fill[Double](nVars)(0.0), 1.0)
          //Vector.fill[Double](nVars)(u), Vector.fill[Double](nVars)(a*u), 1.0)
    }
  }
  val rightBc = new BoundaryCondition {
    def boundaryValues(t: Double, x: Double) = BoundaryValues(t,
        Vector.fill[Double](nVars)(0.0), Vector.fill[Double](nVars)(0.0), -1.0)
  }
    
  override def preStart = {
    // Create boundaries
    if (nodeNum == 1) {
      context.actorOf(Props(classOf[ExternalBoundaryActor], dom.xL, leftBc), "boundary0")
      for (i <- 1 until elemsOnNode1.min(dom.nElems-1)) {
        val x = dom.xL + i*width
        context.actorOf(Props(classOf[InternalBoundaryActor], x), s"boundary$i")
      }
      if (dom.nElems < elemsOnNode1) {
        context.actorOf(Props(classOf[ExternalBoundaryActor], dom.xR, rightBc), s"boundary${dom.nElems}")
      }
    } else {
      for (i <- elemsOnNode1 to dom.nElems-1) {
        val x = dom.xL + i*width
        context.actorOf(Props(classOf[InternalBoundaryActor], x), s"boundary$i")
      }
      if (dom.nElems >= elemsOnNode1) {
        context.actorOf(Props(classOf[ExternalBoundaryActor], dom.xR, rightBc), s"boundary${dom.nElems}")
      }
    }
    
    Thread.sleep(2000)
    
    // Create elements
    for (i <- 0 until dom.nElems) {
      val x = dom.xL + i*width
      val map = new AffineMap(x, x+width)
      if (((nodeNum == 1) && (i < elemsOnNode1)) ||
          ((nodeNum == 2) && (i >= elemsOnNode1))) {
        val elem = context.actorOf(Props(classOf[GllElement], basis, map, pde), s"interval$i")
        elements(i) = elem
      }
    }
    self ! 'Initialize
  }
  
  def receive = {
    case 'Initialize =>
      controller ! 'DomainInitializing
      context.become(initializing(controller, emptyResponses))
  }
  
  def initializing(controller: ActorRef, responses: mutable.Map[ActorRef, Boolean]): Receive = {
    case 'Ready =>
      responses(sender) = true
      if (responses.forall(_._2)) {
        controller ! 'AllReady
        context.become(ready)
      }
  }
  
  def ready: Receive = {
    case step: GllElement.StepTo =>
      log.info("Stepping")
      elements.values foreach { _ ! step }
      context.become(stepping(sender, emptyResponses))
    case interp: GllElement.Interpolate =>
      log.info("Interpolating")
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
}