package edu.cornell.cdm89.scalaspec.domain

import java.io._

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import scala.collection.mutable
import breeze.linalg.DenseVector

import edu.cornell.cdm89.scalaspec.ode.OdeState
import edu.cornell.cdm89.scalaspec.ode.BogackiShampineStepper.ErrorEstimate
import edu.cornell.cdm89.scalaspec.pde.BoundaryCondition
import edu.cornell.cdm89.scalaspec.pde.LaxFriedrichsFlux.BoundaryValues
import edu.cornell.cdm89.scalaspec.pde.FluxConservativePde
import edu.cornell.cdm89.scalaspec.spectral.GllBasis

class DomainSubset(xL: Double, xR: Double, nElems: Int, order: Int,
    pde: FluxConservativePde) extends Actor with ActorLogging {
  val width = (xR - xL) / nElems
  val basis = GllBasis(order)
  val elements = mutable.Map.empty[Int, ActorRef]

  // TODO: Inject BCs
  val leftBc = new BoundaryCondition {
    def boundaryValues(t: Double, x: Double) = BoundaryValues(t,
        Vector.fill[Double](3)(0.0), Vector.fill[Double](3)(0.0), 1.0)
  }
  val rightBc = new BoundaryCondition {
    def boundaryValues(t: Double, x: Double) = BoundaryValues(t,
        Vector.fill[Double](3)(0.0), Vector.fill[Double](3)(0.0), -1.0)
  }
    
  override def preStart = {
    // Create boundaries
    context.actorOf(Props(classOf[ExternalBoundaryActor], xL, leftBc), "boundary0")
    for (i <- 1 to nElems-1) {
      val x = xL + i*width
      context.actorOf(Props(classOf[InternalBoundaryActor], x), s"boundary$i")
    }
    context.actorOf(Props(classOf[ExternalBoundaryActor], xR, rightBc), s"boundary$nElems")
    
    // Create elements
    for (i <- 0 until nElems) {
      val x = xL + i*width
      val map = new AffineMap(x, x+width)
      val elem = context.actorOf(Props(classOf[GllElement], basis, map, pde), s"interval$i")
      elements(i) = elem
    }
  }
  
  def receive = {
    case 'SendId =>
      val zeroId = OdeState(0.0, Vector(DenseVector.zeros[Double](order+1),
        DenseVector.zeros[Double](order+1), DenseVector.zeros[Double](order+1)))
      for ((k, v) <- elements) {
        val id = if (k == 5) {
          val myPsi = DenseVector.zeros[Double](order+1)
          val c = order/2
          myPsi(c-2) = 0.3; myPsi(c-1) = 0.6; myPsi(c) = 1.0; myPsi(c+1) = 0.6; myPsi(c+2) = 0.3
          val myPi = DenseVector.zeros[Double](order+1)
          val myPhi = basis.differentiate(myPsi)
          OdeState(0.0, Vector(myPsi, myPi, myPhi))
        } else zeroId
        v ! GllElement.InitialData(id)
      }
      
      /* val zeroId = OdeState(0.0, Vector(DenseVector.zeros[Double](order+1)))
      for ((k, v) <- elements) {
        val id = if (k == 5) {
          val myPsi = DenseVector.zeros[Double](order+1)
          myPsi(2) = 0.3; myPsi(3) = 0.6; myPsi(4) = 1.0; myPsi(5) = 0.6; myPsi(6) = 0.3
          OdeState(0.0, Vector(myPsi))
        } else zeroId
        v ! GllElement.InitialData(id)
      } */
        
      context.become(initializing(sender, emptyResponses))
    case msg => log.info("Dunno what to do: " + msg)
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
      elements.values foreach { _ ! step }
      context.become(stepping(sender, emptyResponses))
    case interp: GllElement.Interpolate =>
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