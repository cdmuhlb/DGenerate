package edu.cornell.cdm89.scalaspec.domain

import akka.actor.{ActorRef, ActorSystem, Props}
import breeze.linalg.DenseVector

import edu.cornell.cdm89.scalaspec.ode.{Ode, OdeState}
import edu.cornell.cdm89.scalaspec.pde.BoundaryCondition
import edu.cornell.cdm89.scalaspec.pde.FluxConservativePde
import edu.cornell.cdm89.scalaspec.pde.LaxFriedrichsFlux.BoundaryValues
import edu.cornell.cdm89.scalaspec.spectral.GllBasis

object DomainCreator {
  def create(xL: Double, xR: Double, nElems: Int, order: Int, pde: FluxConservativePde,
      controller: ActorRef, system: ActorSystem): Unit = {
    val width = (xR - xL) / nElems
    
    val leftBc = new BoundaryCondition {
      def boundaryValues(t: Double, x: Double) = BoundaryValues(t,
          Vector.fill[Double](3)(0.0), Vector.fill[Double](3)(0.0), 1.0)
    }
    val rightBc = new BoundaryCondition {
      def boundaryValues(t: Double, x: Double) = BoundaryValues(t,
          Vector.fill[Double](3)(0.0), Vector.fill[Double](3)(0.0), -1.0)
    }
    // TODO: Inject BCs, ID
    
    val id = OdeState(0.0, Vector(DenseVector.zeros[Double](order+1),
        DenseVector.zeros[Double](order+1), DenseVector.zeros[Double](order+1)))
    
    val basis = GllBasis(order)
    var boundary1 = system.actorOf(Props(classOf[ExternalBoundaryActor], xL, leftBc), "boundary0")
    for (i <- 1 to nElems-1) {
      val x = xL + i*width
      val boundary2 = system.actorOf(Props(classOf[InternalBoundaryActor], x), s"boundary$i")
      val elem = system.actorOf(Props(classOf[GllElement], basis, width, controller, boundary1, boundary2, pde), s"interval${i-1}")
      if (i == 2) {
        val myPsi = DenseVector.zeros[Double](order+1)
        myPsi(3) = 0.5; myPsi(4) = 1.0; myPsi(5) = 0.5
        val myPi = DenseVector.zeros[Double](order+1)
        val myPhi = basis.differentiate(myPsi)
        val myId = OdeState(0.0, Vector(myPsi, myPi, myPhi))
        elem ! GllElement.InitialData(myId)
      } else elem ! GllElement.InitialData(id)
      boundary1 = boundary2
    }
    val boundary2 = system.actorOf(Props(classOf[ExternalBoundaryActor], xR, rightBc), s"boundary$nElems")
    val elem = system.actorOf(Props(classOf[GllElement], basis, width, controller, boundary1, boundary2, pde), s"interval${nElems-1}")
    elem ! GllElement.InitialData(id)
  }
}