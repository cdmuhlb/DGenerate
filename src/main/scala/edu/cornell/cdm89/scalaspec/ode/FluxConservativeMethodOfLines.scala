package edu.cornell.cdm89.scalaspec.ode

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

import edu.cornell.cdm89.scalaspec.pde.FluxConservativePde
import edu.cornell.cdm89.scalaspec.pde.LaxFriedrichsFlux.{BoundaryValues, NumericalFlux}
import edu.cornell.cdm89.scalaspec.spectral.GllBasis

class FluxConservativeMethodOfLines(pde: FluxConservativePde, basis: GllBasis,
    jacobian: Double, leftBoundary: ActorRef, rightBoundary: ActorRef) extends Ode {
  val weights = basis.weights

  override def rhs(state: OdeState)(implicit executor: ExecutionContext): Future[FieldVec] = {
    val myFlux = pde.flux(state)
    val mySource = pde.source(state)
    val futBvL = onLeftBoundary(state, myFlux)
    val futBvR = onRightBoundary(state, myFlux)

    // How long to wait for neighboring domains to respond
    implicit val timeout = Timeout(1.minute)
    val leftFlux = futBvL map { bv => (leftBoundary ? bv).mapTo[NumericalFlux] }
    val rightFlux = futBvR map { bv => (rightBoundary ? bv).mapTo[NumericalFlux] }

    // Math on futures
    for {
      bulkFlux <- myFlux
      bulkSource <- mySource
      bvL <- futBvL
      bvR <- futBvR
      futFStarL <- leftFlux; fStarL <- futFStarL
      futFStarR <- rightFlux; fStarR <- futFStarR
    } yield {
      val fL = bvL.flux
      val fR  = bvR.flux
      val ans = bulkFlux.zip(bulkSource) map { case (fi, gi) =>
        (basis.differentiate(fi) :* (-1.0/jacobian)) + gi
      }

      for (i <- ans.indices) {
        //println(s"dFL($i) = ${(fL(i) - fStarL.fStar(i)) / (jacobian * weights(0))}")
        //println(s"dFR($i) = ${(fR(i) - fStarR.fStar(i)) / (jacobian * weights(basis.order))}")
        ans(i)(0) -= (fL(i) - fStarL.fStar(i)) / (jacobian * weights(0))
        ans(i)(basis.order) += (fR(i) - fStarR.fStar(i)) / (jacobian * weights(basis.order))
      }
      ans
    }
  }

  private def onLeftBoundary(state: OdeState,
      flux: Future[FieldVec])(implicit executor: ExecutionContext): Future[BoundaryValues] = {
    flux map { f =>
      val bu = state.u.map { ui => ui(0) }
      val bf = f.map{ fi => fi(0) }
      BoundaryValues(state.t, bu, bf, -1.0)
    }
  }

  private def onRightBoundary(state: OdeState,
      flux: Future[FieldVec])(implicit executor: ExecutionContext): Future[BoundaryValues] = {
    flux map { f =>
      val bu = state.u.map { ui => ui(ui.length-1) }
      val bf = f.map{ fi => fi(fi.length-1) }
      BoundaryValues(state.t, bu, bf, 1.0)
    }
  }
}
