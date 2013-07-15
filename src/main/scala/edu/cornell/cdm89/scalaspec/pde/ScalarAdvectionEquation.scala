package edu.cornell.cdm89.scalaspec.pde

import scala.concurrent.{ExecutionContext, Future, future}
import breeze.linalg.DenseVector

import edu.cornell.cdm89.scalaspec.ode.{OdeState, FieldVec}

class ScalarAdvectionEquation(a: Double) extends FluxConservativePde {
  override def flux(u: OdeState)(implicit executor: ExecutionContext): Future[FieldVec] = future {
    Vector(u.u(0) :* a)
  }

  override def source(u: OdeState)(implicit executor: ExecutionContext): Future[FieldVec] = future {
    Vector(DenseVector.zeros[Double](u.u(0).length))
  }
}
