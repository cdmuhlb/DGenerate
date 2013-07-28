package edu.cornell.cdm89.scalaspec.pde

import scala.concurrent.{ExecutionContext, Future, future}
import breeze.linalg.DenseVector

import edu.cornell.cdm89.scalaspec.ode.{ElementState, PointState, FieldVec, PointVec}

class ScalarWaveEquation extends FluxConservativePde {
  def flux(state: ElementState)(implicit executor: ExecutionContext): Future[FieldVec] = future {
    Vector(DenseVector.zeros[Double](state.u(0).length), state.u(2), state.u(1))
  }

  def source(state: ElementState)(implicit executor: ExecutionContext): Future[FieldVec] = future {
    Vector(-state.u(1), DenseVector.zeros[Double](state.u(0).length), DenseVector.zeros[Double](state.u(0).length))
  }
  
  def maxLambda(state: PointState): Double = 1.0
}
