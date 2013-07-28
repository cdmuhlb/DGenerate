package edu.cornell.cdm89.scalaspec.pde

import scala.concurrent.{ExecutionContext, Future, future}
import breeze.linalg.DenseVector

import edu.cornell.cdm89.scalaspec.ode.{ElementState, PointState, FieldVec, PointVec}

class ScalarAdvectionEquation(a: Double) extends FluxConservativePde {
  def flux(state: ElementState)(implicit executor: ExecutionContext): Future[FieldVec] = future {
    Vector(state.u(0) :* a)
  }

  def source(state: ElementState)(implicit executor: ExecutionContext): Future[FieldVec] = future {
    Vector(DenseVector.zeros[Double](state.u(0).length))
  }

  def maxLambda(state: PointState): Double = a
}

class Hesthaven53Equation extends FluxConservativePde {
  def flux(state: ElementState)(implicit executor: ExecutionContext): Future[FieldVec] = future {
    Vector(state.u(0) :* (state.x map a))
  }

  def source(state: ElementState)(implicit executor: ExecutionContext): Future[FieldVec] = future {
    Vector(DenseVector.zeros[Double](state.u(0).length))
  }

  def maxLambda(state: PointState): Double = a(state.x)

  private def a(x: Double) = math.pow((1.0 - x*x), 5) + 1.0
}
