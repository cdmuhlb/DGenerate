package edu.cornell.cdm89.scalaspec.pde

import scala.concurrent.{ExecutionContext, Future}

import edu.cornell.cdm89.scalaspec.ode.{ElementState, PointState, FieldVec, PointVec}

trait FluxConservativePde {
  def flux(state: ElementState)(implicit executor: ExecutionContext): Future[FieldVec]
  def source(state: ElementState)(implicit executor: ExecutionContext): Future[FieldVec]
  def maxLambda(state: PointState): Double
}

