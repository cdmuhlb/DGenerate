package edu.cornell.cdm89.scalaspec.pde

import scala.concurrent.{ExecutionContext, Future}

import edu.cornell.cdm89.scalaspec.ode.{OdeState, FieldVec, PointVec}

trait FluxConservativePde {
  def flux(state: OdeState)(implicit executor: ExecutionContext): Future[FieldVec]
  def source(state: OdeState)(implicit executor: ExecutionContext): Future[FieldVec]
  def maxLambda(t: Double, u: PointVec): Double
}

