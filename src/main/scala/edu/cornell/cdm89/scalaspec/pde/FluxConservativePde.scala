package edu.cornell.cdm89.scalaspec.pde

import scala.concurrent.{ExecutionContext, Future}

import edu.cornell.cdm89.scalaspec.ode.{OdeState, FieldVec}

trait FluxConservativePde {
  def flux(u: OdeState)(implicit executor: ExecutionContext): Future[FieldVec]
  def source(u: OdeState)(implicit executor: ExecutionContext): Future[FieldVec]
}

