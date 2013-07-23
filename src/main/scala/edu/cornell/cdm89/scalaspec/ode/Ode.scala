package edu.cornell.cdm89.scalaspec.ode

import scala.concurrent.{ExecutionContext, Future}
import breeze.linalg.DenseVector

case class OdeState(t: Double, u: FieldVec)

case class InitialData(state: OdeState)

trait Ode {
  def rhs(state: OdeState)(implicit executor: ExecutionContext): Future[FieldVec]
}
