package edu.cornell.cdm89.scalaspec.ode

import scala.concurrent.{ExecutionContext, Future}
import breeze.linalg.DenseVector

case class OdeState(t: Double, u: FieldVec)

case class InitialData(state: OdeState)
case class TakeStep(dt: Double, y1: OdeState, k1: FieldVec)

trait Ode {
  def rhs(state: OdeState)(implicit executor: ExecutionContext): Future[FieldVec]
}
