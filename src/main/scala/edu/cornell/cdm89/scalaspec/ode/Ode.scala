package edu.cornell.cdm89.scalaspec.ode

import scala.concurrent.{ExecutionContext, Future}
import breeze.linalg.DenseVector

case class ElementState(t: Double, x: DenseVector[Double], u: FieldVec)
case class PointState(t: Double, x: Double, u: PointVec)

case class InitialData(state: ElementState)

trait Ode {
  def rhs(state: ElementState)(implicit executor: ExecutionContext): Future[FieldVec]
}
