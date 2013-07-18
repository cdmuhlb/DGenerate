package edu.cornell.cdm89.scalaspec.pde

import LaxFriedrichsFlux.BoundaryValues

class SineBoundary(a: Double, norm: Double) extends BoundaryCondition {
  def boundaryValues(t: Double, x: Double, bvIn: BoundaryValues) = {
    val u = math.sin(x - a*t)
    BoundaryValues(t,
        Vector.fill[Double](1)(u), Vector.fill[Double](1)(a*u), a, norm)
  }
}

class AdvectionOutflowBoundary(norm: Double) extends BoundaryCondition {
  def boundaryValues(t: Double, x: Double, bvIn: BoundaryValues) = {
    BoundaryValues(t, bvIn.u, bvIn.flux, bvIn.maxLambda, norm)
  }
}

class AdvectionConstantBoundary(u: Double, a: Double, norm: Double)
    extends BoundaryCondition {
  def boundaryValues(t: Double, x: Double, bvIn: BoundaryValues) = {
    BoundaryValues(t, Vector.fill[Double](1)(u),
        Vector.fill[Double](1)(a*u), a, norm)
  }
}
