package edu.cornell.cdm89.scalaspec.pde

import LaxFriedrichsFlux.BoundaryValues

class WaveOutflowBoundary(norm: Double)
    extends BoundaryCondition {
  def boundaryValues(t: Double, x: Double, bvIn: BoundaryValues) = {
    val char0 = 0.0 // pi + norm*phi
    val char1 = bvIn.u(1) - norm*bvIn.u(2)
    val charPi = 0.5*(char0 + char1)
    val charPhi = norm*0.5*(char0 - char1)
    BoundaryValues(t, Vector(bvIn.u(0), charPi, charPhi),
        Vector(bvIn.flux(0), charPhi, charPi), 1.0, norm)
  }
}

class WaveInversionBoundary(norm: Double)
    extends BoundaryCondition {
  def boundaryValues(t: Double, x: Double, bvIn: BoundaryValues) = {
    val char1 = bvIn.u(1) - norm*bvIn.u(2)
    val char0 = -char1
    val charPi = 0.5*(char0 + char1)
    val charPhi = norm*0.5*(char0 - char1)
    BoundaryValues(t, Vector(bvIn.u(0), charPi, charPhi),
        Vector(bvIn.flux(0), charPhi, charPi), 1.0, norm)
  }
}

class WaveReflectionBoundary(norm: Double)
    extends BoundaryCondition {
  def boundaryValues(t: Double, x: Double, bvIn: BoundaryValues) = {
    val char1 = bvIn.u(1) - norm*bvIn.u(2)
    val char0 = char1
    val charPi = 0.5*(char0 + char1)
    val charPhi = norm*0.5*(char0 - char1)
    BoundaryValues(t, Vector(bvIn.u(0), charPi, charPhi),
        Vector(bvIn.flux(0), charPhi, charPi), 1.0, norm)
  }
}
