package edu.cornell.cdm89.scalaspec.pde

import edu.cornell.cdm89.scalaspec.ode.PointVec

object LaxFriedrichsFlux {
  case class BoundaryValues(t: Double, u: PointVec, flux: PointVec,
      maxLambda: Double, norm: Double)
  case class NumericalFlux(t: Double, fStar: PointVec)
  
  def flux(bv1: BoundaryValues, bv2: BoundaryValues): NumericalFlux = {
    val c = bv1.maxLambda.max(bv2.maxLambda)
    assert(bv1.t == bv2.t)
    val fStar = bv1.u.zip(bv1.flux.zip(bv2.u.zip(bv2.flux))) map {
      case (u1, (f1, (u2, f2))) =>
        0.5*(f1 + f2 + c*(bv1.norm*u1 + bv2.norm*u2))
    }
    NumericalFlux(bv1.t, fStar)
  }
}
