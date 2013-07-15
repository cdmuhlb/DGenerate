package edu.cornell.cdm89.scalaspec.pde

import LaxFriedrichsFlux.BoundaryValues

trait BoundaryCondition {
  def boundaryValues(t: Double, x: Double): BoundaryValues
}