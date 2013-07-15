package edu.cornell.cdm89.scalaspec.domain

class AffineMap(xL: Double, xR: Double) {
  private val width = xR - xL
  val jacobian = 0.5*width
  
  def mapX(xTopo: Double): Double = (xTopo + 1.0)*jacobian + xL
}