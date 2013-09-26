package edu.cornell.cdm89.scalaspec.math

object LegendrePolynomials {
  def p(order: Int, x: Double): Double = {
    val n = order.toDouble
    return JacobiPolynomials.p00(order, x)*math.sqrt(n + 0.5)
  }

  def d1p(order: Int, x: Double): Double = {
    val n = order.toDouble
    return JacobiPolynomials.d1p00(order, x)*Math.sqrt(n + 0.5)
  }
}
