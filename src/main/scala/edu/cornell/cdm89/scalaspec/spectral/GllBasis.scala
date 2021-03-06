package edu.cornell.cdm89.scalaspec.spectral

import breeze.linalg._

import edu.cornell.cdm89.scalaspec.math.{JacobiPolynomials, LegendrePolynomials}

case class GllBasis(order: Int) {
  // nodes.length == order+1
  def nodes = myNodes.copy
  
  // weights.length == order+1
  def weights = myWeights.copy
  
  //def d1Matrix = myD1.copy
  def differentiate(u: DenseVector[Double]) = myD1*u

  def spectralCoefficients(u: DenseVector[Double]) = mySpectralTransform*u
  def sumCoefficients(spec: DenseVector[Double]) = mySpectralTransform \ spec

  def interpolationMatrix(x: DenseVector[Double]):DenseMatrix[Double] = {
    val ans = DenseMatrix.zeros[Double](x.length, order+1)
    for (row <- 0 until x.length) {
      assert((x(row) >= -1.0) && (x(row) <= 1.0))
      // from SpEC:Utils/Math/InterpolationWeights.cpp
      var c1 = 1.0
      var c4 = myNodes(0) - x(row)
      ans(row, 0) = 1.0
      for (i <- 1 to order) {
        var c2 = 1.0
        val c5 = c4
        c4 = myNodes(i) - x(row)
        for (j <- 0 until i) {
          val c3 = myNodes(i) - myNodes(j)
          c2 *= c3
          if (j == i-1) ans(row, i) = -c1*c5*ans(row, i-1)/c2
          ans(row, j) = c4*ans(row, j)/c3
        }
        c1 = c2
      }
    }
    ans
  }
  
  private lazy val myNodes = {
    val ans = DenseVector.zeros[Double](order+1)

    // Compute eigenvalues of the Golub-Welsch matrix
    val lambdas = eigSym(GolubWelschMatrix11, false)._1.toArray
    java.util.Arrays.sort(lambdas)
    ans.slice(1, ans.length-1) := DenseVector(lambdas)

    // Polish roots with one Newton-Raphson iteration
    val newtonIt = breeze.generic.UFunc{(x: Double) =>
        JacobiPolynomials.p11(order-1, x) / JacobiPolynomials.d1p11(order-1, x)}
    newtonIt.inPlace(lambdas)

    // Set boundary points
    ans(0) = -1.0
    ans(order) = 1.0

    ans
  }

  private lazy val myWeights = nodes map {x =>
    val n = order.toDouble
    val pni = JacobiPolynomials.p00(order, x)

    2.0 / (n*(n + 1.0)*pni*pni)
  }

  private lazy val myD1 = {
    val n = order + 1;
    val c = myNodes map {x =>
      var ci = 1.0
      for (j <- 0 until n if x != myNodes(j)) ci *= x - myNodes(j)
      ci
    }

    val ans = DenseMatrix.zeros[Double](n, n)

    // Compute off-diagonal elements
    // TODO: Double-check row/column convention
    for (j <- 0 until n) {
      val xj = myNodes(j)
      val cj = c(j)
      for (i <- j+1 until n) {
        val dx = myNodes(i) - xj
        ans(i, j) = c(i)/(cj*dx)
        ans(j, i) = -cj/(c(i)*dx)
      }
    }
  
    // Use negative sum trick for diagonal elements
    // TODO: Improve accuracy of sum
    for (i <- 0 until n) {
      var sum = 0.0;
      for (j <- 0 until i) {
        sum += ans(i, j)
      }
      for (j <- i+1 until n) {
        sum += ans(i, j);
      }
      ans(i, i) = -sum
    }

    ans
  }

  private lazy val mySpectralTransform = {
    val n = order + 1
    val ans = DenseMatrix.zeros[Double](n, n)
    for (i <- 0 until n; j <- 0 until n) {
      ans(i, j) = myWeights(j)*LegendrePolynomials.p(i, myNodes(j))
    }
    ans
  }
  
  private def GolubWelschMatrix11 = {
    // TODO: Optimize for symmetric tridiagnoal
    val ans = DenseMatrix.zeros[Double](order-1, order-1)
    
    // Compute subdiagonal elements (diagonal elements are zero)
    for (i <- 0 until order-2) {
      val n = i.toDouble
      val elem = Math.sqrt((n + 1.0)*(n + 3.0) / ((2.0*n + 3.0)*(2.0*n + 5.0)))
      ans(i, i+1) = elem
      ans(i+1, i) = elem
    }
    ans
  }
}
