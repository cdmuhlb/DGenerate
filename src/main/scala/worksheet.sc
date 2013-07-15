import edu.cornell.cdm89.scalaspec._
import breeze.linalg.{DenseMatrix, DenseVector}

object worksheet {
  val order = 8                                   //> order  : Int = 8
  val n = order + 1                               //> n  : Int = 9
  val basis = spectral.GllBasis(order)            //> basis  : edu.cornell.cdm89.scalaspec.spectral.GllBasis = GllBasis(8)
  val nodes = basis.nodes                         //> nodes  : breeze.linalg.DenseVector[Double] = DenseVector(-1.0, -0.8997579954
                                                  //| 114598, -0.6771862795107376, -0.36311746382617816, -2.684465425554698E-16, 0
                                                  //| .3631174638261783, 0.6771862795107372, 0.8997579954114601, 1.0)
  val u = DenseVector.ones[Double](n)             //> u  : breeze.linalg.DenseVector[Double] = DenseVector(1.0, 1.0, 1.0, 1.0, 1.0
                                                  //| , 1.0, 1.0, 1.0, 1.0)
  for (i <- 0 until n) u(i) = nodes(i)
  basis.differentiate(u) foreach println          //> 0.9999999999999923
                                                  //| 1.0
                                                  //| 0.9999999999999989
                                                  //| 1.000000000000001
                                                  //| 0.9999999999999992
                                                  //| 1.0000000000000009
                                                  //| 0.9999999999999988
                                                  //| 1.0000000000000009
                                                  //| 0.9999999999999964
}