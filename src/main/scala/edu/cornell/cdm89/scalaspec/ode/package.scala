package edu.cornell.cdm89.scalaspec

import breeze.linalg.DenseVector

package object ode {
  type FieldVec = IndexedSeq[DenseVector[Double]]
  type PointVec = IndexedSeq[Double]
}
