package edu.cornell.cdm89.scalaspec.domain

import edu.cornell.cdm89.scalaspec.pde.BoundaryCondition

case class Boundary(x: Double, index: Int)
case class Element(order: Int, xL: Double, xR: Double, index: Int)

trait GridDistribution {
  def myExternalBoundaries(nodeId: Int): Seq[(Boundary, BoundaryCondition)]
  def myInternalBoundaries(nodeId: Int): Seq[Boundary]
  def myElements(nodeId: Int): Seq[Element]
}
