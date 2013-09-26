package edu.cornell.cdm89.scalaspec.domain

import edu.cornell.cdm89.scalaspec.pde.BoundaryCondition

class ContiguousGrid(info: DomainInfo, nNodes: Int, leftBc: BoundaryCondition,
    rightBc: BoundaryCondition) extends GridDistribution {
  private val elemsPerNode = 1 + (info.nElems - 1) / nNodes
  private val width = (info.xR - info.xL) / info.nElems

  override def myExternalBoundaries(nodeId: Int):
      Seq[(Boundary, BoundaryCondition)] = {
    var ans = List.empty[(Boundary, BoundaryCondition)]
    if (nodeId == 0) ans = (Boundary(info.xL, 0), leftBc) :: ans
    if (nodeId == nNodes-1) {
      ans = (Boundary(info.xR, info.nElems), rightBc) :: ans
    }
    ans
  }

  override def myInternalBoundaries(nodeId: Int): Seq[Boundary] = {
    val seed = if (nodeId == 0) 1 else nodeId*elemsPerNode
    val ans = (seed until ((nodeId+1)*elemsPerNode).min(info.nElems)) map { i =>
      val x = info.xL + i*width
      Boundary(x, i)
    }
    ans.toList
  }

  override def myElements(nodeId: Int): Seq[Element] = {
    val ans = (nodeId*elemsPerNode until
        ((nodeId+1)*elemsPerNode).min(info.nElems)) map { i =>
      val x = info.xL + i*width
      Element(info.order, x, x+width, i)
    }
    ans.toList
  }
}
