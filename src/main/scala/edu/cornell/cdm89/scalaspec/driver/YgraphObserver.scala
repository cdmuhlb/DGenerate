package edu.cornell.cdm89.scalaspec.driver

import java.io._

import scala.collection.mutable
import akka.actor.Actor
import breeze.linalg.{DenseMatrix, DenseVector}

import edu.cornell.cdm89.scalaspec.domain.{AffineMap, GllElement}
import edu.cornell.cdm89.scalaspec.ode.OdeState
import edu.cornell.cdm89.scalaspec.ode.TimeStepper.TimeChunk
import edu.cornell.cdm89.scalaspec.spectral.GllBasis

class YgraphObserver(dt: Double) extends Actor {
  def receive = {
    case GllElement.StateChanged(name, x, chunk) =>
      val t0 = chunk.lastTime
      val t1 = chunk.currentTime
      // Assumes positive t0 and/or dt?
      // TODO: Handle exact multiples
      val i0 = math.ceil(t0/dt).toInt
      val i1 = math.floor(t1/dt).toInt
      for (i <- i0 to i1) {
        val t = dt*i
        val state = chunk.interpolate(t)
        val filename = "/tmp/harvest/" + name + ".yg"
        val out = new PrintWriter(new FileWriter(filename, true))
        out.println(f""""Time = ${state.t}%.6f""")
        for (j <- 0 until state.u.head.length) {
          out.println(f"${x(j)}%.6f    ${state.u(0)(j)}%.6f")
        }
        out.println
        out.close()
      }
  }
}

class YgraphInterpObserver(dt: Double, dx: Double) extends Actor {
  val interpolators = mutable.Map.empty[(Int, Int), DenseMatrix[Double]]

  private def xInterp(xL: Double, xR: Double) = {
    val map = new AffineMap(xL, xR)
    val j0 = math.ceil(xL/dx).toInt
    val j1 = math.floor(xR/dx).toInt
    val nXInterp = j1 - j0 + 1
    val ans = DenseVector.zeros[Double](nXInterp)
    for (j <- j0 to j1) {
      val i = j - j0
      ans(i) = map.inverseMapX(j*dx)
    }
    ans
  }

  def receive = {
    case GllElement.StateChanged(name, x, chunk) =>
      val t0 = chunk.lastTime
      val t1 = chunk.currentTime
      // Assumes positive t0 and/or dt?
      // TODO: Handle exact multiples
      val i0 = math.ceil(t0/dt).toInt
      val i1 = math.floor(t1/dt).toInt
      for (i <- i0 to i1) {
        val t = dt*i
        val state = chunk.interpolate(t)

        val xL = x(0)
        val xR = x(x.length-1)
        val interpCoords = xInterp(xL, xR)
        val nInterp = interpCoords.length
        if (nInterp > 0) {
          val map = new AffineMap(xL, xR)
          val interp = interpolators.getOrElseUpdate(
              (x.length, nInterp), GllBasis(x.length-1).interpolationMatrix(interpCoords))
          val u0 = interp * state.u(0)
          val filename = "/tmp/harvest/" + name + ".yg"
          val out = new PrintWriter(new FileWriter(filename, true))
          out.println(f""""Time = ${state.t}%.6f""")
          for (j <- 0 until u0.length) {
            out.println(f"${map.mapX(interpCoords(j))}%.6f    ${u0(j)}%.6f")
          }
          out.println
          out.close()
        }
      }
  }
}
