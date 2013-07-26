package edu.cornell.cdm89.scalaspec.driver

import java.io._

import scala.collection.immutable.SortedMap
import scala.collection.mutable
import akka.actor.{Actor, ActorRef, ActorLogging}
import breeze.linalg.{DenseMatrix, DenseVector}

import edu.cornell.cdm89.scalaspec.domain.{AffineMap, GllElement, Subdomain}
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

class YgraphInterpObserver(dt: Double, dx: Double, nodeId: Int) extends Actor with ActorLogging {
  val interpolators = mutable.Map.empty[(Int, Int), DenseMatrix[Double]]
  val subdomain = context.parent

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
    case 'ElementsReady =>
      subdomain ! 'GetLocalElements
    case Subdomain.ElementsList(elements) =>
      context.become(active(new InterpolatedDataHelper(elements.length, nodeId)))
    case GllElement.StateChanged(name, x, chunk) =>
      log.error("Too early!")
  }

  def active(helper: InterpolatedDataHelper): Receive = {
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
          val coords = interpCoords map map.mapX
          helper.put(t, coords, u0, sender)
        }
      }
  }
}

class InterpolatedDataHelper(nElements: Int, nodeId: Int) {
  val times = mutable.Map.empty[Double, (mutable.Set[ActorRef], mutable.Map[Double, Double])]

  def put(time: Double, coords: DenseVector[Double], u0: DenseVector[Double], element: ActorRef) = {
    assert(coords.length == u0.length)
    if (!times.contains(time)) {
      times(time) = (mutable.Set.empty[ActorRef], mutable.Map.empty[Double, Double])
    }
    val (elements, data) = times(time)
    elements.add(element)
    for (i <- 0 until coords.length) {
      if (data.contains(coords(i))) {
        // Assume at most two co-located values
        data(coords(i)) = 0.5*(u0(i) + data(coords(i)))
      } else data(coords(i)) = u0(i)
    }
    if (elements.size == nElements) {
      // Write
      val filename = s"/tmp/harvest/InterpOutput_$nodeId.yg"
      val out = new PrintWriter(new FileWriter(filename, true))
      out.println(f""""Time = ${time}%.6f""")
      for ((k, v) <- data.toList.sorted) {
        out.println(f"$k%.6f    $v%.6f")
      }
      out.println
      out.close()
      times.remove(time)
    }
  }
}
