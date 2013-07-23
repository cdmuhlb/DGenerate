package edu.cornell.cdm89.scalaspec.driver

import java.io._

import akka.actor.Actor

import edu.cornell.cdm89.scalaspec.domain.GllElement
import edu.cornell.cdm89.scalaspec.ode.OdeState
import edu.cornell.cdm89.scalaspec.ode.TimeStepper.TimeChunk

class YgraphObserver(dt: Double) extends Actor {
  def receive = {
    case GllElement.StateChanged(name, x, chunk) =>
      val t0 = chunk.lastState.t
      val t1 = chunk.currentState.t
      // Assumes positive t0 and/or dt?
      // TODO: Handle exact multiples
      val i0 = math.ceil(t0/dt).toInt
      val i1 = math.floor(t1/dt).toInt
      for (i <- i0 to i1) {
        val t = dt*i
        val state = interpolate(chunk, t)
        val filename = "/tmp/harvest/" + name + ".yg"
        val out = new PrintWriter(new FileWriter(filename, true))
        out.println(f""""Time = ${state.t}%.6f""")
        for (i <- 0 until state.u.head.length) {
          out.println(f"${x(i)}%.6f    ${state.u(0)(i)}%.6f")
        }
        out.println
        out.close()
      }
  }

  private def interpolate(chunk: TimeChunk, t: Double): OdeState = {
    require((t >= chunk.lastState.t) && (t <= chunk.currentState.t))
    if (t == chunk.lastState.t) chunk.lastState
    else if (t == chunk.currentState.t) chunk.currentState
    else {
      val h = chunk.currentState.t - chunk.lastState.t
      val s = (t - chunk.lastState.t)/h
      val u = chunk.lastState.u.zip(chunk.lastRhs.zip(
          chunk.currentState.u.zip(chunk.currentRhs))) map {
        case (y0, (f0, (y1, f1))) =>
        (y0:*(1.0 - s)) + (y1:*s) + ((((y1 - y0):*(1.0 - 2.0*s)) +
        (f0:*(h*(s-1.0))) + (f1:*(h*s))):*(s*(s-1.0)))
      }
      OdeState(t, u)
    }
  }
}
