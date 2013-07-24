package edu.cornell.cdm89.scalaspec.driver

import java.io._

import akka.actor.Actor

import edu.cornell.cdm89.scalaspec.domain.GllElement
import edu.cornell.cdm89.scalaspec.ode.OdeState
import edu.cornell.cdm89.scalaspec.ode.TimeStepper.TimeChunk

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
        for (i <- 0 until state.u.head.length) {
          out.println(f"${x(i)}%.6f    ${state.u(0)(i)}%.6f")
        }
        out.println
        out.close()
      }
  }
}
