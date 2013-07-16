package edu.cornell.cdm89.scalaspec.pde

import akka.actor.{Actor, ActorLogging}
import breeze.linalg.DenseVector

import edu.cornell.cdm89.scalaspec.domain.GllElement.{Coords, InitialData}
import edu.cornell.cdm89.scalaspec.ode.OdeState
import edu.cornell.cdm89.scalaspec.spectral.GllBasis

class TrianglePulseInitialData(center: Double, halfWidth: Double,
    height: Double) extends Actor with ActorLogging {
  override def preStart = {
    val elements = context.actorSelection("/user/domain0/interval*")
    elements ! 'GetCoords
  }
  
  def receive = {
    case Coords(xs) =>
      val t0 = 0.0
      val width = xs(xs.length-1) - xs(0)
      val basis = GllBasis(xs.length-1)
      val psi0 = xs map { x =>
        if ((x < center-halfWidth) || (x > center+halfWidth)) 0.0
        else if (x < center) (height/halfWidth) * (halfWidth - (center - x))
        else (height/halfWidth) * (halfWidth - (x - center))
      }
      val pi0 = DenseVector.zeros[Double](xs.length)
      val phi0 = basis.differentiate(psi0) :* (2.0/width)
      sender ! InitialData(OdeState(t0, Vector(psi0, pi0, phi0)))
  }
}
