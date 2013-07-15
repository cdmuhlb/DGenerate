import akka.actor.{ActorSystem, Props}
import akka.actor.Inbox
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._

import edu.cornell.cdm89.scalaspec.domain.{DomainSubset, GllElement}
import edu.cornell.cdm89.scalaspec.ode.OdeState
import edu.cornell.cdm89.scalaspec.pde.{ScalarWaveEquation, ScalarAdvectionEquation}

object Main extends App {
  val system = ActorSystem("Harvest")
  
  val inbox = Inbox.create(system)
  
  val nElems = 256
  val order = 16
  val dt = 0.05
  val nSteps = 250
  
  val domain = system.actorOf(Props(classOf[DomainSubset], 0.0, 20.0, nElems,
      //order, new ScalarAdvectionEquation(0.5)), "domain0")
      order, new ScalarWaveEquation), "domain0")

  inbox.send(domain, 'SendId)
  inbox.receive(10.seconds) match {
    case 'AllReady => //println("All ready!")
    case msg =>
      println("Unexpected message: " + msg)
      system.shutdown
  }
  
  val startTime = System.nanoTime
  for (i <- 1 to nSteps) {
    // Step
    val ti = dt*i
    inbox.send(domain, GllElement.StepTo(ti))
    inbox.receive(10.seconds) match {
      case 'AllAdvanced => //println(s"All advanced to $ti!")
      case msg =>
        println("Unexpected message: " + msg)
        system.shutdown
    }
    
    /* // Observe
    if (i == 1) {
      inbox.send(domain, GllElement.Interpolate(0.0))
      inbox.receive(10.seconds) match {
        case 'AllObserved =>
      }
    }

    // Observe
    inbox.send(domain, GllElement.Interpolate(ti))
    inbox.receive(10.seconds) match {
      case 'AllObserved => //println(s"All observed at $ti!")
      case msg =>
        println("Unexpected message: " + msg)
        system.shutdown
    } */
  }
  val stopTime = System.nanoTime
  val runtime = 1.0e-9 * (stopTime - startTime)
  println(f"dt = $runtime%.3f")

  system.shutdown()
}
