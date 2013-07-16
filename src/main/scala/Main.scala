import akka.actor.{ActorSystem, Props}
import akka.actor.Inbox
import akka.cluster.Cluster
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._

import edu.cornell.cdm89.scalaspec.domain.{DomainInfo, DomainSubset, GllElement}
import edu.cornell.cdm89.scalaspec.ode.OdeState
import edu.cornell.cdm89.scalaspec.pde.{ScalarWaveEquation, ScalarAdvectionEquation}
import edu.cornell.cdm89.scalaspec.pde.{SineWaveInitialData, TrianglePulseInitialData}

object Main extends App {
  val config = (
      if (args.nonEmpty) ConfigFactory.parseString(s"akka.remote.netty.tcp.port=${args(0)}")
      else ConfigFactory.empty
    ).withFallback(ConfigFactory.parseString("akka.cluster.roles = [compute]")
    ).withFallback(ConfigFactory.load())
  val system = ActorSystem("Harvest", config)
  
  Cluster(system) registerOnMemberUp {
    println(s"Cluster is UP")
  
    val inbox = Inbox.create(system)

    val order = 8
    val nElems = 100
    val t0 = 0.0
    val dt = 0.001
    val nSteps = 500
    val obsFreq = 10
  
    val domInfo = DomainInfo(0.0, 10.0, order, nElems)
    val domain = system.actorOf(Props(classOf[DomainSubset], domInfo,
        //new ScalarAdvectionEquation(2.0*math.Pi)), "domain0")
        new ScalarWaveEquation), "domain0")

    inbox.send(domain, 'Initialize)
    inbox.receive(10.seconds) match {
      case 'Initializing =>
    }
    
    //val idActor = system.actorOf(Props[SineWaveInitialData], "idProvider")
    val idActor = system.actorOf(Props(classOf[TrianglePulseInitialData], 5.0, 0.5, 1.0), "idProvider")
    inbox.receive(10.seconds) match {
      case 'AllReady => //println("All ready!")
      case msg =>
        println("Unexpected message: " + msg)
        system.shutdown
    }
    
    // Observe at t0
    inbox.send(domain, GllElement.Interpolate(t0))
    inbox.receive(10.seconds) match {
      case 'AllObserved =>
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

      // Observe
      if (i%obsFreq == 0) {
        inbox.send(domain, GllElement.Interpolate(ti))
        inbox.receive(10.seconds) match {
          case 'AllObserved => //println(s"All observed at $ti!")
          case msg =>
            println("Unexpected message: " + msg)
            system.shutdown
        }
      }
    }
    val stopTime = System.nanoTime
    val runtime = 1.0e-9 * (stopTime - startTime)
    println(f"dt = $runtime%.3f")

    system.shutdown()
  }
}
