import akka.actor.{ActorSystem, Props}
import akka.actor.Inbox
import akka.cluster.Cluster
import akka.routing.FromConfig
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory

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

    val order = config.getInt("harvest.order-of-elements")
    val nElems = config.getInt("harvest.nr-of-elements")
    val t0 = config.getDouble("harvest.initial-time")
    val dt = config.getDouble("harvest.step-size")
    val nSteps = config.getInt("harvest.nr-of-steps")
    val obsFreq = config.getInt("harvest.steps-per-obs")
    val doObserve = config.getBoolean("harvest.observe-solution")
  
    val domInfo = DomainInfo(0.0, 10.0, order, nElems)
    val domain = system.actorOf(Props(classOf[DomainSubset], domInfo,
        //new ScalarAdvectionEquation(2.0*math.Pi)),
        new ScalarWaveEquation,
        inbox.getRef), "domain0")
    inbox.receive(10.seconds) match {
      // Don't send ID until domain is ready for its actors' messages
      case 'DomainInitializing =>  println("Node1 is ready for ID")
    }
    val domRouter = system.actorOf(Props.empty.withRouter(FromConfig), "domRouter")
    
    //val idActor = system.actorOf(Props[SineWaveInitialData], "idProvider")
    val idActor = system.actorOf(Props(classOf[TrianglePulseInitialData], 5.0, 0.5, 1.0), "idProvider")
    inbox.receive(10.seconds) match {
      case 'AllReady => println("All ready!")
      case msg =>
        println("Unexpected message: " + msg)
        system.shutdown
    }
    // Wait for AllReady from slave node
    Thread.sleep(2000)
    
    // Observe at t0
    if (doObserve) {
      println("Observing t0")
      inbox.send(domRouter, GllElement.Interpolate(t0))
      inbox.receive(10.seconds) match {
        case 'AllObserved => //println("Observation1")
      }
      inbox.receive(10.seconds) match {
        case 'AllObserved => //println("Observation2")
      }
    }
  
    println("Starting evolution")
    val startTime = System.nanoTime
    for (i <- 1 to nSteps) {
      // Step
      val ti = dt*i
      inbox.send(domRouter, GllElement.StepTo(ti))
      inbox.receive(10.seconds) match {
        case 'AllAdvanced => //println(s"All advanced to $ti!")
        case msg =>
          println("Unexpected message: " + msg)
          system.shutdown
      }
      inbox.receive(10.seconds) match {
        case 'AllAdvanced => //println("AllAdvanced2")
      }

      // Observe
      if (doObserve) {
        if (i%obsFreq == 0) {
          inbox.send(domRouter, GllElement.Interpolate(ti))
          inbox.receive(10.seconds) match {
            case 'AllObserved => //println(s"All observed at $ti!")
            case msg =>
              println("Unexpected message: " + msg)
              system.shutdown
          }
          inbox.receive(10.seconds) match {
            case 'AllObserved => //println("Observation2")
          }
        }
      }

    }
    val stopTime = System.nanoTime
    val runtime = 1.0e-9 * (stopTime - startTime)
    println("Finishing evolution")
    println(f"dt = $runtime%.3f")

    system.shutdown()
  }
}
