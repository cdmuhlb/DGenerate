import akka.actor.{ActorSystem, Props}
import akka.actor.AddressFromURIString
import akka.actor.Inbox
import akka.cluster.Cluster
import akka.routing.FromConfig
import akka.japi.Util.immutableSeq
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory

import edu.cornell.cdm89.scalaspec.domain.{DomainInfo, Subdomain, GllElement}
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
  
  val order = config.getInt("harvest.order-of-elements")
  val nElems = config.getInt("harvest.nr-of-elements")
  val seedNodes = immutableSeq(config.getStringList(
      "harvest.cluster.seed-nodes")).map {
      case AddressFromURIString(addr) => addr }.toVector
  val t0 = config.getDouble("harvest.initial-time")
  val dt = config.getDouble("harvest.step-size")
  val nSteps = config.getInt("harvest.nr-of-steps")
  val obsFreq = config.getInt("harvest.steps-per-obs")
  val doObserve = config.getBoolean("harvest.observe-solution")
  val nNodes = config.getInt("akka.cluster.role.compute.min-nr-of-members")

  val domInfo = DomainInfo(0.0, 10.0, order, nElems)
  val subdomain = system.actorOf(Props(classOf[Subdomain], domInfo,
      //new ScalarAdvectionEquation(2.0*math.Pi)),
      new ScalarWaveEquation),
      "subdomain")

  //val idActor = system.actorOf(Props[SineWaveInitialData], "idProvider")
  val idActor = system.actorOf(Props(classOf[TrianglePulseInitialData],
      subdomain, 5.0, 0.5, 1.0), "idProvider")
  
  val domRouter = system.actorOf(Props.empty.withRouter(FromConfig), "domain")
  val idRouter = system.actorOf(Props.empty.withRouter(FromConfig), "initialData")

  println(s"Joining seed notes $seedNodes")
  Cluster(system).joinSeedNodes(seedNodes)

  Cluster(system) registerOnMemberUp {
    println(s"Cluster is UP")
    val inbox = Inbox.create(system)
    
    def waitForResponses(response: Any): Unit = {
      var count = 0
      while (count < nNodes) {
        inbox.receive(10.seconds) match {
          case `response` =>
            count += 1
            //println("Tick")
          case msg =>
            println("Unexpected message: $msg ; expected: $response")
            system.shutdown()
        }
      }
      //println("In sync")
    }
    
    inbox.send(domRouter, 'CreateElements)
    waitForResponses('ElementsCreated)

    inbox.send(idRouter, 'ProvideId)
    waitForResponses('AllReady)
    
    // Observe at t0
    if (doObserve) {
      println("Observing t0")
      inbox.send(domRouter, GllElement.Interpolate(t0))
      waitForResponses('AllObserved)
    }
  
    println("Starting evolution")
    val startTime = System.nanoTime
    for (i <- 1 to nSteps) {
      // Step
      val ti = dt*i
      inbox.send(domRouter, GllElement.StepTo(ti))
      waitForResponses('AllAdvanced)

      // Observe
      if (doObserve) {
        if (i%obsFreq == 0) {
          inbox.send(domRouter, GllElement.Interpolate(ti))
          waitForResponses('AllObserved)
        }
      }

    }
    val stopTime = System.nanoTime
    val runtime = 1.0e-9 * (stopTime - startTime)
    println("Finishing evolution")
    println(f"Computation time: $runtime%.3f s")

    system.shutdown()
  }
}
