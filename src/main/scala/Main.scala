import akka.actor.{ActorSystem, Props}
import akka.actor.AddressFromURIString
import akka.actor.Inbox
import akka.cluster.Cluster
import akka.routing.FromConfig
import akka.japi.Util.immutableSeq
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory

import edu.cornell.cdm89.scalaspec.domain.{DomainInfo, Subdomain, GllElement}
import edu.cornell.cdm89.scalaspec.domain.{ContiguousGrid, RoundRobinGrid}
import edu.cornell.cdm89.scalaspec.pde.BoundaryCondition
import edu.cornell.cdm89.scalaspec.pde.LaxFriedrichsFlux.BoundaryValues
import edu.cornell.cdm89.scalaspec.ode.OdeState
import edu.cornell.cdm89.scalaspec.pde.{ScalarWaveEquation, ScalarAdvectionEquation}
import edu.cornell.cdm89.scalaspec.pde.{SineBoundary, AdvectionOutflowBoundary, AdvectionConstantBoundary}
import edu.cornell.cdm89.scalaspec.pde.{WaveOutflowBoundary, WaveInversionBoundary, WaveReflectionBoundary}
import edu.cornell.cdm89.scalaspec.pde.{SineWaveInitialData, TrianglePulseInitialData}
import edu.cornell.cdm89.scalaspec.driver.EvolutionController

object Main extends App {
  val config = {
    if (args.length != 2) {
      Console.err.println("Usage: Main <hostname> <port>")
      System.exit(1)
    }
    val hostname = s""""${args(0)}""""
    val port = args(1)
    ConfigFactory.parseString(s"akka.remote.netty.tcp.hostname=$hostname").withFallback(
        ConfigFactory.parseString(s"akka.remote.netty.tcp.port=$port").withFallback(
        ConfigFactory.parseString("akka.cluster.roles = [compute]").withFallback(
        ConfigFactory.load())))
  }
  val system = ActorSystem("Harvest", config)

  // Load configuration
  val order = config.getInt("harvest.order-of-elements")
  val nElems = config.getInt("harvest.nr-of-elements")
  val seedNodes = immutableSeq(config.getStringList(
      "harvest.cluster.seed-nodes")).map {
      case AddressFromURIString(addr) => addr }.toVector
  //val t0 = config.getDouble("harvest.initial-time")
  //val dt = config.getDouble("harvest.step-size")
  //val nSteps = config.getInt("harvest.nr-of-steps")
  val obsFreq = config.getInt("harvest.steps-per-obs")
  val doObserve = config.getBoolean("harvest.observe-solution")
  val nNodes = config.getInt("akka.cluster.role.compute.min-nr-of-members")
  val nodeId = Cluster(system).selfAddress.port.get - 2552 // Hack
  require((nodeId >= 0) && (nodeId < nNodes))

  // PDE
  //val a = 2.0*math.Pi
  //val pde = new ScalarAdvectionEquation(a)
  val pde = new ScalarWaveEquation

  // Boundary conditions
  //val leftBc = new SineBoundary(a, 1.0)
  //val rightBc = new OutflowBoundary(-1.0)
  val leftBc = new WaveOutflowBoundary(1.0)
  val rightBc = new WaveOutflowBoundary(-1.0)

  // Create domain
  val domInfo = DomainInfo(0.0, 10.0, order, nElems)
  //val grid = new ContiguousGrid(domInfo, nNodes, leftBc, rightBc)
  val grid = new RoundRobinGrid(domInfo, nNodes, leftBc, rightBc)
  val subdomain = system.actorOf(Props(classOf[Subdomain], grid,
      pde, nodeId), "subdomain")

  // Establish initial data
  //val idActor = system.actorOf(Props(classOf[SineWaveInitialData], subdomain),
  //    "idProvider")
  val idActor = system.actorOf(Props(classOf[TrianglePulseInitialData],
      subdomain, 5.0, 0.5, 1.0), "idProvider")

  Cluster(system).joinSeedNodes(seedNodes)

  if (nodeId == 0) {
//    val domRouter = system.actorOf(Props.empty.withRouter(FromConfig), "domain")
//    val idRouter = system.actorOf(Props.empty.withRouter(FromConfig), "initialData")

    Cluster(system).registerOnMemberUp {
      println(s"Cluster is UP")
/*
      val inbox = Inbox.create(system)

      def waitForResponses(response: Any): Unit = {
        var count = 0
        while (count < nNodes) {
          inbox.receive(2.minutes) match {
            case `response` =>
              count += 1
            case msg =>
              println("Unexpected message: $msg ; expected: $response")
              system.shutdown()
          }
        }
      }

      inbox.send(domRouter, GllElement.CreateElements(domRouter))
      waitForResponses('ElementsCreated)

      inbox.send(idRouter, 'ProvideId)
      waitForResponses('AllReady)

*/
      val control = system.actorOf(Props(classOf[EvolutionController], nNodes), "driver")
      control ! 'StartEvolution

      /* println("Warming up")
      for (i <- 1 to 500) {
        val ti = dt*i
        inbox.send(domRouter, GllElement.StepTo(ti))
        waitForResponses('AllAdvanced)
      }
      System.gc() */
/*
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
*/
    }
  }

}
