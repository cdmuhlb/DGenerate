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
  //val obsFreq = config.getInt("harvest.steps-per-obs")
  //val doObserve = config.getBoolean("harvest.observe-solution")
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
  val grid = new ContiguousGrid(domInfo, nNodes, leftBc, rightBc)
  //val grid = new RoundRobinGrid(domInfo, nNodes, leftBc, rightBc)
  val subdomain = system.actorOf(Props(classOf[Subdomain], grid,
      pde, nodeId), "subdomain")

  // Establish initial data
  //val idActor = system.actorOf(Props(classOf[SineWaveInitialData], subdomain),
  //    "idProvider")
  val idActor = system.actorOf(Props(classOf[TrianglePulseInitialData],
      subdomain, 5.0, 0.5, 1.0), "idProvider")

  if (nodeId == 0) {
    val control = system.actorOf(Props(classOf[EvolutionController], nNodes), "driver")
    Cluster(system).joinSeedNodes(seedNodes)

    Cluster(system).registerOnMemberUp {
      println(s"Cluster is UP")
      // Hack: Make sure broadcast routers have registered all nodes
      val gossipTime = config.getMilliseconds("akka.cluster.gossip-interval")
      Thread.sleep(3*gossipTime/2)
      control ! 'StartEvolution
    }
  } else Cluster(system).joinSeedNodes(seedNodes)
}
