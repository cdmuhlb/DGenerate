import akka.actor.{ActorSystem, Props}
import akka.actor.AddressFromURIString
import akka.cluster.Cluster
import akka.japi.Util.immutableSeq
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory

import edu.cornell.cdm89.scalaspec.domain.{DomainInfo, Subdomain}
import edu.cornell.cdm89.scalaspec.domain.{ContiguousGrid, RoundRobinGrid}
import edu.cornell.cdm89.scalaspec.driver.{EvolutionController, RunConfiguration}
import edu.cornell.cdm89.scalaspec.driver.{ScalarAdvectionConfig, ScalarWaveConfig, Hesthaven53Config}

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
  val seedNodes = immutableSeq(config.getStringList(
      "harvest.cluster.seed-nodes")).map {
      case AddressFromURIString(addr) => addr }.toVector
  val nNodes = config.getInt("akka.cluster.role.compute.min-nr-of-members")
  val nodeId = Cluster(system).selfAddress.port.get - 2552 // Hack
  require((nodeId >= 0) && (nodeId < nNodes))

  val runConf = new ScalarAdvectionConfig(2.0*math.Pi, config)
  //val runConf = new Hesthaven53(config)
  //val runConf = new ScalarWaveConfig(config)
  val pde = runConf.pde
  val leftBc = runConf.leftBc
  val rightBc = runConf.rightBc
  val domInfo = runConf.domainInfo

  // Create domain
  val grid = new ContiguousGrid(domInfo, nNodes, leftBc, rightBc)
  //val grid = new RoundRobinGrid(domInfo, nNodes, leftBc, rightBc)
  val subdomain = system.actorOf(Props(classOf[Subdomain], grid,
      pde, nodeId), "subdomain")

  // Establish initial data
  val idActor = runConf.createIdActor(system, subdomain)

  // Create observer
  val obsActor = runConf.createObsActor(system, subdomain)

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
