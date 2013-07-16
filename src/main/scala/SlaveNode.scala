import akka.actor.{ActorSystem, Props}
import akka.actor.AddressFromURIString
import akka.cluster.Cluster
import akka.japi.Util.immutableSeq
import com.typesafe.config.ConfigFactory

import edu.cornell.cdm89.scalaspec.domain.{DomainInfo, Subdomain}
import edu.cornell.cdm89.scalaspec.pde.{ScalarWaveEquation, ScalarAdvectionEquation}
import edu.cornell.cdm89.scalaspec.pde.{SineWaveInitialData, TrianglePulseInitialData}

object SlaveNode extends App {
  // // Override the configuration of the port when specified as program argument,
  // and set the role to "compute"
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

  val domInfo = DomainInfo(0.0, 10.0, order, nElems)
  val subdomain = system.actorOf(Props(classOf[Subdomain], domInfo,
      //new ScalarAdvectionEquation(2.0*math.Pi)),
      new ScalarWaveEquation),
      "subdomain")

  //val idActor = system.actorOf(Props[SineWaveInitialData], "idProvider")
  val idActor = system.actorOf(Props(classOf[TrianglePulseInitialData],
      subdomain, 5.0, 0.5, 1.0), "idProvider")

  println(s"Joining seed notes $seedNodes")
  Cluster(system).joinSeedNodes(seedNodes)
  
  Cluster(system).registerOnMemberUp {
    println(s"Cluster is UP")
  }
}
