import akka.actor.{ActorSystem, Props}
import akka.actor.Inbox
import akka.cluster.Cluster
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory

import edu.cornell.cdm89.scalaspec.domain.{DomainInfo, DomainSubset}
import edu.cornell.cdm89.scalaspec.pde.{ScalarWaveEquation, ScalarAdvectionEquation}
import edu.cornell.cdm89.scalaspec.pde.{SineWaveInitialData, TrianglePulseInitialData}

object SlaveNode extends App {
  // // Override the configuration of the port when specified as program argument,
  // and set the role to "backend"
  val config = (
      if (args.nonEmpty) ConfigFactory.parseString(s"akka.remote.netty.tcp.port=${args(0)}")
      else ConfigFactory.empty
    ).withFallback(ConfigFactory.parseString("akka.cluster.roles = [compute]")
    ).withFallback(ConfigFactory.load())

  val system = ActorSystem("Harvest", config)
  
  Cluster(system) registerOnMemberUp {
    val inbox = Inbox.create(system)
    
    val order = 8
    val nElems = 100
  
    val domInfo = DomainInfo(0.0, 10.0, order, nElems)
    val domain = system.actorOf(Props(classOf[DomainSubset], domInfo,
        //new ScalarAdvectionEquation(2.0*math.Pi)),
        new ScalarWaveEquation,
        inbox.getRef), "domain0")
    inbox.receive(10.seconds) match {
      // Don't send ID until domain is ready for its actors' messages
      case 'DomainInitializing => println("Node2 is ready for ID")
    }
    
    val idActor = system.actorOf(Props(classOf[TrianglePulseInitialData], 5.0, 0.5, 1.0), "idProvider")
    inbox.receive(10.seconds) match {
      case 'AllReady => println("All ready!")
    }
    
    //Thread.sleep(2.seconds.toMillis)
    //system.shutdown()
  }
}
