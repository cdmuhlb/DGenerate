import akka.actor.{ActorSystem, Props}
import com.typesafe.config.ConfigFactory

object SlaveNode extends App {
  // // Override the configuration of the port when specified as program argument,
  // and set the role to "backend"
  val config = (
      if (args.nonEmpty) ConfigFactory.parseString(s"akka.remote.netty.tcp.port=${args(0)}")
      else ConfigFactory.empty
    ).withFallback(ConfigFactory.parseString("akka.cluster.roles = [compute]")
    ).withFallback(ConfigFactory.load())

  val system = ActorSystem("Harvest", config)
}
