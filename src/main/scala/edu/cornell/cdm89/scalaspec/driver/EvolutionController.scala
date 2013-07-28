package edu.cornell.cdm89.scalaspec.driver

import akka.actor.{Actor, ActorLogging, Props}
import akka.actor.{ActorIdentity, Identify}
import akka.routing.FromConfig

import edu.cornell.cdm89.scalaspec.domain.{Subdomain, GllElement}
import edu.cornell.cdm89.scalaspec.ode.TimestepController

class EvolutionController(nNodes: Int) extends Actor with ActorLogging {
  // TODO: Can some of this be moved to preStart?  Should some be passed in
  //   instead?
  val config = context.system.settings.config
  val t0 = config.getDouble("harvest.initial-time")
  val dt = config.getDouble("harvest.step-size")
  val nSteps = config.getInt("harvest.nr-of-steps")

  // Create broadcast routers
  val domRouter = context.system.actorOf(Props.empty.withRouter(FromConfig), "domain")
  val idRouter = context.system.actorOf(Props.empty.withRouter(FromConfig), "initialData")
  val obsRouter = context.system.actorOf(Props.empty.withRouter(FromConfig), "observers")

  override def receive = standby

  def standby: Receive = {
    case 'StartEvolution =>
      domRouter ! GllElement.CreateElements(domRouter)
      context.become(creatingElements)
  }

  def creatingElements: Receive = {
    waitForResponses(0, 'ElementsCreated, () => {
      idRouter ! 'ProvideId
      obsRouter ! 'Initialize
    }, loadingId)
  }

  def loadingId: Receive = {
    waitForResponses(0, 'AllReady, () => {
        log.info("Starting evolution")
        context.actorOf(Props(classOf[TimestepController], domRouter,
            t0, dt, t0+dt*nSteps), "timeStepper")
      }, stepping(System.nanoTime))
  }

  def stepping(timestamp: Long): Receive = {
    waitForResponses(0, 'AllDone, () => {
        val wallTime = 1.0e-9 * (System.nanoTime - timestamp)
        log.info(f"All done! ($wallTime%.3f s)")
        domRouter ! 'Shutdown
      }, finished)
  }

  def finished: Receive = {
    case msg =>
      log.error(s"Unexpected message: $msg")
  }

  def waitForResponses(count: Int, response: Any, action: () => Unit,
      nextState: Receive): Receive = {
    case `response` =>
      //log.info(s"Got response $response (${count+1})")
      if (count+1 == nNodes) {
        action()
        context.become(nextState)
      } else context.become(waitForResponses(count+1, response, action, nextState))
    case msg =>
      log.error(s"Unexpected message: $msg")
  }
}
