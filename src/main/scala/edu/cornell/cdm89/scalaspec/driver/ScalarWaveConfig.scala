package edu.cornell.cdm89.scalaspec.driver

import akka.actor.{ActorRef, ActorSystem, Props}
import com.typesafe.config.Config

import edu.cornell.cdm89.scalaspec.domain.DomainInfo
import edu.cornell.cdm89.scalaspec.pde.ScalarWaveEquation
import edu.cornell.cdm89.scalaspec.pde.{WaveOutflowBoundary, WaveInversionBoundary, WaveReflectionBoundary}
import edu.cornell.cdm89.scalaspec.pde.TrianglePulseInitialData

class ScalarWaveConfig(config: Config) extends RunConfiguration {
  val leftBc = new WaveOutflowBoundary(1.0)
  val rightBc = new WaveOutflowBoundary(-1.0)

  val pde = new ScalarWaveEquation

  val domainInfo = {
    val order = config.getInt("harvest.order-of-elements")
    val nElems = config.getInt("harvest.nr-of-elements")
    DomainInfo(0.0, 10.0, order, nElems)
  }

  def createIdActor(system: ActorSystem, subdomain: ActorRef): ActorRef = {
    system.actorOf(Props(classOf[TrianglePulseInitialData],
      subdomain, 5.0, 0.5, 1.0), "idProvider")
  }

  def createObsActor(system: ActorSystem, subdomain: ActorRef): ActorRef = ???
}
