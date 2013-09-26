package edu.cornell.cdm89.scalaspec.driver

import akka.actor.{ActorRef, ActorSystem, Props}
import com.typesafe.config.Config

import edu.cornell.cdm89.scalaspec.domain.DomainInfo
import edu.cornell.cdm89.scalaspec.pde.ScalarAdvectionEquation
import edu.cornell.cdm89.scalaspec.pde.{SineBoundary, AdvectionOutflowBoundary, AdvectionConstantBoundary}
import edu.cornell.cdm89.scalaspec.pde.SineWaveInitialData

class ScalarAdvectionConfig(a: Double, config: Config) extends RunConfiguration {
  val leftBc = new SineBoundary(a, 1.0)
  val rightBc = new AdvectionOutflowBoundary(-1.0)

  val pde = new ScalarAdvectionEquation(a)

  val domainInfo = {
    val order = config.getInt("harvest.order-of-elements")
    val nElems = config.getInt("harvest.nr-of-elements")
    DomainInfo(0.0, 2.0, order, nElems)
  }

  def createIdActor(system: ActorSystem, subdomain: ActorRef): ActorRef = {
    system.actorOf(Props(classOf[SineWaveInitialData], 1.0, 1.0, subdomain),
      "idProvider")
  }

  def createObsActor(system: ActorSystem, subdomain: ActorRef): ActorRef = {
    val obsDt = config.getDouble("harvest.obs-dt")
    system.actorOf(Props(classOf[YgraphObserver], obsDt, subdomain), "obs")
  }
}


import edu.cornell.cdm89.scalaspec.pde.Hesthaven53Equation

class Hesthaven53Config(config: Config) extends RunConfiguration {
  // Periodic BCs
  val leftBc = ???
  val rightBc = ???

  val pde = new Hesthaven53Equation

  val domainInfo = {
    val order = config.getInt("harvest.order-of-elements")
    val nElems = config.getInt("harvest.nr-of-elements")
    DomainInfo(-1.0, 1.0, order, nElems)
  }

  def createIdActor(system: ActorSystem, subdomain: ActorRef): ActorRef = {
    system.actorOf(Props(classOf[SineWaveInitialData], 1.0, 4.0*math.Pi, subdomain),
      "idProvider")
  }

  def createObsActor(system: ActorSystem, subdomain: ActorRef): ActorRef = {
    val obsDt = config.getDouble("harvest.obs-dt")
    system.actorOf(Props(classOf[YgraphObserver], obsDt, subdomain), "obs")
  }
}