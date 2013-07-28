package edu.cornell.cdm89.scalaspec.driver

import akka.actor.{ActorRef, ActorSystem}

import edu.cornell.cdm89.scalaspec.domain.DomainInfo
import edu.cornell.cdm89.scalaspec.pde.{BoundaryCondition, FluxConservativePde}

trait RunConfiguration {
  def leftBc: BoundaryCondition
  def rightBc: BoundaryCondition

  def pde: FluxConservativePde

  def domainInfo: DomainInfo

  def createIdActor(system: ActorSystem, subdomain: ActorRef): ActorRef
  def createObsActor(system: ActorSystem, subdomain: ActorRef): ActorRef
}
