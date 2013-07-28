package edu.cornell.cdm89.scalaspec.util

import scala.collection.immutable
import scala.collection.mutable

class ResponseTracker[A, B](responders: immutable.Set[B]) {
  private val completed = mutable.Set.empty[A]
  private val inProgress = mutable.Map.empty[A, mutable.Set[B]]

  def register(key: A, responder: B): Unit = {
    require(!completed.contains(key))
    if (!inProgress.contains(key)) inProgress(key) = responders.to[mutable.Set]
    inProgress(key).remove(responder)
    if (inProgress(key).isEmpty) {
      completed.add(key)
      inProgress.remove(key)
    }
  }

  def allCompleted: Boolean = inProgress.isEmpty
  def keyCompleted(key: A): Boolean = completed.contains(key)
}
