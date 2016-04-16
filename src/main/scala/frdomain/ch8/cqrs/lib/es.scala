package frdomain.ch8
package cqrs.lib

import org.joda.time.DateTime
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import \/._

object Common {
  type AggregateId = String
  type Error = String
}

import Common._

/**
 * The `Event` abstraction. `Next` points to the next event in chain
 */
trait Event[A] {
  def at: DateTime
}

/**
 * All aggregates need to have an id
 */
trait Aggregate {
  def id: AggregateId
}

trait Snapshot[A <: Aggregate] {
  def updateState(e: Event[_], initial: Map[String, A]): Map[String, A]

  def snapshot(es: List[Event[_]]): String \/ Map[String, A] = 
    es.reverse.foldLeft(Map.empty[String, A]) { (a, e) => updateState(e, a) }.right
}

trait Commands[A] {
  type Command[A] = Free[Event, A]
}

trait RepositoryBackedInterpreter {
  def step: Event ~> Task

  def apply[A](action: Free[Event, A]): Task[A] = action.foldMap(step)
}
