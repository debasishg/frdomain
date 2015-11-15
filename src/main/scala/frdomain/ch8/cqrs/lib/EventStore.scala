package frdomain.ch8
package cqrs.lib

import collection.concurrent.TrieMap
import scalaz._
import Scalaz._

import Common._

trait EventStore[K] {
  def get(key: K): List[Event[_]]
  def put(key: K, event: Event[_]): Error \/ Event[_]
  def events(no: K): Error \/ List[Event[_]]
}

object InMemoryEventStore {
  def apply[K] = new EventStore[K] {
    val eventLog = TrieMap[K, List[Event[_]]]() 

    def get(key: K): List[Event[_]] = eventLog.get(key).getOrElse(List.empty[Event[_]])
    def put(key: K, event: Event[_]): Error \/ Event[_] = {
      val currentList = eventLog.getOrElse(key, Nil)
      eventLog += (key -> (event :: currentList))
      event.right
    }
    def events(no: K): Error \/ List[Event[_]] = {
      val currentList = eventLog.getOrElse(no, Nil)
      if (currentList.isEmpty) s"Aggregate $no does not exist".left
      else currentList.right
    }
  }
}
      

