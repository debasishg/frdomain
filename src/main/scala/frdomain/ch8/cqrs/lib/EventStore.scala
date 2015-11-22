package frdomain.ch8
package cqrs.lib

import collection.concurrent.TrieMap
import scalaz._
import Scalaz._

import Common._
import spray.json._

trait EventStore[K] {
  /**
   * gets the list of events for an aggregate key `key`
   */
  def get(key: K): List[Event[_]]

  /**
   * puts a `key` and its associated `event`
   */
  def put(key: K, event: Event[_]): Error \/ Event[_]

  /**
   * similar to `get` but returns an error if the `key` is not found
   */
  def events(key: K): Error \/ List[Event[_]]
  
  /**
   * get all ids from the event store
   */
  def allEvents: Error \/ List[Event[_]]
}

/**
 * In memory store
 */
object InMemoryEventStore {
  def apply[K] = new EventStore[K] {
    val eventLog = TrieMap[K, List[Event[_]]]() 

    def get(key: K): List[Event[_]] = eventLog.get(key).getOrElse(List.empty[Event[_]])
    def put(key: K, event: Event[_]): Error \/ Event[_] = {
      val currentList = eventLog.getOrElse(key, Nil)
      eventLog += (key -> (event :: currentList))
      event.right
    }
    def events(key: K): Error \/ List[Event[_]] = {
      val currentList = eventLog.getOrElse(key, Nil)
      if (currentList.isEmpty) s"Aggregate $key does not exist".left
      else currentList.right
    }
    def allEvents: Error \/ List[Event[_]] = eventLog.values.toList.flatten.right
  }
}
      
/**
 * In memory json store
 */
trait InMemoryJSONEventStore { 
  implicit val eventJsonFormat: RootJsonFormat[Event[_]]
  def apply[K] = new EventStore[K] {
    val eventLog = TrieMap[K, List[String]]() 

    def get(key: K): List[Event[_]] = 
      eventLog.get(key).map(ls => ls.map(_.parseJson.convertTo[Event[_]])).getOrElse(List.empty[Event[_]])

    def put(key: K, event: Event[_]): Error \/ Event[_] = {
      val currentList = eventLog.getOrElse(key, Nil)
      eventLog += (key -> (eventJsonFormat.write(event).toString :: currentList))
      event.right
    }
    def events(key: K): Error \/ List[Event[_]] = {
      val currentList = eventLog.getOrElse(key, Nil)
      if (currentList.isEmpty) s"Aggregate $key does not exist".left
      else currentList.map(js => js.parseJson.convertTo[Event[_]]).right
    }
    def allEvents: Error \/ List[Event[_]] = eventLog.values.toList.flatten.map(_.parseJson.convertTo[Event[_]]).right
  }
}
      

