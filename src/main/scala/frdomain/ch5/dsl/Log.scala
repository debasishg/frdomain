package frdomain.ch5
package dsl

import org.joda.time.DateTime
import freek._
import cats.{~>, Id}
import cats.std.option._
import cats.data.Xor

object Log {
  sealed trait LogLevel
  case object ErrorLevel extends LogLevel
  case object WarnLevel extends LogLevel
  case object InfoLevel extends LogLevel
  case object DebugLevel extends LogLevel

  trait DSL[A]
  case class LogMsg(level: LogLevel, msg: String) extends DSL[Unit]

  /** just helpers without any weird implicits */
  def debug(msg: String) = LogMsg(DebugLevel, msg)
  def info(msg: String) = LogMsg(InfoLevel, msg)

  // Interpreters as simple TransNat
  val logger = new (Log.DSL ~> cats.Id) {
    def apply[A](a: Log.DSL[A]) = a match {
      case Log.LogMsg(lvl, msg) =>
        println(s"$lvl $msg")
    }
  }
}
