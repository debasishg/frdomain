package frdomain.ch6
package domain

import scala.concurrent.Future
import scalaz._
import Scalaz._

package object service {
  type Valid[A] = EitherT[Future, NonEmptyList[String], A]
}
