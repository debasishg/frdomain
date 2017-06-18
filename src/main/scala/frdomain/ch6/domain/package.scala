package frdomain.ch6
package domain

import scala.concurrent.Future
import scalaz._
import Scalaz._

package object service {
  type Valid[A] = EitherT[Future, NonEmptyList[String], A]

  object Valid {
    implicit val validAppl = new Applicative[Valid] {
      override def point[A](a: => A): Valid[A] = EitherT(Future.successful(a.right))
      override def ap[A, B](fa: => Valid[A])(f: => Valid[(A) => B]): Valid[B] = ???
    }

    def apply[A](block: => NonEmptyList[String] \/ A): Valid[A] = EitherT { Future.successful(block) }
  }
}
