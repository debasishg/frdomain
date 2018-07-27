package frdomain.ch6
package domain

import cats._
import cats.data._
import cats.instances.all._
import cats.effect.IO

import repository.AccountRepository

package object service {
  type Valid[A] = EitherT[IO, AccountServiceException, A]
  type AccountOperation[A] = Kleisli[Valid, AccountRepository, A]
}
