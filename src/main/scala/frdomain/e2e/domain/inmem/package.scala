package frdomain.e2e
package domain

import scalaz._
import Scalaz._

import domain.inmem.repository.AccountRepoF

package object inmem {
  type AccountRepo[A] = Free[AccountRepoF, A]
}


