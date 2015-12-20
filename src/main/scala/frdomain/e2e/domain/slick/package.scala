package frdomain.e2e
package domain

import scalaz._
import Scalaz._

import domain.slickdb.repository.AccountRepoF

package object slickdb {
  type AccountRepo[A] = Free[AccountRepoF, A]
}



