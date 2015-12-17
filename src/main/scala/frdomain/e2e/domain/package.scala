package frdomain.e2e

import scalaz.Free
import domain.repository.AccountRepoF

package object domain {
  type AccountRepo[A] = Free[AccountRepoF, A]
}


