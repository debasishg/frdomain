package frdomain.ch5

import scalaz.Free

package object free {
  type AccountRepo[A] = Free[AccountRepoF, A]
}

