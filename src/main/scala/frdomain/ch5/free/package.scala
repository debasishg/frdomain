package frdomain.ch5

import scalaz.Free.FreeC

package object free {
  type AccountRepo[A] = FreeC[AccountRepoF, A]
}

