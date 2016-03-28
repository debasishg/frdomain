package frdomain.ch5
package free

object Main {

  import common._

  object app1 {
    import AccountService._

    val composite =
      for {
        x <- open("a-123", "debasish ghosh", Some(today))
        _ <- credit(x.no, 10000)
        _ <- credit(x.no, 30000)
        _ <- debit(x.no, 23000)
        a <- query(x.no)
      } yield a

    val t = AccountRepoMutableInterpreter().apply(composite)
  }

  object app2 {
    import AccountRepository._

    val account = Account("a-123", "John K")
    val comp = for {
      a <- store(account.copy(balance = Balance(1000)))
      _ <- delete(account.no)
      c <- query(account.no)
    } yield c

    val inter = AccountRepoMutableInterpreter()
    val u = inter.apply(comp)

    val v = AccountRepoShowInterpreter().interpret(comp, List.empty[String])
  }
}
