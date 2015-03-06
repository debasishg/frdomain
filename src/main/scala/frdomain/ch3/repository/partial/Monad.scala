package frdomain.ch3
package repository
package partial

import scala.language.higherKinds

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = {
    flatMap(ma)(a => map(mb)(b => f(a, b)))
  }

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]())){(ma, mla) => println(s"from monad $ma"); map2(ma, mla)(_ :: _)}

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
}

object Monad {
  def apply[F[_]: Monad]: Monad[F] =
    implicitly[Monad[F]]

  implicit val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    override def flatMap[A,B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }

  implicit val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)
    override def flatMap[A,B](ma: List[A])(f: A => List[B]) = ma flatMap f
  }

  implicit def function1Monad[A1]: Monad[({type f[x] = Function1[A1, x]})#f] = new Monad[({type f[x] = Function1[A1, x]})#f] {
    def unit[A](a: => A) = (_: A1) => a
    override def flatMap[A, B](r: A1 => A)(f: A => A1 => B) = (t: A1) => f(r(t))(t)
  }
}
