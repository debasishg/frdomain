package frdomain.ch4
package patterns

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    println(s"+++ app $fa $fb")
    ap(map(fa)(f.curried))(fb)
  }

  def ap[A, B](f: F[A => B])(a: F[A]): F[B]

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]())){(a, fbs) => println(s"from app $a"); map2(f(a), fbs)(_ :: _)}
}

object Applicative {
  def apply[F[_]: Applicative]: Applicative[F] =
    implicitly[Applicative[F]]

  implicit def ListApply: Applicative[List] = new Applicative[List] {
    def map[A, B](a: List[A])(f: A => B): List[B] = a map f
    def unit[A](a: => A): List[A] = List(a)
    def ap[A, B](fs: List[A => B])(as: List[A]): List[B] = for {
      a <- as
      f <- fs
    } yield f(a)
  }

  implicit def OptionApply: Applicative[Option] = new Applicative[Option] {
    def map[A, B](a: Option[A])(f: A => B): Option[B] = a map f
    def unit[A](a: => A): Option[A] = Some(a)
    def ap[A, B](fs: Option[A => B])(as: Option[A]): Option[B] = for {
      a <- as
      f <- fs
    } yield f(a)
  }
}
