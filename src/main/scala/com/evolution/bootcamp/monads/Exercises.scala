package com.evolution.bootcamp.monads

import cats.Functor
import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.Monoid

trait MonoidHierarchy {

  /*
    Semigroup[A] {
      def combine(y: A): A
    }
   */

  /*
    Monoid[A] {
      def combine(y: A): A
    }

    def empty: A
   */

  // NonEmptyList

  type Error = String
  type Result[T] = Either[NonEmptyList[Error], T]

  // laws
  type A
  val anyValue: A = ???
  val v1: A = ???
  val v2: A = ???
  val v3: A = ???

  implicit val instance: Monoid[A]

  (Monoid[A].empty |+| anyValue) == anyValue // left identity

  (anyValue |+| Monoid[A].empty) == anyValue // right identity

  ((v1 |+| v2) |+| v3) == (v1 |+| (v2 |+| v3)) // associativity
}

object MonadHierarchy {
  /*
    Functor[F[A]] {
      def map(f: A => B): F[B]
    }
   */

  /*
    Applicative[F[A]] {
      def ap(f: F[A => B]): F[B]
    }

    def pure[A](x: A): F[A]
   */

  /*
    Monad[F[A]] {
      def flatMap(f: A => F[B]): F[B]
    }

    def pure[A](x: A): F[A]
   */

  // applicative: (F[A], F[B]) => F[(A, B)]
  // monad:        F[F[A]]     => F[A]
}

object Excercises {

  trait Applicative[F[_]] extends Functor[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def unit[A](a: => A): F[A]

    // implement methods using each other
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))

    def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = sequence(as.map(f))
  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    // implement methods using each other
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

    def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))

    def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = join(map(ma)(a => map(mb)(b => f(a, b))))
  }

}

object Monads {

  trait Monad[M[_]] {
    def unit[A](a: A): M[A]
    def bind[A, B](ma: M[A])(amb: A => M[B]): M[B]
  }

  trait Monoid[A] {
    def mempty: A
    def mappend(x: A)(y: A): A
  }


  case class Identity[A](a: A)
  object Identity {
    implicit val identityMonad = new Monad[Identity] {
      def unit[A](a: A): Identity[A] = Identity(a)
      def bind[A, B](ma: Identity[A])(amb: A => Identity[B]): Identity[B] = amb(ma.a)
    }
  }

  sealed trait Maybe[+A]
  case class Just[A](a: A) extends Maybe[A]
  case object None extends Maybe[Nothing]
  object Maybe {
    implicit val maybeMonad = new Monad[Maybe] {
      def unit[A](a: A): Maybe[A] = Just(a)
      def bind[A, B](ma: Maybe[A])(amb: A => Maybe[B]): Maybe[B] =  ma match {
        case Just(a) => amb(a)
        case None => None
      }
    }
  }

  case class State[S, A](run: S => (S, A))
  object State {
    // inspired by https://stackoverflow.com/a/6248296
    implicit def stateMonad[S] = new Monad[({type x[a]=State[S, a]})#x] {
      def unit[A](a: A): State[S, A] = State(s => (s, a))
      def bind[A, B](ma: State[S, A])(amb: A => State[S, B]): State[S, B] = {
        State(s => {
          val (newS, a) = ma.run(s)
          val stateB = amb(a)
          stateB.run(newS)
        })
      }
    }
  }

  case class Reader[R, A](run: R => A)
  object Reader {
    implicit def readerMonad[R] = new Monad[({type x[a]=Reader[R, a]})#x] {
      def unit[A](a: A): Reader[R, A] = Reader(_ => a)
      def bind[A, B](ma: Reader[R, A])(amb: A => Reader[R, B]): Reader[R, B] = Reader(r => {
        amb(ma.run(r)).run(r)
      })
    }
  }

  case class Writer[W, A](run: (W, A))
  object Writer {
    implicit def readerMonad[W](implicit m: Monoid[W]) = new Monad[({type x[a]=Writer[W, a]})#x] {
      def unit[A](a: A): Writer[W, A] = ??? //https://thumbs.gfycat.com/SoulfulWeeElephant-size_restricted.gif
      def bind[A, B](ma: Writer[W, A])(amb: A => Writer[W, B]): Writer[W, B] = ???
    }
  }
}
