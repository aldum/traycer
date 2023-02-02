import scala.util.chaining.*

trait Functor[F[_]]:
  extension [A](x: F[A]) def map[B](f: A => B): F[B]

trait Monad[F[_]] extends Functor[F]:
  def pure[A](x: A): F[A]

  extension [A](x: F[A])
    /** The fundamental composition operation */
    def flatMap[B](f: A => F[B]): F[B]

    /** The `map` operation can now be defined in terms of `flatMap` */
    def map[B](f: A => B) = x.flatMap(f.andThen(pure))

    def >>[B](mb: F[B]): F[B] = x.flatMap(_ => mb)

enum Maybe[T]:
  case Some(t: T)
  case Empty()

import Maybe.*

given Monad[Maybe] with
  def pure[A](x: A): Maybe[A] = Some(x)

  extension [A](xs: Maybe[A])
    def flatMap[B](f: A => Maybe[B]): Maybe[B] =
      xs match
        case Empty() => Empty()
        case Some(a) => f(a)

@main def m =
  Some("a") >> Some("b") tap println
