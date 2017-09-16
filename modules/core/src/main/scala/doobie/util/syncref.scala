package doobie.util

import cats._
import cats.implicits._
import cats.effect.Sync
import java.util.concurrent.atomic.AtomicReference

sealed trait SyncRef[F[_], A] { outer =>

  def get: F[A]

  def gets[B](f: A => B)(implicit ev: Functor[F]): F[B] =
    get.map(f)

  def set(a: A): F[Unit]

  def modify(f: A => A)(implicit ev: FlatMap[F]): F[Unit] =
    gets(f).flatMap(set)

  def imap[B](f: A => B)(g: B => A)(
    implicit ev: Functor[F]
  ): SyncRef[F, B] =
    new SyncRef[F, B] {
      override def get = outer.get.map(f)
      override def set(b: B) = outer.set(g(b))
    }

}
object SyncRef {

  def apply[F[_], A](a: A)(
    implicit ev: Sync[F]
  ): F[SyncRef[F, A]] =
    ev.delay {
      new SyncRef[F, A] {
        val cell = new AtomicReference(a)
        override def get = ev.delay(cell.get)
        override def set(a: A) = ev.delay(cell.set(a))
      }
    }

}
