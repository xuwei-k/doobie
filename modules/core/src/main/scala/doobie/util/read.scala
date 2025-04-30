// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import cats._
import doobie.free.{ FRS, ResultSetIO }
import doobie.enumerated.Nullability._
import java.sql.ResultSet

final class Read[A](
  val gets: List[(Get[_], NullabilityKnown)],
  val unsafeGet: (ResultSet, Int) => A
) {

  final lazy val length: Int = gets.length

  def map[B](f: A => B): Read[B] =
      new Read(gets, (rs, n) => f(unsafeGet(rs, n)))

  def ap[B](ff: Read[A => B]): Read[B] =
    new Read(gets ++ ff.gets, (rs, n) => ff.unsafeGet(rs, n)(unsafeGet(rs, n + ff.length)))

  def get(n: Int): ResultSetIO[A] =
    FRS.raw(unsafeGet(_, n))

}

object Read extends ReadPlatform {

  def apply[A](implicit ev: Read[A]): ev.type = ev

  implicit val ReadApply: Applicative[Read] =
    new Applicative[Read] {
      def ap[A, B](ff: Read[A => B])(fa: Read[A]): Read[B] = fa.ap(ff)
      def pure[A](x: A): Read[A] = new Read(Nil, (_, _) => x)
      override def map[A, B](fa: Read[A])(f: A => B): Read[B] = fa.map(f)
    }

  implicit val unit: Read[Unit] =
    new Read(Nil, (_, _) => ())

  implicit def fromGet[A](implicit ev: Get[A]): Read[A] =
    new Read(List((ev, NoNulls)), ev.unsafeGetNonNullable)

  implicit def fromGetOption[A](implicit ev: Get[A]): Read[Option[A]] =
    new Read(List((ev, Nullable)), ev.unsafeGetNullable)

}
