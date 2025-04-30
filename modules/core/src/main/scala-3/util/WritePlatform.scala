// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import scala.deriving.Mirror
import scala.compiletime.summonAll

object WritePlatform {

  def productImpl[A <: Product](mirror: Mirror.ProductOf[A], values: Tuple.Map[mirror.MirroredElemTypes, Write]): Write[A] = {
    val writes: List[Write[Any]] = values.toList.map(_.asInstanceOf[Write[Any]])
    new Write[A](
      writes.flatMap(_.puts),
      { (x: A) =>
        x.productIterator.zip(writes).flatMap{
          (a, w) =>
            w.toList(a)
        }.toList
      },
      (ps, n, x) =>
        writes.zip(x.productIterator).zipWithIndex.foreach {
          case (((w, a), i)) =>
            w.unsafeSet(ps, n + writes.take(i).map(_.length).sum, a)
        }
      ,
      (ps, n, x) =>
        writes.zip(x.productIterator).zipWithIndex.foreach {
          case (((w, a), i)) =>
            w.unsafeUpdate(ps, n + writes.take(i).map(_.length).sum, a)
        }
    )
  }
}

trait WritePlatform {

  // Inductive write for writable head and tail.
  inline given product[A <: Product](using mirror: Mirror.ProductOf[A]): Write[A] =
    WritePlatform.productImpl[A](
      mirror,
      summonAll[Tuple.Map[mirror.MirroredElemTypes, Write]]
    )

  // Trivial write for option of empty tuple.
  given woe: Write[Option[EmptyTuple]] =
    new Write[Option[EmptyTuple]](Nil, _ => Nil, (_, _, _) => (), (_, _, _) => ())

  // Trivial write for option of Unit.
  given wou: Write[Option[Unit]] =
    new Write[Option[Unit]](Nil, _ => Nil, (_, _, _) => (), (_, _, _) => ())

  // Write[Option[H]], Write[Option[T]] implies Write[Option[H *: T]]
  given cons1[H, T <: Tuple](
    using H: => Write[Option[H]],
          T: => Write[Option[T]],
          // N: H <:!< Option[_],
  ): Write[Option[H *: T]] =

    def split[A](i: Option[H *: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case h *: t => f(Some(h), Some(t)) }

    new Write(
      H.puts ++ T.puts,
      split(_) { (h, t) => H.toList(h) ++ T.toList(t) },
      (ps, n, i) => split(i) { (h, t) => H.unsafeSet(ps, n, h); T.unsafeSet(ps, n + H.length, t) },
      (rs, n, i) => split(i) { (h, t) => H.unsafeUpdate(rs, n, h); T.unsafeUpdate(rs, n + H.length, t) }
    )

  // Write[Option[H]], Write[Option[T]] implies Write[Option[Option[H] *: T]]
  given cons2[H, T <: Tuple](
    using H: => Write[Option[H]],
          T: => Write[Option[T]]
  ): Write[Option[Option[H] *: T]] =

    def split[A](i: Option[Option[H] *: T])(f: (Option[H], Option[T]) => A): A =
      i.fold(f(None, None)) { case oh *: t => f(oh, Some(t)) }

    new Write(
      H.puts ++ T.puts,
      split(_) { (h, t) => H.toList(h) ++ T.toList(t) },
      (ps, n, i) => split(i) { (h, t) => H.unsafeSet(ps, n, h); T.unsafeSet(ps, n + H.length, t) },
      (rs, n, i) => split(i) { (h, t) => H.unsafeUpdate(rs, n, h); T.unsafeUpdate(rs, n + H.length, t) }
    )

  // Generic write for options of products.
  given [P <: Product, A](
    using m: Mirror.ProductOf[P],
          i: m.MirroredElemTypes =:= A,
          w: Write[Option[A]]
  ): Write[Option[P]] =
    w.contramap(op => op.map(p => i(Tuple.fromProductTyped(p))))

}
