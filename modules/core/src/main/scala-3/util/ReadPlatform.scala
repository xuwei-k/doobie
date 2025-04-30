// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import scala.deriving.Mirror
import scala.compiletime.summonAll

object ReadPlatform {
  final class SeqProduct(values: Seq[Any]) extends Product {
    def canEqual(that: Any): Boolean = true
    def productArity: Int = values.length
    def productElement(n: Int): Any = values(n)
  }

  def productImpl[A <: Product](mirror: Mirror.ProductOf[A], values: Tuple.Map[mirror.MirroredElemTypes, Read]): Read[A] = {
    val reads: List[Read[?]] = values.toList.map(_.asInstanceOf[Read[?]])
    new Read[A](
      reads.flatMap(_.gets),
      (rs, i) =>
        mirror.fromProduct(
          new SeqProduct(
            reads.zipWithIndex.map { (r, n) =>
              r.unsafeGet(rs, i + reads.take(n).map(_.length).sum)
            }
          )
        )
    )
  }
}

trait ReadPlatform {

  inline given [A <: Product](using mirror: Mirror.ProductOf[A]): Read[A] =
    ReadPlatform.productImpl(
      mirror,
      summonAll[Tuple.Map[mirror.MirroredElemTypes, Read]]
    )

  given roe: Read[Option[EmptyTuple]] =
    new Read[Option[EmptyTuple]](Nil, (_, _) => Some(EmptyTuple))

  given rou: Read[Option[Unit]] =
    new Read[Option[Unit]](Nil, (_, _) => Some(()))

  given cons1[H, T <: Tuple](
    using H: => Read[Option[H]],
          T: => Read[Option[T]],
  ): Read[Option[H *: T]] =
    new Read[Option[H *: T]](
      H.gets ++ T.gets,
      (rs, n) =>
        for {
          h <- H.unsafeGet(rs, n)
          t <- T.unsafeGet(rs, n + H.length)
        } yield h *: t
    )

  given cons2[H, T <: Tuple](
    using H: => Read[Option[H]],
          T: => Read[Option[T]]
  ): Read[Option[Option[H] *: T]] =
    new Read[Option[Option[H] *: T]](
      H.gets ++ T.gets,
      (rs, n) => T.unsafeGet(rs, n + H.length).map(H.unsafeGet(rs, n) *: _)
    )

  // Generic Read for option of products.
  given [P <: Product, A](
    using m: Mirror.ProductOf[P],
          i: A =:= m.MirroredElemTypes,
          w: Read[Option[A]]
  ): Read[Option[P]] =
    w.map(a => a.map(a => m.fromProduct(i(a))))

}
