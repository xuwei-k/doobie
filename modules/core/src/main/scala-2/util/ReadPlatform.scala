// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import shapeless.{ HList, HNil, ::, Generic, Lazy, <:!< }

trait ReadPlatform extends LowerPriorityRead { this: Read.type =>

}


final case class ProductRead [A <: HList] private(value: Read[A])

object ProductRead {
  implicit val nil: ProductRead[HNil] =
    ProductRead[HNil](new Read[HNil](Nil, (_, _) => HNil))

  implicit def cons[H, T <: HList](H: Read[H], T: ProductRead[T]): ProductRead[H :: T] =
    ProductRead(
      new Read[H :: T](
        H.gets ++ T.value.gets,
        (rs, n) => H.unsafeGet(rs, n) :: T.value.unsafeGet(rs, n + H.length)
      )
    )
}

trait LowerPriorityRead extends EvenLower { this: Read.type =>

  implicit def generic[F, G <: HList](implicit gen: Generic.Aux[F, G], G: Lazy[ProductRead[G]]): Read[F] =
    new Read[F](G.value.value.gets, (rs, n) => gen.from(G.value.value.unsafeGet(rs, n)))

}

trait EvenLower {

  implicit val ohnil: Read[Option[HNil]] =
    new Read[Option[HNil]](Nil, (_, _) => Some(HNil))

  implicit def ohcons1[H, T <: HList](
    implicit H: Lazy[Read[Option[H]]],
              T: Lazy[Read[Option[T]]],
              N: H <:!< Option[α] forSome { type α }
  ): Read[Option[H :: T]] = {
    void(N)
    new Read[Option[H :: T]](
      H.value.gets ++ T.value.gets,
      (rs, n) =>
        for {
          h <- H.value.unsafeGet(rs, n)
          t <- T.value.unsafeGet(rs, n + H.value.length)
        } yield h :: t
    )
  }

  implicit def ohcons2[H, T <: HList](
    implicit H: Lazy[Read[Option[H]]],
              T: Lazy[Read[Option[T]]]
  ): Read[Option[Option[H] :: T]] =
    new Read[Option[Option[H] :: T]](
      H.value.gets ++ T.value.gets,
      (rs, n) => T.value.unsafeGet(rs, n + H.value.length).map(H.value.unsafeGet(rs, n) :: _)
    )

  implicit def ogeneric[A, Repr <: HList](
    implicit G: Generic.Aux[A, Repr],
              B: Lazy[Read[Option[Repr]]]
  ): Read[Option[A]] =
    new Read[Option[A]](B.value.gets, B.value.unsafeGet(_, _).map(G.from))

}

