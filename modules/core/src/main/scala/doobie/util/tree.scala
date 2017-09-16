// Copyright (c) 2013-2017 Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import annotation.tailrec
import cats._
import cats.free._
import cats.implicits._

object Tree {

  type Tree[A] = Cofree[List, A]
  object Tree {
    def apply[A](h: A, t: List[Tree[A]]): Tree[A] =
      Cofree(h, Eval.now(t))
  }

  implicit class TreeOps[A](ta: Tree[A]) {
    def zipper: Zipper[A] =
      Zipper(ta, Nil, Nil, Nil)
  }

  /**
   * A zipper over `Cofree[List, A]`, distilled from
   * [[https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/TreeLoc.scala Scalaz's TreeLoc]],
   * originally written by Jason Zaugg.
   */
  final case class Zipper[A](
    focus: Tree[A],
    ls:    List[Tree[A]],
    rs:    List[Tree[A]],
    ps:    List[(List[Tree[A]], A, List[Tree[A]])]
  ) {

    // We end up doing this a lot, so abstract it out.
    private final implicit class LocalListOps[T](as: List[T]) {
      def uncons[U](f: (T, List[T]) => U): Option[U] =
        as match { case a :: as => Some(f(a, as)); case _ => None }
    }

    def done: Tree[A] =
      top.focus

    def up: Option[Zipper[A]] =
      ps.uncons { case ((pls, v, prs), ps) =>
        Zipper(Tree(v, ls.reverse_:::(focus :: rs)), pls, prs, ps)
      }

    @tailrec
    def top: Zipper[A] =
      up match {
        case Some(z) => z.top
        case None    => this
      }

    def left:  Option[Zipper[A]] =
      ls.uncons(Zipper(_, _, focus :: rs, ps))

    def right: Option[Zipper[A]] =
      rs.uncons(Zipper(_, focus :: ls, _, ps))

    def firstChild: Option[Zipper[A]] =
      focus.tail.value.uncons(Zipper(_, Nil, _, downParents))

    def lastChild:  Option[Zipper[A]] =
      focus.tail.value.reverse.uncons(Zipper(_, _, Nil, downParents))

    def isRoot: Boolean =
      ps.isEmpty

    def isFirst: Boolean =
      ls.isEmpty

    def isLast: Boolean =
      rs.isEmpty

    def isLeaf: Boolean =
      focus.tail.isEmpty

    def isChild: Boolean =
      !isRoot

    def hasChildren: Boolean =
      !isLeaf

    def setTree(t: Tree[A]): Zipper[A] =
      Zipper(t, ls, rs, ps)

    def modifyTree(f: Tree[A] => Tree[A]): Zipper[A] =
      setTree(f(focus))

    def modify(f: A => A): Zipper[A] =
      set(f(get))

    def get: A =
      focus.head

    def set(a: A): Zipper[A] =
      modifyTree(_.copy(head = a))

    def insertLeft(t: Tree[A]): Zipper[A] =
      Zipper(t, ls, focus :: rs, ps)

    def insertRight(t: Tree[A]): Zipper[A] =
      Zipper(t, focus :: ls, rs, ps)

    def insertDownFirst(t: Tree[A]): Zipper[A] =
      Zipper(t, Nil, focus.tail.value, downParents)

    def insertDownLast(t: Tree[A]): Zipper[A] =
      Zipper(t, focus.tail.value.reverse, Nil, downParents)

    def delete: Option[Zipper[A]] =
      rs.uncons(Zipper(_, ls, _, ps)) <+>
      ls.uncons(Zipper(_, _, rs, ps)) <+>
      up.map(_.modifyTree(_.copy(tail = Eval.now(List.empty[Tree[A]]))))

    def path: List[A] =
      get :: ps.map(_._2)

    def map[B](f: A => B): Zipper[B] = {
      Zipper(
        focus.map(f), ls.map(_.map(f)), rs.map(_.map(f)),
        ps.map { case (l, t, r) => (l.map(_.map(f)), f(t), r.map(_.map(f))) }
      )
    }

    private def downParents =
      (ls, focus.head, rs) :: ps

  }

}
