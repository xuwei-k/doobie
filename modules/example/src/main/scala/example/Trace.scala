// Copyright (c) 2013-2017 Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.example

import cats._
import cats.data._
import cats.free._
import cats.implicits._
import cats.effect._
import doobie._
import doobie.implicits._
// import doobie.util.transactor.Strategy
import doobie.util.Tree._
import doobie.util.SyncRef

object Trace {

  sealed abstract case class Remnant private (className: String, identity: Int, s: Option[String]) {
    override def toString = s getOrElse s"${className}<#${identity.toHexString}>"
  }
  object Remnant {
    def apply[A](a: A): Remnant =
      new Remnant(
        a match {
          case _: Free[_, _] => "Free"
          case a if a.getClass.getName.contains("$Lambda$") => "λ"
          case a => a.getClass.getSimpleName
        },
        System.identityHashCode(a),
        a match {
          case p: Product => Some(p.toString)
          case b: Boolean => Some(b.toString)
          case n: Int     => Some(n.toString)
          case _: Unit    => Some("()")
          case _          => None
        }
      ) {}
  }


  ///////

  sealed abstract case class Intention private (context: Remnant, opcode: String, args: List[Remnant], start: Long)
  object Intention {
    def apply[A](context: A, op: Product, start: Long): Intention =
      new Intention(Remnant(context), op.productPrefix, op.productIterator.map(Remnant(_)).toList, start) {}
  }

  ///////

  sealed trait Elem { def intention: Intention }
  final case class Pending(intention: Intention) extends Elem
  final case class Success(intention: Intention, end: Long, result: Remnant) extends Elem
  final case class Failure(intention: Intention, end: Long, error: Throwable) extends Elem

  //////

  def now[F[_]](
    implicit ev: Sync[F]
  ): F[Long] =
    ev.delay(System.currentTimeMillis)

  def trace[F[_] <: Product, J, M[_]: Sync](nt: F ~> Kleisli[Kleisli[M, SyncRef[M, Zipper[Elem]], ?], J, ?]) =
    λ[F ~> Kleisli[Kleisli[M, SyncRef[M, Zipper[Elem]], ?], J, ?]] { op =>
      Kleisli { j =>
        Kleisli { ref =>
          for {
            t  <- now[M]
            _  <- ref.modify(_.insertDownLast(Tree(Pending(Intention(j, op, t)), Nil)))
            e  <- nt(op).run(j).run(ref).attempt
            t  <- now[M]
            a  <- e match  {
              case Right(a) => ref.modify(_.modify(e => Success(e.intention, t, Remnant(a)))).as(a)
              case Left(ex) => ref.modify(_.modify(e => Failure(e.intention, t, ex))) *> Sync[M].raiseError(ex)
            }
            _  <- ref.modify(_.up.getOrElse(sys.error("corrupted trace")))
          } yield a
        }
      }
    }

  object TracingInterpreter {

    def apply[M[_]: Async](f: Tree[Elem] => M[Unit]): doobie.util.transactor.Interpreter[M] =
      new doobie.util.transactor.Interpreter[M] {
        val interp = new Impl[M].ConnectionInterpreter
        def apply[A](op: FC.ConnectionOp[A]) =
          Kleisli { conn =>
            for {
              ref <- SyncRef[M, Zipper[Elem]](Tree(Pending(Intention("root", Nil, 0)) : Elem, Nil).zipper)
              a   <- interp(op).run(conn).run(ref) guarantee ref.gets(_.done).flatMap(f)
            } yield a
          }
      }


    // ffs
    implicit def kleisliAsync[F[_], E](
      implicit ev: Async[F]
    ): Async[Kleisli[F, E, ?]] =
      new Async[Kleisli[F, E, ?]] {
        val ke = Kleisli.catsDataMonadErrorForKleisli[F, E, Throwable]
        def pure[A](x: A): Kleisli[F,E,A] = ke.pure(x)
        def handleErrorWith[A](fa: Kleisli[F,E,A])(f: Throwable => Kleisli[F,E,A]): Kleisli[F,E,A] = ke.handleErrorWith(fa)(f)
        def raiseError[A](e: Throwable): Kleisli[F,E,A] = ke.raiseError(e)
        def async[A](k: (scala.util.Either[Throwable,A] => Unit) => Unit): Kleisli[F,E,A] = Kleisli(_ => ev.async(k))
        def flatMap[A, B](fa: Kleisli[F,E,A])(f: A => Kleisli[F,E,B]): Kleisli[F,E,B] = ke.flatMap(fa)(f)
        def tailRecM[A, B](a: A)(f: A => Kleisli[F,E,Either[A,B]]): Kleisli[F,E,B] = ke.tailRecM(a)(f)
        def suspend[A](thunk: => Kleisli[F,E,A]): Kleisli[F,E,A] = Kleisli(e => thunk.run(e))
      }

    class Impl[M[_]: Async] extends KleisliInterpreter[Kleisli[M, SyncRef[M, Zipper[Elem]], ?]] {
      val M = implicitly
      override lazy val NClobInterpreter             = trace(new NClobInterpreter {})
      override lazy val BlobInterpreter              = trace(new BlobInterpreter {})
      override lazy val ClobInterpreter              = trace(new ClobInterpreter {})
      override lazy val DatabaseMetaDataInterpreter  = trace(new DatabaseMetaDataInterpreter {})
      override lazy val DriverInterpreter            = trace(new DriverInterpreter {})
      override lazy val RefInterpreter               = trace(new RefInterpreter {})
      override lazy val SQLDataInterpreter           = trace(new SQLDataInterpreter {})
      override lazy val SQLInputInterpreter          = trace(new SQLInputInterpreter {})
      override lazy val SQLOutputInterpreter         = trace(new SQLOutputInterpreter {})
      override lazy val ConnectionInterpreter        = trace(new ConnectionInterpreter {})
      override lazy val StatementInterpreter         = trace(new StatementInterpreter {})
      override lazy val PreparedStatementInterpreter = trace(new PreparedStatementInterpreter {})
      override lazy val CallableStatementInterpreter = trace(new CallableStatementInterpreter {})
      override lazy val ResultSetInterpreter         = trace(new ResultSetInterpreter {})
    }

  }

  def handleLog(log: Tree[Elem]): IO[Unit] = {
    def go(log: Tree[Elem], indent: Int): IO[Unit] =
      for {
        _ <- IO(println("  " * indent + log.head))
        _ <- log.tail.value.traverse(go(_, indent + 1))
      } yield ()
    go(log, 0)
  }

  val xa =
    Transactor.fromDriverManager[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql:world",
      "postgres",
      ""
    ).copy(
      // strategy0 = Strategy.void,
      interpret0 = TracingInterpreter[IO](handleLog)
    )

  case class Data(a: String, b: Int) {
    require(a != "Uganda")
  }

  def names(pat: String): ConnectionIO[List[Data]] =
    sql"select name, population from country where name like $pat".query[Data].list

  def prog: ConnectionIO[Int] =
    List(1,2,3,4,5).traverse(FC.delay(_)).map(_.sum)

  val runc: IO[Unit] =
    prog.transact(xa).void

  def main(args: Array[String]): Unit =
    runc.unsafeRunSync

}
