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
// import fs2._

object Trace {


  /*
   * This is't particularly principled because we want to log an arbitrary program (not just one
   * whose products are showable) so there are some janky type patterns.
   */

  def string(a: Any): String =
    a match {
      case _: Free[_, _] => s"Free<${System.identityHashCode(a).toHexString}>"
      case x if x.getClass.getName.contains("$Lambda$") => s"λ<${System.identityHashCode(x).toHexString}>"
      case p: Product => p.productIterator.map(string).mkString(p.productPrefix + "(", ", ", ")")
      case a: Boolean => a.toString
      case a: String  => s""""$a""""
      case a: Int     => a.toString
      case _: Unit    => "()"
      case a => s"${a.getClass.getSimpleName}<${System.identityHashCode(a).toHexString}>"
    }

  def trace[F[_], J: Manifest, M[_]: Sync](nt: F ~> Kleisli[StateT[M, Int, ?], J, ?]) =
    λ[F ~> Kleisli[StateT[M, Int, ?], J, ?]] { op =>
      Kleisli { (j: J) =>

        val st = nt(op).run(j)

        import StateT._
        for {
          n <- get[M, Int]
          _ <- lift(Sync[M].delay(Console.println(f"${string(j)}%-31s |${"  " * n} ${string(op)}")))
          _ <- modify[M, Int](_ + 1)
          a <- st
          _ <- modify[M, Int](_ - 1)
          // _ <- lift(ev.delay(Console.println("<" +  ("  " * n) + string(a))))
        } yield a

      }
    }

  abstract class TracingKleisliInterpreter[M[_]: Async] extends KleisliInterpreter[StateT[M, Int, ?]] {
    override lazy val NClobInterpreter             = trace(            new NClobInterpreter {})
    override lazy val BlobInterpreter              = trace(             new BlobInterpreter {})
    override lazy val ClobInterpreter              = trace(             new ClobInterpreter {})
    override lazy val DatabaseMetaDataInterpreter  = trace( new DatabaseMetaDataInterpreter {})
    override lazy val DriverInterpreter            = trace(           new DriverInterpreter {})
    override lazy val RefInterpreter               = trace(              new RefInterpreter {})
    override lazy val SQLDataInterpreter           = trace(          new SQLDataInterpreter {})
    override lazy val SQLInputInterpreter          = trace(         new SQLInputInterpreter {})
    override lazy val SQLOutputInterpreter         = trace(        new SQLOutputInterpreter {})
    override lazy val ConnectionInterpreter        = trace(       new ConnectionInterpreter {})
    override lazy val StatementInterpreter         = trace(        new StatementInterpreter {})
    override lazy val PreparedStatementInterpreter = trace(new PreparedStatementInterpreter {})
    override lazy val CallableStatementInterpreter = trace(new CallableStatementInterpreter {})
    override lazy val ResultSetInterpreter         = trace(        new ResultSetInterpreter {})
  }

  object TracingKleisliInterpreter {
    def apply[M[_]: Async]: TracingKleisliInterpreter[M] =
      new TracingKleisliInterpreter[M] {
        val M = implicitly
      }
  }

  val xa =
    Transactor.fromDriverManager[StateT[IO, Int, ?]](
      "org.postgresql.Driver",
      "jdbc:postgresql:world",
      "postgres",
      ""
    ).copy(interpret0 = TracingKleisliInterpreter[IO].ConnectionInterpreter)

  case class Data(a: String, b: Int) {
    require(a != "Uganda")
  }

  def names(pat: String): ConnectionIO[List[Data]] =
    sql"select name, population from country where name like $pat".query[Data].list

  // def names2(pat: String): ConnectionIO[Unit] =
  //   sql"select name, population from country where name like $pat".query[Data].stream.run.void

  val runc: IO[Unit] =
    names("U%").transact(xa).runA(1).void

  def main(args: Array[String]): Unit =
    runc.unsafeRunSync

}
