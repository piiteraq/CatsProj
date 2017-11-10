import com.sun.net.httpserver.Authenticator.Failure

import scala.util.Try
// Typeclass Exercise

trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}

object Monoid {
  def apply[A : Monoid]: Monoid[A] = implicitly[Monoid[A]]
}

final case class Par[A, B](first: A, second: B)

object Par {

  implicit val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def empty: String = ""
    def combine(x: String, y: String): String = x ++ y
  }

  def combineAll[A : Monoid](list: List[A]): A = list.foldRight(Monoid[A].empty)(Monoid[A].combine)

  // Build a monoid of Par from two separate monoids of, possibly, different types
  implicit def tuple2Instance[A, B](implicit A: Monoid[A], B: Monoid[B]): Monoid[Par[A, B]] =
    new Monoid[Par[A, B]] {
      def empty: Par[A, B] = Par(A.empty, B.empty)

      def combine(x: Par[A, B], y: Par[A, B]): Par[A, B] =
        Par(A.combine(x.first, y.first), B.combine(x.second, y.second))
    }
}


object TypeClassTest extends App { // needed for tut, irrelevant to demonstration

  import Par._
  import concurrent.Future
  import concurrent.ExecutionContext.Implicits.global // Import global thread pool
  import util.{Success, Failure}

  println(combineAll(List("ab", "cd", "ef")))
  println(combineAll(List(1,2,4,8)))
  println(combineAll(List(Par(1, "hello"), Par(2, " "), Par(3, "world"))))

  val list = (1 to 10000).toList
  val (left, right) = list.splitAt(5000)

  val sumLeft = Future {
    combineAll(left)
  }

  val sumRight = Future {
    combineAll(right)
  }

  val sumTot = for {
    sl <- sumLeft
    sr <- sumRight
  } yield Monoid[Int].combine(sl,sr)

  sumTot onComplete {
    case Success(s) => println(s"SUM_TOT: $s")
    case Failure(f) => println(s"Failed: $f")
  }

  Thread.sleep(2000) // Give future a chance to complete.
}