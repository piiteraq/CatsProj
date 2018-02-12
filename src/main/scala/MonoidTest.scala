import com.sun.net.httpserver.Authenticator.Failure

import scala.util.Try
// Typeclass Exercise

final case class Par[A, B](first: A, second: B)

object Par {

  trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A
  }

  object Monoid {
    def apply[A : Monoid]: Monoid[A] = implicitly[Monoid[A]]
  }

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

object CatsSemigroupTest extends App {

  import cats.Semigroup
  import cats.syntax.semigroup._
  import cats.Monoid
  import cats.instances._
  import cats.instances.map._
  import cats.instances.set._
  import cats.instances.string._

  implicit val intAdditionSemigroup: Semigroup[Int] = new Semigroup[Int] {
    def combine(x: Int, y: Int): Int = x + y
  }

  implicit val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    def combine(x: Int, y: Int): Int = x + y

    def empty: Int = 0
  }

  implicit val listConcatSemigroup: Semigroup[List[String]] = new Semigroup[List[String]] {
    def combine(x: List[String], y: List[String]): List[String] = x ++ y
  }

  def optionCombine[A: Semigroup](a: A, opt: Option[A]): A =
    opt.map(a |+| _).getOrElse(a)

  def mergeMap[K, V: Semigroup](lhs: Map[K, V], rhs: Map[K, V]): Map[K, V] =
    lhs.foldLeft(rhs) {
      case (acc, (k, v)) => acc.updated(k, optionCombine(v, acc.get(k)))
    }

  // This function is provided in Cats as Monoid.combineAll
  //def combineAll[A: Monoid](as: List[A]): A = as.foldLeft(Monoid[A].empty)(Monoid[A].combine)

  val x = 1
  val y = 2
  val z = 3
  println(Semigroup[Int].combine(x, Semigroup[Int].combine(y, z)))
  println(Semigroup[Int].combine(Semigroup[Int].combine(x, y), z))
  println(1 |+| 2 |+| 3)


  val map1 = Map("hello" -> 0, "world" -> 1)
  val map2 = Map("hello" -> 2, "cats" -> 3)
  println(map1 |+| map2)

  val xm1 = Map('a' -> 1, 'b' -> 2)
  val xm2 = Map('b' -> 3, 'c' -> 4)
  println(mergeMap(xm1, xm2))

  val ym1 = Map(1 -> List("hello"))
  val ym2 = Map(2 -> List("cats"), 1 -> List("world"))
  println(mergeMap(ym1, ym2))

  val leftwards = List(1, 2, 3).foldLeft(0)(_ |+| _)
  val rightwards = List(4, 5, 6).foldRight(0)(_ |+| _)
  val result = leftwards |+| rightwards

  val v = 1
  println(Monoid[Int].combine(x, Monoid[Int].empty))
  println(Monoid[Int].combine(Monoid[Int].empty, x))
  println(x |+| Monoid[Int].empty)

  println(Monoid.combineAll(List(1, 2, 3)))
  Monoid.combineAll(List("hello", " ", "world"))
  println(Monoid.combineAll(List(Map('a' -> 1), Map('a' -> 2, 'b' -> 3), Map('b' -> 4, 'c' -> 5))))
  println(Monoid.combineAll(List(Set(1, 2), Set(2, 3, 4, 5))))

  // How to lift semigroup[a] into Option[A]
  import cats.Monoid
  import cats.data.NonEmptyList
  import cats.instances.option._
  val list = List(NonEmptyList(1, List(2, 3)), NonEmptyList(4, List(5, 6)))
  val lifted = list.map(nel => Option(nel))
  println(Monoid.combineAll(lifted))
}