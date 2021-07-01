package io.univalence.workshop

object Variance {

  final case class Box[+A](a: A) {
    def map_[B](f: A => B): Box[B] = Box(f(a))
    def map[B](f: A => B): Box[B] = {
//      flatMap(x => Box(f(x)))
//      flatMap(f andThen Box.apply)
      flatMap(Box.apply[B] _ compose f)
    }

    def flatMap[B](f: A => Box[B]): Box[B] = f(a)

  }

  sealed trait Option[+A] {
    import Option._

    def map_[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(x) => Some(f(x))
    }

    def map[B](f: A => B): Option[B] =
      flatMap(x => Some(f(x)))

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(x) => f(x)
    }

    def filter(p: A => Boolean): Option[A] = ???

    def fold[B >: A](init: B)(f: (B, A) => B): B = ???

  }
  object Option {
    case class Some[A](a: A) extends Option[A]
    case object None extends Option[Nothing]
  }

  trait Either[+L, +R] {
    import Either._

    def map[B](f: R => B): Either[L, B] = this match {
      case Left(x) => Left(x)
      case Right(x) => Right(f(x))
    }

    def flatMap[B, C >: L](f: R => Either[C, B]): Either[C, B] = this match {
      case Left(x) => Left(x)
      case Right(x) => f(x)
    }

    def bimap[C, B](g: L => C, f: R => B): Either[C, B] = this match {
      case Left(x) => Left(g(x))
      case Right(x) => Right(f(x))
    }


  }
  object Either {
    case class Left[L](left: L) extends Either[L, Nothing]
    case class Right[R](right: R) extends Either[Nothing, R]
  }

  case class Function[-In, +Out](apply: In => Out) {

    def map[B](f: Out => B): Function[In, B] =
//    Function(x => f(apply(x)))
      Function(f compose apply)

    def contramap[B](f: B => In): Function[B, Out] =
//    Function(x => apply(f(x)))
      Function(apply compose f)

    def promap[B, C](f: B => In, g: Out => C): Function[B, C] =
//    Function(x => g(apply(f(x))))
      Function(g compose apply compose f)

  }

  def main(args: Array[String]): Unit = {
    import Option._

    println(Box(5).map(_ * 2))                           // => Box(10)
    println(Box(1).flatMap(i => Box(i + 2)))             // => Box(3)
    println(Box(1).flatMap(i => Box(2).map(j => i + j))) // => Box(3)

    println(None.asInstanceOf[Option[Int]].map(_ * 2))                 // => None
    println(None.asInstanceOf[Option[Int]].flatMap(_ => None))         // => None
    println(None.asInstanceOf[Option[Int]].flatMap(i => Some(i + 2)))  // => None
    println(Some(5).map(_ * 2))                                        // => Some(10)
    println(Some(1).asInstanceOf[Option[Int]].flatMap(_ => None))      // => None
    println(Some(1).flatMap(i => Some(i + 2)))                         // => Some(3)

    def addOptions(oa: Option[Int], ob: Option[Int]): Option[Int] = oa.flatMap(a => ob.map(b => a + b))

    println(addOptions(Some(1), None))    // => None
    println(addOptions(None, Some(1)))    // => None
    println(addOptions(Some(1), Some(2))) // => Some(3)

    val multTwo: Function[Int, Int] = Function((n: Int) => n * 2)

    val multTwoOnString: Function[String, String] = multTwo.promap((s: String) => s.toInt, _.toString)

    val result: String = multTwoOnString.apply("123")
    println(result)
  }


}
