package io.univalence.workshop

object sumof {

  def sumOf(values: List[Option[Int]]): Option[Int] = {
    Option(values.filter(_.isDefined).map(_.get).sum)
  }


  def sumOf2(values: List[Option[Int]]) : Option[Int] = {
     values.fold(Some(0)) {
       case (None, _) => None
       case (_, None) => None
       case (Some(x), Some(y)) => Some(x + y)
     }
  }

  def main(args: Array[String]): Unit = {
    println(sumOf2(List(Option(1), Option(4), None)))
    println(sumOf2(List(Option(1), Option(4))))
    println(sumOf2(List(Option(1), None, Option(4))))
  }
}
