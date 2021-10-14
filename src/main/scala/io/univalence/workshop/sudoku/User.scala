package io.univalence.workshop.sudoku

import scala.collection.mutable.ListBuffer

case class User(
    name: String,
    following: ListBuffer[User] = ListBuffer.empty
) {
  def follows(otherUser: User): Unit = following += otherUser
}
object User {
  def printFollowing(fromUser: User): Unit = ???

  def followers(targetUser: User, db: Set[User]): Set[User] = ???

  def level2Following(fromUser: User): Set[User] = ???
}

object UserMain {
  def main(args: Array[String]): Unit = {
    val pierre   = User("Pierre")
    val sauguy   = User("Sauguy")
    val brahim   = User("Brahim")
    val francois = User("François")

    pierre.follows(sauguy)
    sauguy.follows(brahim)
    pierre.follows(francois)
    sauguy.follows(francois)
    brahim.follows(francois)
    francois.follows(pierre)

    User.printFollowing(brahim)
  }
}
