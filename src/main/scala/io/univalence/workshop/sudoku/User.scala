package io.univalence.workshop.sudoku

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class User(
    name: String,
    following: ListBuffer[User] = ListBuffer.empty
) {
  def follows(otherUser: User): Unit = following.addOne(otherUser)

  def isFollowing(otherUser: User): Boolean =
    following.contains(otherUser)

  override def toString: String =
    s"User(name=$name, following=${following.map(_.name)})"
}
object User {
  def printFollowing(fromUser: User): Unit = {
    printFollowing(fromUser, mutable.Queue(fromUser), Set.empty)
  }

  def printFollowing(
      targetUser: User,
      toVisit: mutable.Queue[User],
      visited: Set[User]
  ): Unit = {
    while (toVisit.nonEmpty) {
      val currentUser = toVisit.dequeue()

      if (currentUser.isFollowing(targetUser))
        println(">>> " + currentUser)
      if (!visited.contains(currentUser)) {
        printFollowing(
          targetUser,
          toVisit.enqueueAll(currentUser.following),
          visited + currentUser
        )
      }
    }
  }

  def printFollowing2(fromUser: User): Unit = {
    printFollowing2(fromUser, fromUser, Set.empty)
  }

  def printFollowing2(targetUser: User, currentUser: User, visited: Set[User]): Set[User] = {
    val newVisited: Set[User] = visited + currentUser

    if (!visited.contains(currentUser)) {
      if (currentUser.isFollowing(targetUser))
        println(currentUser)

      currentUser.following.foldLeft(newVisited) { case (v, fu) =>
        printFollowing2(targetUser, fu, v)
      }
    } else {
      newVisited
    }
  }

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

    val users = List(pierre, sauguy, francois, brahim)
    users.foreach(println)

    User.printFollowing(brahim)

    println("-----")
    println("following sauguy:")
    User.printFollowing2(sauguy)
    println("following francois:")
    User.printFollowing2(francois)
  }
}
