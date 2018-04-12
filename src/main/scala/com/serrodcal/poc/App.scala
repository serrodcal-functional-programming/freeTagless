package com.serrodcal.poc

import cats.implicits._
import java.util.UUID

import cats.Monad

import scala.concurrent.{Await, Future}

case class User(id: UUID, loyaltyPoints: Int)

trait UserRepository[F[_]] {
  def findUser(id: UUID): F[Option[User]]
  def updateUser(user: User): F[Unit]
}

class LoyaltyPoints[F[_]: Monad](userRepository: UserRepository[F]) {
  def addPoints(userId: UUID, pointsToAdd: Int): F[Either[String, Unit]] = {
    userRepository.findUser(userId).flatMap {
      case None => implicitly[Monad[F]].pure(Left("User not found"))
      case Some(user) => {
        val updated = user.copy(userId, user.loyaltyPoints + pointsToAdd)
        userRepository.updateUser(updated).map(_ => Right(()))
      }
    }
  }
}

trait FutureUserRepository extends UserRepository[Future] {
  override def findUser(id: UUID): Future[Option[User]] = Future.successful(None)
  override def updateUser(user: User): Future[Unit] = Future.successful()
}

object Main extends App {

  val result: Future[Either[String, Unit]] = new LoyaltyPoints(new FutureUserRepository {}).addPoints(UUID.randomUUID(), 10)

}
