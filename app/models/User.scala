package models

import java.util.UUID

import com.mohiva.play.silhouette.api.{ Identity, LoginInfo }
import com.mohiva.play.silhouette.api.services.IdentityService
import ru.yudnikov.core.{ Manager, Model }

import scala.concurrent.Future

case class User(
  loginInfo: LoginInfo,
  firstName: Option[String],
  lastName: Option[String],
  fullName: Option[String],
  email: Option[String],
  avatarURL: Option[String],
  activated: Boolean = false,
  id: UUID = UUID.randomUUID()
) extends Model(User) with Identity {

  def name = fullName.orElse {
    firstName -> lastName match {
      case (Some(f), Some(l)) => Some(f + " " + l)
      case (Some(f), None) => Some(f)
      case (None, Some(l)) => Some(l)
      case _ => None
    }
  }
}

object User extends Manager[User] with IdentityService[User] {
  override def retrieve(loginInfo: LoginInfo): Future[Option[User]] = Future.successful(find(_.email.contains(loginInfo.providerKey)))
}
