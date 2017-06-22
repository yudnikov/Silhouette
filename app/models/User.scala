package models

import java.util.UUID
import javax.inject.Inject

import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.{ AuthInfo, Identity, LoginInfo }
import com.mohiva.play.silhouette.api.services.IdentityService
import com.mohiva.play.silhouette.api.util.PasswordInfo
import com.mohiva.play.silhouette.persistence.daos.{ DelegableAuthInfoDAO, InMemoryAuthInfoDAO }
import ru.yudnikov.core.{ Manager, Model }

import scala.concurrent.Future
import scala.reflect.ClassTag

case class User(
  loginInfo: LoginInfo,
  firstName: Option[String],
  lastName: Option[String],
  fullName: Option[String],
  email: Option[String],
  avatarURL: Option[String],
  isActivated: Boolean = false,
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

  trait AuthInfoStorage {
    def load[T <: AuthInfo: ClassTag](loginInfo: LoginInfo): Option[T]
  }

  override def retrieve(loginInfo: LoginInfo): Future[Option[User]] =
    Future.successful {
      find(_.email.contains(loginInfo.providerKey)) match {
        case Some(user) =>
          Some(user)
        case None =>
          // todo For instance we have Some(String(email@m.ru)) but asking for String(email@m.ru) - whe should take care?!
          find("email", Some(loginInfo.providerKey))
      }
    }

  override def update(model: Model): Unit = {
    /*
    val user = model.asInstanceOf[User]
    if (user.isActivated) save(user)
    */
    super.update(model)
  }

}
