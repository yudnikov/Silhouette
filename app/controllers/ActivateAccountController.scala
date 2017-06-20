package controllers

import java.net.URLDecoder
import java.util.UUID
import javax.inject.Inject

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.impl.providers.CredentialsProvider
import models.{ AuthToken, User }
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.mailer.Email
import play.api.mvc.Controller
import ru.yudnikov.core.postman.Postman
import utils.auth.DefaultEnv

import scala.concurrent.Future
import scala.language.postfixOps

class ActivateAccountController @Inject() (
  val messagesApi: MessagesApi,
  postman: Postman,
  silhouette: Silhouette[DefaultEnv],
  implicit val webJarAssets: WebJarAssets)
  extends Controller with I18nSupport {

  /**
   * Sends an account activation email to the user with the given email.
   *
   * @param email The email address of the user to send the activation mail to.
   * @return The result to display.
   */
  def send(email: String) = silhouette.UnsecuredAction.async { implicit request =>
    val decodedEmail = URLDecoder.decode(email, "UTF-8")
    val loginInfo = LoginInfo(CredentialsProvider.ID, decodedEmail)
    val result = Redirect(routes.SignInController.view()).flashing("info" -> Messages("activation.email.sent", decodedEmail))

    User.retrieve(loginInfo).flatMap {
      case Some(user) if !user.activated =>
        Future.successful(new AuthToken(user.reference)).map { authToken =>
          val url = routes.ActivateAccountController.activate(authToken.id).absoluteURL()
          postman.send(Email(
            subject = Messages("email.activate.account.subject"),
            from = Messages("email.from"),
            to = Seq(decodedEmail),
            bodyText = Some(views.txt.emails.activateAccount(user, url).body),
            bodyHtml = Some(views.html.emails.activateAccount(user, url).body)
          ))
          result
        }
      case None => Future.successful(result)
    }
  }

  /**
   * Activates an account.
   *
   * @param token The token to identify a user.
   * @return The result to display.
   */
  def activate(token: UUID) = silhouette.UnsecuredAction.async { implicit request =>
    AuthToken.validate(token).flatMap {
      case Some(authToken) =>
        User.retrieve(authToken.user.get.get.loginInfo).flatMap {
          case Some(user) if user.loginInfo.providerID == CredentialsProvider.ID =>
            Future.successful(user.copy(activated = true)).map { _ =>
              Redirect(routes.SignInController.view()).flashing("success" -> Messages("account.activated"))
            }
          case _ =>
            Future.successful(Redirect(routes.SignInController.view()).flashing("error" -> Messages("invalid.activation.link")))
        }
      case None =>
        Future.successful(Redirect(routes.SignInController.view()).flashing("error" -> Messages("invalid.activation.link")))
    }
  }
}
