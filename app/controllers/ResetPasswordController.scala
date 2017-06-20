package controllers

import java.util.UUID
import javax.inject.Inject

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.util.{ PasswordHasherRegistry, PasswordInfo }
import com.mohiva.play.silhouette.impl.providers.CredentialsProvider
import forms.ResetPasswordForm
import models.AuthToken
import models.User
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Controller
import utils.auth.DefaultEnv

import scala.concurrent.Future

/**
 * The `Reset Password` controller.
 *
 * @param messagesApi            The Play messages API.
 * @param silhouette             The Silhouette stack.
 * @param authInfoRepository     The auth info repository.
 * @param passwordHasherRegistry The password hasher registry.
 * @param webJarAssets           The WebJar assets locator.
 */
class ResetPasswordController @Inject() (
  val messagesApi: MessagesApi,
  silhouette: Silhouette[DefaultEnv],
  authInfoRepository: AuthInfoRepository,
  passwordHasherRegistry: PasswordHasherRegistry,
  implicit val webJarAssets: WebJarAssets)
  extends Controller with I18nSupport {

  /**
   * Views the `Reset Password` page.
   *
   * @param id The token to identify a user.
   * @return The result to display.
   */
  def view(id: UUID) = silhouette.UnsecuredAction.async { implicit request =>
    AuthToken.validate(id).map {
      case Some(authToken) => Ok(views.html.resetPassword(ResetPasswordForm.form, id))
      case None => Redirect(routes.SignInController.view()).flashing("error" -> Messages("invalid.reset.link"))
    }
  }

  /**
   * Resets the password.
   *
   * @param id The token to identify a user.
   * @return The result to display.
   */
  def submit(id: UUID) = silhouette.UnsecuredAction.async { implicit request =>
    AuthToken.validate(id).flatMap {
      case Some(authToken) =>
        ResetPasswordForm.form.bindFromRequest.fold(
          form => Future.successful(BadRequest(views.html.resetPassword(form, id))),
          password => User.retrieve(authToken.user.get.get.loginInfo).flatMap {
            case Some(user) if user.loginInfo.providerID == CredentialsProvider.ID =>
              val passwordInfo = passwordHasherRegistry.current.hash(password)
              authInfoRepository.update[PasswordInfo](user.loginInfo, passwordInfo).map { _ =>
                Redirect(routes.SignInController.view()).flashing("success" -> Messages("password.reset"))
              }
            case _ => Future.successful(Redirect(routes.SignInController.view()).flashing("error" -> Messages("invalid.reset.link")))
          }
        )
      case None => Future.successful(Redirect(routes.SignInController.view()).flashing("error" -> Messages("invalid.reset.link")))
    }
  }
}
