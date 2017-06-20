package jobs

import javax.inject.Inject

import akka.actor._
import com.mohiva.play.silhouette.api.util.Clock
import jobs.AuthTokenCleaner.Clean
//import models.services.AuthTokenService
import utils.Logger

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * A job which cleanup invalid auth tokens.
 *
 * @param service The auth token service implementation.
 * @param clock The clock implementation.
 */
class AuthTokenCleaner @Inject() (

  //service: AuthTokenService,
  clock: Clock)
  extends Actor with Logger {

  /**
   * Process the received messages.
   */
  def receive: Receive = {

    case Clean =>
  }
}

/**
 * The companion object.
 */
object AuthTokenCleaner {
  case object Clean
}
