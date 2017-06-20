package models

import java.util.UUID

import org.joda.time.{ DateTime, DateTimeZone }
import ru.yudnikov.core.{ Manager, Model, Reference }

import scala.concurrent.Future

case class AuthToken(
  user: Reference[User],
  expiry: DateTime = DateTime.now.withZone(DateTimeZone.UTC).plusMinutes(5),
  id: UUID = UUID.randomUUID()
) extends Model(AuthToken) {

}

object AuthToken extends Manager[AuthToken] {

  def validate(id: UUID): Future[Option[AuthToken]] = Future.successful(get(id))

}