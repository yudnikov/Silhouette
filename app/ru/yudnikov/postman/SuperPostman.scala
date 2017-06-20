package ru.yudnikov.postman

import javax.inject.Inject

import org.simplejavamail.email.{ Email, EmailBuilder }
import org.simplejavamail.mailer.Mailer
import org.simplejavamail.mailer.config.{ ProxyConfig, ServerConfig, TransportStrategy }
import play.api.Configuration
import play.api.libs.mailer

/**
 * Created by Don on 02.06.2017.
 */
class SuperPostman @Inject() (val conf: Configuration) extends Postman {

  val serverConfig = new ServerConfig(
    conf.underlying.getString("superpostman.server.host"),
    conf.underlying.getInt("superpostman.server.port"),
    conf.underlying.getString("superpostman.server.username"),
    conf.underlying.getString("superpostman.server.password")
  )

  val proxyConfig = new ProxyConfig(
    conf.underlying.getString("superpostman.proxy.host"),
    conf.underlying.getInt("superpostman.proxy.port")
  )

  val m = new Mailer(
    serverConfig,
    TransportStrategy.valueOf(conf.underlying.getString("superpostman.transportStrategy")),
    proxyConfig
  )

  def toEmail(email: mailer.Email): Email =
    new EmailBuilder().from(
      conf.underlying.getString("application.name"),
      conf.underlying.getString("superpostman.server.username"))
      .to(email.to: _*)
      .subject(email.subject)
      .text(email.bodyText.get)
      .build()

  override def send(email: mailer.Email): Unit = m.sendMail(toEmail(email), true)

}
