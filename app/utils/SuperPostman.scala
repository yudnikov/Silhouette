package utils

import javax.inject.Inject

import org.simplejavamail.email.{ Email, EmailBuilder }
import org.simplejavamail.mailer.Mailer
import org.simplejavamail.mailer.config.{ ProxyConfig, ServerConfig, TransportStrategy }
import play.api.libs.mailer
import play.api.{ Configuration, Play }

/**
 * Created by Don on 02.06.2017.
 */
object SuperPostman extends App {

  val sc = new ServerConfig("mail2torx3jqgcpm.onion", 25, "putana@mail2tor.com", "1234qwerasdfT")
  val mailer = new Mailer(sc, TransportStrategy.SMTP_PLAIN, new ProxyConfig("127.0.0.1", 9050))

  val email = new EmailBuilder()
    .from("Tor Store", "putana@mail2tor.com")
    .to("putana@mail2tor.com")
    .subject("My Bakery is finally open!")
    .text("Mom, Dad. We did the opening ceremony of our bakery!!!")
    .build()

  mailer.sendMail(email)

}

trait Postman {

  def send(email: mailer.Email)
}

class SuperPostman @Inject() (val conf: Configuration) extends Postman {

  val serverConfig = new ServerConfig(
    conf.underlying.getString("superpostman.server.host"),
    conf.underlying.getInt("superpostman.server.port"),
    conf.underlying.getString("superpostman.server.username"),
    conf.underlying.getString("superpostman.server.password")
  )

  val proxyConfig = new ProxyConfig(
    conf.underlying.getString("superpostman.server.proxy.host"),
    conf.underlying.getInt("superpostman.server.proxy.port")
  )

  val m = new Mailer(
    serverConfig,
    TransportStrategy.valueOf(conf.underlying.getString("superpostman.server.transportStrategy")),
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
