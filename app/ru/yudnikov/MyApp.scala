package ru.yudnikov

import com.mohiva.play.silhouette.api.LoginInfo
import models.User
import ru.yudnikov.meta.Parser

/**
 * Created by Don on 20.06.2017.
 */
object MyApp extends App {

  /*val u = new User(new LoginInfo("credentials", "m@mail.ru"), Some("Oleg"), Some("Pukan"), Some("Oleg Pukan"), Some("m@mail.ru"), None)
  println(u.toString)
  println(Parser.parse[User](u.toString))*/
  val l = User.list
  println(l)

}
