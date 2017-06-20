package ru.yudnikov.core.postman

import play.api.libs.mailer

/**
 * Created by Don on 20.06.2017.
 */
trait Postman {
  def send(email: mailer.Email)
}