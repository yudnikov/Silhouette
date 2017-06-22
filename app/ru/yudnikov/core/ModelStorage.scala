package ru.yudnikov.core

import java.util.UUID

import scala.reflect.ClassTag

/**
 * Created by Don on 20.06.2017.
 */
trait ModelStorage {

  def save[M <: Model](model: M): Unit
  def list[M <: Model: ClassTag]: List[M]
  def load[M <: Model: ClassTag](id: UUID): Option[M]
  def remove[M <: Model](model: M): Unit

}