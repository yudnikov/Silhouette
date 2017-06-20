package ru.yudnikov.core

import java.util.UUID

import ru.yudnikov.meta.Reflector

/**
 * Created by Don on 20.06.2017.
 */
class Reference[+M <: Model](val id: UUID, private[this] val aClass: Class[M], val manager: Manager[M]) {

  private val hashRoot = 41

  def this(id: UUID, aClass: Class[M]) {
    this(id, aClass, Reflector.getManager(aClass))
  }
  //def this(model: M, manager: Manager[M]) = this(model.id, model.getClass.asInstanceOf[Class[M]], manager)

  def get: Option[M] = manager.get(id)

  override def toString: String = s"$id"

  override def hashCode(): Int = hashRoot * id.hashCode * aClass.hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case r: Reference[M] => r.id == id && r.manager == manager
    case _ => false
  }
}
