package ru.yudnikov.core

import java.util.UUID

import ru.yudnikov.meta.{ Description, Reflector }

import scala.reflect.ClassTag

/**
 * Created by Don on 11.06.2017.
 */
abstract class Model(private val manager: Manager[Model]) {

  private val hashRoot = 41

  val id: UUID
  def update(): Unit = manager.update(this)
  def reference[M <: Model]: Reference[M] = new Reference(id, getClass.asInstanceOf[Class[M]])
  def save(): Unit = manager.save(this)

  override def toString: String = {
    s"${getClass.getName}(${Reflector.describe(this).values.map(_.toString).mkString(",")})"
  }

  update()

  private lazy val description: Map[String, Description] = Reflector.describe(this)

  def getDescription: Map[String, Description] = description

  override def hashCode(): Int = hashRoot * id.hashCode * getClass.hashCode

  override def equals(obj: scala.Any): Boolean =
    if (obj.getClass == getClass && obj.asInstanceOf[Model].description == description)
      true
    else
      false

}

