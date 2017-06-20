package ru.yudnikov.core

import java.util.UUID

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

abstract class Manager[+M <: Model: ClassTag] {

  //def storage: Storage

  private[this] var models: Map[UUID, M] = Map()
  private[this] var persistent: Map[UUID, M] = Map()
  private[this] val aClass: Class[M] = implicitly[reflect.ClassTag[M]].runtimeClass.asInstanceOf[Class[M]]

  def update(model: Model): Unit = models = models + (model.id -> model.asInstanceOf[M])
  def get(id: UUID): Option[M] = models.get(id) match {
    case None => None //storage.load(id)
    case Some(x) => Some(x)
  }
  def list: List[M] = models.values.toList // storage.list
  def list(f: M => Boolean): List[M] = models.values.filter(f).toList
  def find(f: M => Boolean): Option[M] = models.values.find(f)

  def save(model: Model): Unit = {
    val p = persistent.get(model.id)
    if (p.isEmpty || p.get != model) {
      //storage.save(model.asInstanceOf[M])
      persistent = persistent + (model.id -> model.asInstanceOf[M])
    } else
      println(s"model $model is already saved")
  }

  override def toString: String = {
    getClass.getName
  }
}

trait Storage {

  def save[M <: Model](model: M): Unit
  def list[M <: Model: ClassTag]: List[M]
  def load[M <: Model: ClassTag](id: UUID): Option[M]
  def remove[M <: Model](model: M): Unit

}
