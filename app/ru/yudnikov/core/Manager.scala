package ru.yudnikov.core

import java.util.UUID
import javax.inject.Inject

import play.DefaultApplication
import play.api.{ Application, Play }
import ru.yudnikov.cassandra.CassandraModelStorage

import scala.reflect.ClassTag

/**
 * Created by Don on 20.06.2017.
 */
abstract class Manager[+M <: Model: ClassTag] {

  val storage: ModelStorage = CassandraModelStorage

  private[this] var models: Map[UUID, M] = Map()
  private[this] var persistent: Map[UUID, M] = Map()
  //private[this] val aClass: Class[M] = implicitly[reflect.ClassTag[M]].runtimeClass.asInstanceOf[Class[M]]

  def update(model: Model): Unit = models = models + (model.id -> model.asInstanceOf[M])

  def get(id: UUID): Option[M] = models.get(id) match {
    case None => storage.load(id)
    case Some(x) => Some(x)
  }

  def list: List[M] = storage.list

  def list(f: M => Boolean): List[M] = models.values.filter(f).toList

  def find(f: M => Boolean): Option[M] = models.values.find(f)

  def find(name: String, value: Any): Option[M] = storage.find[M](name, value) match {
    case List(x) => Some(x)
    case _ => None
  }

  def findAll(name: String, value: Any): List[M] = storage.find[M](name, value)

  def save(model: Model): Unit = {
    val p = persistent.get(model.id)
    if (p.isEmpty || p.get != model) {
      storage.save(model.asInstanceOf[M])
      persistent = persistent + (model.id -> model.asInstanceOf[M])
    } else {
      println(s"model $model is already saved")
    }
  }

  override def toString: String = {
    getClass.getName
  }
}
