package ru.yudnikov.cassandra

import java.io.File
import java.util.UUID

import com.datastax.driver.core.{ Cluster, ResultSet, Row, Session }
import com.typesafe.config.ConfigFactory

import ru.yudnikov.core._
import ru.yudnikov.meta.{ Description, Parser, Reflector }

import scala.collection.JavaConversions
import scala.reflect.ClassTag

object CassandraModelStorage extends Cassandra with ModelStorage {

  private class Column(val name: String, val description: Description) {

    val defaultSerializer: (Description) => String = _.value.toString

    // (type, serializer, parse from String, class)
    lazy val map: (String, (Description) => String, Boolean, Class[_]) = description.aClass.getName match {
      case s: String if s.endsWith(".Boolean") =>
        ("boolean", defaultSerializer, false, classOf[java.lang.Boolean])
      case s: String if s.endsWith(".Int") | s.endsWith(".Integer") =>
        ("int", defaultSerializer, false, classOf[java.lang.Integer])
      case s: String if s.endsWith(".Double") =>
        ("double", defaultSerializer, false, classOf[java.lang.Double])
      case s: String if s.endsWith(".Long") =>
        ("double", defaultSerializer, false, classOf[java.lang.Double])
      case s: String if s.endsWith(".Date") =>
        ("timestamp", d => Reflector.serializer(d.aClass)(d.value), false, classOf[java.util.Date])
      case s: String if s.endsWith(".String") =>
        ("varchar", d => s"'${d.value}'", false, classOf[java.lang.String])
      case _ if description.isReference | name == "id" =>
        ("uuid", _.value.toString, false, classOf[java.util.UUID])
      case _ =>
        ("varchar", d => s"'$d'", true, classOf[java.lang.String])
    }

    def getSuffix: String = if (name == "id") "primary key" else ""

    lazy val value: String = map._2(description)

    override def toString: String = s"$name ${map._1} $getSuffix".trim
  }

  private def createTable[M <: Model](aClass: Class[M], withCollections: Boolean = true): Unit = {
    val ds = Reflector.describe(aClass)
    val part = ds.partition(t => t._2.isReferenceCollection)
    val columns = part._2.map(t => new Column(t._1, t._2)).toList
    executeQuery(s"create table if not exists ${aClass.getSimpleName} (${columns.mkString(", ")});")
    if (withCollections) {
      for (rc <- part._1) {
        val thisClass = aClass.getSimpleName
        val refClass = rc._2.referredClass.get.getSimpleName
        executeQuery(s"create table if not exists ${thisClass}_$refClass (${thisClass}_id uuid, ${refClass}_id uuid, primary key(${thisClass}_id, ${refClass}_id));")
      }
    }
  }

  private def dropTable[M <: Model](aClass: Class[M], withCollections: Boolean = true): Unit = {
    if (withCollections) {
      val ds = Reflector.describe(aClass)
      val part = ds.partition(t => t._2.isReferenceCollection)
      for (refCollection <- part._1) {
        val thisClass = aClass.getSimpleName
        val refClass = refCollection._2.referredClass.get.getSimpleName
        executeQuery(s"drop table if exists ${thisClass}_$refClass;")
      }
    }
    executeQuery(s"drop table if exists ${aClass.getSimpleName};")
  }

  private def fetchRow[M <: Model](aClass: Class[M], row: Row): M = {
    val id = row.get("id", classOf[UUID])
    val ds = Reflector.describe(aClass)

    def getReferenceCollection(t: (String, Description)): List[Reference[Model]] = {
      val rc = t
      val thisClass = aClass.getSimpleName
      val refClass = rc._2.referredClass.get.getSimpleName
      val collection: List[Reference[Model]] = executeQuery(s"select * from ${thisClass}_$refClass where ${thisClass}_id = $id") match {
        case rs: ResultSet =>
          val iterator = JavaConversions.asScalaIterator(rs.iterator)
          val res = for {
            r <- iterator
          } yield {
            val id = r.get(s"${refClass}_id", classOf[UUID])
            val refclass = rc._2.referredClass
            new Reference[Model](id, refclass.get.asInstanceOf[Class[Model]])
          }
          res.toList
        case _ =>
          Nil
      }
      collection
    }

    def getArgument(t: (String, Description)) = {
      val column = new Column(t._1, t._2)
      if (column.description.isReference)
        new Reference[M](row.get("id", classOf[UUID]), aClass, Reflector.getManager(aClass))
      else {
        if (column.map._3) {
          val result = Parser.parseFunction(row.get(column.name, classOf[String])).execute()
          result
        } else {
          val result = row.get(column.name, column.map._4)
          result
        }
      }
    }

    val args = ds.map(t => {
      if (t._2.isReferenceCollection) {
        getReferenceCollection(t)
      } else {
        getArgument(t)
      }
    }).toList

    Reflector.instantiate(aClass, args)
  }

  override def save[M <: Model](model: M): Unit = {
    val aClass = model.getClass
    // dropTable(aClass)
    createTable(aClass)
    val ds = Reflector.describe(model)
    val part = ds.partition(t => t._2.isReferenceCollection)
    val columns = part._2.map(t => new Column(t._1, t._2)).toList
    columns.filter(_.description.isReference).foreach(_.description.reference.get.get.get.save())
    // todo save all models from mixed collections
    executeQuery(s"insert into ${aClass.getSimpleName} (${columns.map(_.name).mkString(", ")}) values (${columns.map(_.value).mkString(", ")});")
    for (refCollection <- part._1) {
      val thisClass = aClass.getSimpleName
      val refClass = refCollection._2.referredClass.get.getSimpleName
      val collection = refCollection._2.value.asInstanceOf[Iterable[Reference[Model]]]
      for (item <- collection) {
        item.get.get.save()
        executeQuery(s"insert into ${thisClass}_$refClass (${thisClass}_id, ${refClass}_id) values (${model.id}, ${item.id});")
      }
    }
  }

  override def load[M <: Model: ClassTag](id: UUID): Option[M] = {
    val aClass = implicitly[reflect.ClassTag[M]].runtimeClass.asInstanceOf[Class[M]]
    executeQuery(s"select * from ${aClass.getSimpleName} where id = $id") match {
      case rs: ResultSet => Some(JavaConversions.asScalaIterator(rs.iterator).map(fetchRow(aClass, _)).toList.head)
      case _ => None
    }
  }

  override def remove[M <: Model](model: M): Unit = {
    executeQuery(s"delete * from ${model.getClass.getSimpleName} where id = ${model.id} if exists")
  }

  override def list[M <: Model: ClassTag]: List[M] = {
    val aClass = implicitly[reflect.ClassTag[M]].runtimeClass.asInstanceOf[Class[M]]
    executeQuery(s"select * from ${aClass.getSimpleName};") match {
      case rs: ResultSet => JavaConversions.asScalaIterator(rs.iterator).map(fetchRow(aClass, _)).toList
      case _ => List()
    }
  }

}
