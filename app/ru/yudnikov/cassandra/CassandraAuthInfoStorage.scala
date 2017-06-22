package ru.yudnikov.cassandra

import com.datastax.driver.core.ResultSet
import com.mohiva.play.silhouette.api.{ AuthInfo, LoginInfo }
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.persistence.daos.DelegableAuthInfoDAO
import ru.yudnikov.meta.{ Parser, Reflector }

import scala.collection.JavaConversions
import scala.concurrent.Future
import scala.reflect.ClassTag

class CassandraAuthInfoStorage[T <: AuthInfo: ClassTag] extends DelegableAuthInfoDAO[T] with Cassandra {

  private val aClass = implicitly[reflect.ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]
  private def tableName = aClass.getSimpleName

  private def createTable(aClass: Class[T]): Unit = {
    executeQuery(s"create table if not exists ${aClass.getSimpleName} (loginInfo varchar primary key, authInfo varchar)")
  }

  private def dropTable(aClass: Class[T]): Unit = {
    executeQuery(s"drop table if exists ${aClass.getSimpleName}")
  }

  def find(loginInfo: LoginInfo): Future[Option[T]] = {
    createTable(aClass.asInstanceOf[Class[T]])
    val res = executeQuery(s"select * from $tableName where loginInfo = '${Reflector.toString(loginInfo)}'") match {
      case rs: ResultSet =>
        JavaConversions.asScalaIterator(rs.iterator).toList match {
          case List(row) =>
            val x = row.get("authInfo", classOf[String])
            Some(Parser.parseFunction(x).execute().asInstanceOf[T])
          //None
          case _ =>
            None
        }
      case _ =>
        None
    }
    Future.successful(res)
  }

  def add(loginInfo: LoginInfo, authInfo: T): Future[T] = save(loginInfo, authInfo)

  def update(loginInfo: LoginInfo, authInfo: T): Future[T] = save(loginInfo, authInfo)

  def save(loginInfo: LoginInfo, authInfo: T): Future[T] = {
    createTable(aClass)
    Future.successful {
      executeQuery(s"insert into $tableName (loginInfo, authInfo) values ('${Reflector.toString(loginInfo)}', '${Reflector.toString(authInfo)}')")
      authInfo
    }
  }

  def remove(loginInfo: LoginInfo): Future[Unit] = {
    Future.successful(executeQuery(s"delete * from $tableName where loginInfo = ${loginInfo.toString}"))
  }

}
