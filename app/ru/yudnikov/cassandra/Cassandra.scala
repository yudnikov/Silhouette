package ru.yudnikov.cassandra

import java.io.File

import com.datastax.driver.core.{ Cluster, ResultSet, Session }
import com.typesafe.config.ConfigFactory
import ru.yudnikov.cassandra.CassandraModelStorage.{ executeQuery, keyspace, session }

/**
 * Created by Don on 21.06.2017.
 */
trait Cassandra {

  private val confFileName: String = "cassandra.conf"
  private val conf = ConfigFactory.parseFile(new File(s"conf/$confFileName"))

  protected val host: String = conf.getString("cassandra.host")
  protected val port: Int = conf.getInt("cassandra.port")
  protected val keyspace: String = conf.getString("cassandra.keyspace")

  lazy protected val cluster: Cluster = Cluster.builder().addContactPoint(host).withPort(port).build()
  lazy protected val session: Session = cluster.connect(keyspace)

  protected def executeQuery(query: String): ResultSet = {
    println(s"[Cassandra]: $query")
    try
      session.execute(query)
    catch {
      case e: Exception =>
        e.printStackTrace()
        null
    }
  }

  // executeQuery(s"create keyspace if not exists $keyspace with replication = { 'class' : 'SimpleStrategy', 'replication_factor' : 3 };")

}
