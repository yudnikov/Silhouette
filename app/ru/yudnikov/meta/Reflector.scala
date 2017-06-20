package ru.yudnikov.meta

import java.util.Date

import org.joda.time.DateTime
import ru.yudnikov.core.{ Manager, Model }

import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

/**
 * Created by Don on 20.06.2017.
 */
object Reflector {

  lazy val runtimeMirror: RuntimeMirror = universe.runtimeMirror(getClass.getClassLoader)

  def getType(aClass: Class[_]): Type = runtimeMirror.classSymbol(aClass).toType

  def getPrimaryConstructor(aClass: Class[_]): MethodSymbol = {
    val aType = getType(aClass)
    aType.decl(termNames.CONSTRUCTOR).asTerm.alternatives.collectFirst {
      case ms: MethodSymbol if ms.isPrimaryConstructor => ms
    }.get
  }

  def instantiate[M <: Model](aClass: Class[M], paramLists: List[Any]): M = {

    val primaryConstructor = getPrimaryConstructor(aClass)
    assume(primaryConstructor.paramLists.flatten.length == paramLists.length, "required and provided argument list's lengths must be equal")
    val classMirror = runtimeMirror.reflectClass(runtimeMirror.classSymbol(aClass))
    val constructorMirror = classMirror.reflectConstructor(primaryConstructor)

    def check(params: List[Symbol], args: List[Any]): Boolean = {
      val equal = if (params.nonEmpty && args.nonEmpty) {
        val c1 = runtimeMirror.runtimeClass(params.head.typeSignature.typeSymbol.asClass)
        val c2 = args.head.getClass
        val res = c1 == c2 | c1.isAssignableFrom(c2)
        res
      } else
        false
      if (params.tail.nonEmpty && args.tail.nonEmpty)
        equal && check(params.tail, args.tail)
      else
        equal
    }

    val x = check(primaryConstructor.paramLists.flatten, paramLists)

    //assume(x)

    constructorMirror.apply(paramLists: _*).asInstanceOf[M]
  }

  def getManager[M <: Model](aClass: Class[M]): Manager[M] = {
    val classMirror = runtimeMirror.reflectClass(runtimeMirror.classSymbol(aClass))
    runtimeMirror.reflectModule(classMirror.symbol.companion.asModule).instance.asInstanceOf[Manager[M]]
  }

  def getObject(fullName: String): Any = runtimeMirror.reflectModule(runtimeMirror.staticModule(fullName)).instance

  def getManager[M <: Model](fullName: String): Manager[M] = getObject(fullName).asInstanceOf[Manager[M]]

  def getArgs[M <: Model](aClass: Class[M]): List[String] = {
    val pc = getPrimaryConstructor(aClass)
    // args - are necessaries to instantiate some class, ordered as "instantiate" method needs
    pc.paramLists.flatten.collect {
      case ts: TermSymbol => ts.name.toString
    }
  }

  def getTerms[M <: Model](aClass: Class[M]): Map[String, TermSymbol] = {
    val aType = getType(aClass)
    // terms - available fields of instance
    aType.decls.collect {
      case ts: TermSymbol if ts.isPublic & !ts.isConstructor => ts.name.toString -> ts
    }.toMap
  }

  def describe[M <: Model](model: M): ListMap[String, Description] = {
    val aClass = model.getClass
    val args = getArgs(aClass)
    val terms = getTerms(aClass)
    // we can describe an instance when and only when number of args equals to number of terms
    assume(args.size == terms.size)
    val instanceMirror = runtimeMirror.reflect(model.asInstanceOf[Model])
    val values: Map[String, (Type, Any)] = args.map(s =>
      (s, (terms(s).typeSignature.finalResultType, instanceMirror.reflectField(terms(s)).get))).toMap
    ListMap[String, Description](args.map(s => s -> new Description(values(s)._1, values(s)._2)): _*)
  }

  def describe[M <: Model](aClass: Class[M]): ListMap[String, Description] = {
    val args = getArgs(aClass)
    val terms = getTerms(aClass)
    // we can describe an instance when and only when number of args equals to number of terms
    assume(args.size == terms.size)
    //ListMap[String, Description](args.map(s => s -> new Description(values(s)._1, values(s)._2)): _*)
    ListMap[String, Description](args.map(s =>
      s -> new Description(terms(s).typeSignature.finalResultType, null)): _*)
  }

  def isAssignableFrom(aClass: Class[_]): Type => Boolean = _.baseClasses.contains(runtimeMirror.classSymbol(aClass))

  def serializer(aClass: Class[_]): Any => String = aClass match {
    case _ if aClass == classOf[Date] =>
      value => value.asInstanceOf[Date].getTime.toString
    case _ if aClass == classOf[DateTime] =>
      value => value.asInstanceOf[DateTime].getMillis.toString
    case _ =>
      value => value.toString
  }

}

