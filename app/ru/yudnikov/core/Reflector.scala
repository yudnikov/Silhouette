package ru.yudnikov.core

import java.util.{ Date, UUID }

import models._
import org.joda.time.DateTime
import ru.yudnikov.core.Reflector._

import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

/**
 * Created by igor.yudnikov on 05-Jun-17.
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

object Parser {

  class Node(name: String, args: List[Node] = List()) {
    val isArgument: Boolean = args.isEmpty
    val isPrimitive: Boolean = !args.exists(!_.isArgument)

    def execute(): Any = {
      if (!isArgument) {
        val aClass = Class.forName(name)
        //println(s"executing $aClass")
        val result =
          if (isPrimitive) {
            fromStringer(aClass) match {
              case Some(f) =>
                f(args.map(_.execute()).asInstanceOf[List[String]])
              case _ =>
                //println(s"can't define primitive executor for $aClass")
                null
            }
          } else {
            instanter(aClass) match {
              case Some(f) =>
                f(args.map(_.execute()))
              case _ =>
                //println(s"can't define non-primitive executor for $aClass")
                null
            }
          }
        result
      } else
        name
    }

    override def toString: String = if (args.isEmpty) name else s"$name{${args.mkString(",")}}"
  }

  def parse[M <: Model](string: String): M = parseFunction(string).execute().asInstanceOf[M]

  def parseFunction(string: String): Node = {
    val args = "\\(.*\\)".r.findFirstIn(string)
    if (args.isEmpty)
      new Node(string, Nil)
    else {
      val a = args.get.replaceAll("^\\(|\\)$", "")
      val name = string.substring(0, string.length - args.get.length)
      new Node(name, parseArgs(a.toList).map(parseFunction))
    }
  }

  def parseArgs(chars: List[Char], i: Int = 0, result: List[String] = List()): List[String] = {
    val j = chars.head match {
      case '(' => i + 1
      case ')' => i - 1
      case _ => i
    }
    chars.tail.length match {
      case 0 if result.nonEmpty =>
        (result.head + chars.head :: result.tail).reverse
      case 0 =>
        (chars.head.toString :: Nil).reverse
      case _ if result.isEmpty =>
        parseArgs(chars.tail, j, List(chars.head.toString))
      case _ if chars.head == ',' && j == 0 =>
        parseArgs(chars.tail, j, "" :: result)
      case _ =>
        parseArgs(chars.tail, j, result.head + chars.head :: result.tail)
    }
  }

  // returns function which instantiates class from list of strings
  def fromStringer(aClass: Class[_]): Option[List[String] => Any] = aClass match {
    case _ if aClass == classOf[UUID] =>
      Some(list => UUID.fromString(list.head))
    case _ if classOf[String].isAssignableFrom(aClass) =>
      Some(list => list.mkString(""))
    case _ if aClass == Class.forName("scala.Int") | classOf[java.lang.Integer].isAssignableFrom(aClass) =>
      Some(list => list.head.toString.toInt)
    case _ if aClass == Class.forName("scala.Boolean") | aClass == Class.forName("java.lang.Boolean") =>
      Some(list => if (list.head == "true") true else false)
    case _ if aClass == Class.forName("scala.Double") | aClass == Class.forName("java.lang.Double") =>
      Some(list => list.head.toDouble)
    case _ if aClass == Class.forName("java.util.Date") =>
      Some(list => new Date(list.head.toLong))
    case _ if aClass == Class.forName("org.joda.time.DateTime") =>
      Some(list => new DateTime(list.head.toLong))
    case _ if classOf[Enumeration].isAssignableFrom(aClass) =>
      Some(list => getObject(aClass.getName).asInstanceOf[Enumeration].withName(list.head))
    case _ if classOf[Model].isAssignableFrom(aClass) =>
      Some(list => (UUID.fromString(list.head), aClass))
    case _ => None
  }

  // returns function which instantiates any from list of any
  def instanter(aClass: Class[_]): Option[List[Any] => Any] = {
    aClass match {
      case _ if classOf[Reference[Model]].isAssignableFrom(aClass) =>
        Some(list => {
          val t = list.head.asInstanceOf[(UUID, Class[Model])]
          new Reference(t._1, t._2, Reflector.getManager(t._2.getName + "$"))
        })
      case _ if classOf[Model].isAssignableFrom(aClass) =>
        Some(instantiate(aClass.asInstanceOf[Class[Model]], _))
      case _ if classOf[Iterable[_]].isAssignableFrom(aClass) =>
        Some(list => Iterable(list: _*))
      case _ if classOf[Option[_]].isAssignableFrom(aClass) =>
        Some(list => list.head match {
          case "None" => None
          case x: Any => Some(x)
        })
      case _ => None
    }
  }

}

class Description(val aType: Type, val value: Any) {

  def this(v: Any) = this(getType(v.getClass), v)

  val current: String = aType.typeSymbol.fullName match {
    case "scala.Enumeration.Value" | "scala.Enumeration.Val" =>
      // fucking voodoo dancing...
      val typeSymbol = aType.getClass.getMethod("pre").invoke(aType).asInstanceOf[Type].typeSymbol
      val moduleSymbol = typeSymbol.getClass.getMethod("module").invoke(typeSymbol).asInstanceOf[ModuleSymbol]
      val module = Reflector.runtimeMirror.reflectModule(moduleSymbol)
      module.instance.getClass.getName
    case s: String if s.contains(classOf[Reference[_]].getName) =>
      // assume that it's very bad and slow
      classOf[Reference[_]].getName
    case s: String =>
      s
  }

  val children: List[Description] = aType match {
    case _ if aType.typeArgs.isEmpty =>
      Nil
    case _ if isAssignableFrom(classOf[Reference[_]])(aType) && value != null =>
      val resultType = aType.typeArgs.head.finalResultType
      if (resultType.typeSymbol.name.toString != "M")
        List(new Description(resultType, value.asInstanceOf[Reference[_]].id))
      else {
        val resultType = getType(value.asInstanceOf[Reference[_]].manager.getClass)
        List(new Description(resultType, value.asInstanceOf[Reference[_]].id))
      }
    case _ if isAssignableFrom(classOf[Reference[_]])(aType) =>
      List(new Description(aType.typeArgs.head.finalResultType, null))
    case _ if isAssignableFrom(classOf[Option[_]])(aType) =>
      value match {
        case Some(v) => List(new Description(aType.typeArgs.head.finalResultType, v))
        case null => List(new Description(aType.typeArgs.head.finalResultType, null))
        case _ => Nil
      }
    case _ if isAssignableFrom(classOf[Iterable[Any]])(aType) =>
      val t = getType(classOf[Any])
      if (value != null) {
        value.asInstanceOf[Iterable[Any]].flatMap(v => new Description(v) :: Nil).toList
      } else {
        if (aType.typeArgs.head.finalResultType.erasure == t)
          Nil
        else
          List(new Description(aType.typeArgs.head.finalResultType, null))
      }
    case _ =>
      List(new Description(aType.typeArgs.head.finalResultType, value))
  }

  val aClass: Class[_] = Class.forName(current)

  val isReference: Boolean = classOf[Reference[Model]].isAssignableFrom(aClass)

  val isCollection: Boolean = classOf[Iterable[_]].isAssignableFrom(aClass)

  val isReferenceCollection: Boolean =
    !isReference && isCollection && children.nonEmpty && children.map(_.isReference).reduceLeft(_ & _)

  def referredClass[M <: Model]: Option[Class[M]] =
    if (isReference)
      Some(children.head.aClass.asInstanceOf[Class[M]])
    else if (isReferenceCollection && children.nonEmpty)
      children.map(d => d.children.head.aClass).distinct match {
        case List(x) =>
          Some(x.asInstanceOf[Class[M]])
        case _ =>
          None
      }
    else
      None

  def reference[M <: Model]: Option[Reference[M]] = {
    if (isReference)
      Some(value.asInstanceOf[Reference[M]])
    else
      None
  }

  override def toString: String =
    if (children.isEmpty)
      s"$current${if (value != null) s"(${serializer(aClass)(value)})"}"
    else
      s"$current(${children.mkString(",")})"

  override def hashCode(): Int = 41 * aType.hashCode * value.hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case d: Description => current == d.current && children == d.children && value == d.value
  }

}

object ReflectorApp extends App {

  /*
  var product = new Product("Vagina", "Some amazing vagina", ProductCategory.Others)
  var bookmark = new Bookmark(product.reference, new Place(58.1234, 36.4321, "place description").reference, 10, List(product.reference, 10, "Hello!", product.reference))
  var lot = new Lot(bookmark.reference, 100, List(product.reference, product.reference))

  println(Parser.parse[Bookmark](bookmark.toString))
  println(Reflector.describe(bookmark))
  println(Reflector.describe(classOf[Bookmark]))

  println(getManager(classOf[Product]))
  */

}
