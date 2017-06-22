package ru.yudnikov.meta

import java.util.{ Date, UUID }

import com.mohiva.play.silhouette.api.LoginInfo
import org.joda.time.DateTime
import ru.yudnikov.core.{ Model, Reference }
import ru.yudnikov.meta.Reflector.{ getObject, instantiate }

/**
 * Created by Don on 20.06.2017.
 */
object Parser {

  class Node(name: String, args: List[Node] = List()) {
    val isArgument: Boolean = args.isEmpty
    val isPrimitive: Boolean = !args.exists(!_.isArgument)
    def execute(): Any = {
      if (!isArgument) {
        val aClass = Class.forName(name)
        //println(s"executing $aClass")
        val result =
          /* if (isCaseClass) {
            Reflector.instantiate(aClass, args.map(_))
          } else */ if (isPrimitive & !Reflector.isCaseClass(aClass)) {
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
    if (chars.nonEmpty) {
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
    } else result
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
    case _ if aClass == classOf[Date] =>
      Some(list => new Date(list.head.toLong))
    case _ if aClass == classOf[DateTime] =>
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
      case _ if Reflector.isCaseClass(aClass) =>
        Some(instantiate(aClass, _))
      case _ => None
    }
  }

}
