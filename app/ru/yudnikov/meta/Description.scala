package ru.yudnikov.meta

import ru.yudnikov.core.{ Model, Reference }
import ru.yudnikov.meta.Reflector.{ getType, isAssignableFrom, serializer }
import scala.reflect.runtime.universe._

/**
 * Created by Don on 20.06.2017.
 */
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

