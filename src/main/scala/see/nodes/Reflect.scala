/*
 *  
 */

package see.nodes

import java.lang.reflect.Constructor
import see.EvalError
import see.Scope
import see.values._

private[see] class ReflectP(methName: String, args: Node) extends Proto {
  override def precedence = PREC.Deref

  override def finish(n: Node) = Some(new MethReflector(n, methName, args))
}

private[see] class ReflectFieldP(fieldName: String) extends Proto {
  override def precedence = PREC.Deref

  override def finish(n: Node) = Some(new FieldReflector(n, fieldName))
}

private[see] class MethReflector(
                                  ref: Node,
                                  val methName: String,
                                  val args: Node)
  extends Atom(ref) {

  override def evalIn(s: Scope) = {
    if (!see.See.reflectionEnabled)
      throw new EvalError("Java Reflection is disabled.")
    val obj = opd.evalIn(s).coerce
    val cargs = args.evalIn(s).coerce match {
      case v: Vector => v.values.toArray
      case x => Array(x)
    }
    obj match {
      case AnyVal(any) => deref(any, cargs)
      case _ => derefClass(obj.toStr, cargs)
    }
  }

  override def simplifyIn(s: Scope) = try {
    val ref = opd.simplifyIn(s)
    val sargs = args.simplifyIn(s)
    new MethReflector(ref, methName, sargs)
  } catch {
    case _: Exception => this
  }

  override def isDefinedIn(s: Scope): Boolean =
    opd.isDefinedIn(s) && args.isDefinedIn(s)

  override def toString = "(" + opd + ":>" + methName + "[" + args + "])"

  private def deref(obj: AnyRef, cargs: Array[Val]) =
    callMethod(obj.getClass, obj, cargs)

  private def derefClass(clsName: String, cargs: Array[Val]) = {
    val cls = try {
      Class.forName(clsName)
    } catch {
      case _: Exception =>
        throw new EvalError("Unknown classname: '" + clsName + "'.")
    }
    if (methName == "new") create(cls, cargs)
    else callMethod(cls, null, cargs) // static call
  }

  private def callMethod(cls: Class[_],
                         inst: AnyRef, cargs: Array[Val]): Val = {
    val mths = for {
      m <- cls.getMethods
      pt = m.getParameterTypes
      if (m.getName == methName) && (pt.length == cargs.length)
    } yield m

    if (mths.isEmpty)
      throw new EvalError(s"Class '${cls.getName}' has no method $methName with ${cargs.length} arguments")

    val meth = if (mths.size > 1)
      Native.select(mths, cargs)
    else mths.head

    val jargs = Native.convertArgs(meth.getParameterTypes, cargs)
    try {
      val result = meth.invoke(inst, jargs: _*)

      if (result != null) Val(result)
      else if (meth.getReturnType == java.lang.Void.TYPE) VoidVal
      else NullVal
    } catch {
      case x: Exception =>
        throw new EvalError(s"Invocation of '${cls.getName}.$methName' failed: ${x.getMessage}")
    }
  }

  private def create(cls: Class[_], cargs: Array[Val]): Val = {
    val ctors = for {
      c <- cls.getConstructors
      pt = c.getParameterTypes
      if pt.length == cargs.length
    } yield c

    if (ctors.isEmpty)
      throw new EvalError(s"Class '${cls.getName}' has no constructor with ${cargs.length} arguments")

    val ctor = if (ctors.size > 1)
      select(ctors, cargs)
    else ctors.head

    val jargs = Native.convertArgs(ctor.getParameterTypes, cargs)

    try {
      Val(ctor.newInstance(jargs: _*).asInstanceOf[AnyRef])
    } catch {
      case x: Exception =>
        throw new EvalError(s"Construction of '${cls.getName}' failed: ${x.getMessage}")
    }
  }

  private def select(ctors: Array[Constructor[_]], args: Array[Val]) = {
    val n = Native.bestMatch(ctors.map(_.getParameterTypes), args)
    if (n >= 0) ctors(n)
    else throw new EvalError(args.mkString(
      "No constructor matches argument list (", ", ", ").")
    )
  }
}

/** Reflector Node, performs native java call using reflection
  */
private[see] class FieldReflector(
                                   ref: Node,
                                   val fieldName: String)
  extends Atom(ref) with LvNode {

  override def evalIn(s: Scope) = {
    if (!see.See.reflectionEnabled)
      throw new EvalError("Java Reflection is disabled.")
    val obj = opd.evalIn(s).coerce
    obj match {
      case AnyVal(any) => getField(any.getClass, any)
      case _ => getField(ofClass(obj.toStr), null)
    }
  }

  override def setIn(s: Scope, value: Val) {
    if (!see.See.reflectionEnabled)
      throw new EvalError("Java Reflection is disabled.")
    val obj = opd.evalIn(s).coerce
    obj match {
      case AnyVal(any) => setField(any.getClass, any, value)
      case _ => setField(ofClass(obj.toStr), null, value)
    }
  }

  override def simplifyIn(s: Scope) = try {
    val ref = opd.simplifyIn(s)
    new FieldReflector(ref, fieldName)
  } catch {
    case _: Exception =>
      this
  }

  override def isDefinedIn(s: Scope): Boolean =
    opd.isDefinedIn(s)

  override def toString = s"($opd:>$fieldName)"

  private def ofClass(clsName: String) = try {
    Class.forName(clsName)
  } catch {
    case _: Exception =>
      throw new EvalError(s"Unknown classname: '$clsName'.")
  }

  private def jfield(cls: Class[_]) = try {
    cls.getDeclaredField(fieldName)
  } catch {
    case _: Exception =>
      throw new EvalError(s"Class '${cls.getName}' has no field '$fieldName'."
    )
  }

  private def getField(cls: Class[_], inst: AnyRef): Val = {
    val field = jfield(cls)
    try {
      Val(field.get(inst))
    } catch {
      case x: Exception =>
        throw new EvalError(s"Cannot access '${cls.getName}.$fieldName': ${x.getMessage}")
    }
  }

  private def setField(cls: Class[_], inst: AnyRef, value: Val) {
    val field = jfield(cls)
    try {
      val jobj = value.convertTo(field.getType)
      field.set(inst, jobj)
    } catch {
      case x: Exception =>
        throw new EvalError(s"Cannot change '${cls.getName}.$fieldName': ${x.getMessage}")
    }
  }

}

