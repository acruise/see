/*
 *  
 */

package see.values

import java.lang.reflect.Method
import see.CopyContext
import see.EvalError
import see.JavaWrapper
import see.ParamError
import see.Scope


private[see] case class Native(wrapper: JavaWrapper)
  extends Val with Callable {

  import Native._

  override def selType = 'Native

  override def isType(typeId: Symbol) =
    (typeId == 'Native) || super.isType(typeId)

  override def toJava: AnyRef = wrapper

  override def size = 1

  override def copy(cc: CopyContext) = new Native(wrapper)

  def isDefinedIn(s: Scope) = true

  // obviously
  def isStable = false // we simply don't know.

  def call(s: Scope, arg: Val): Val = {
    val invokes = wrapper.getClass.getMethods.filter(_.getName == "invoke")
    if (invokes.isEmpty)
      throw new EvalError(wrapper.name + " is not a valid JavaWrapper.")

    val args = arg match {
      case v: Vector => v.values.toArray
      case x => Array(x)
    }
    val meths = invokes.filter(_.getParameterTypes.length == args.size)
    if (meths.isEmpty) throw new ParamError(
      "No native invocation of %s with %d parameters available.".format(
        wrapper.name, args.size))

    val method = if (meths.size == 1) meths.head
    else select(meths, args)
    call(method, args)
  }

  // convert arguments to fit method parameters
  private def call(method: Method, args: Array[Val]): Val = {
    val jargs = convertArgs(method.getParameterTypes, args)
    try {
      val r = method.invoke(wrapper, jargs: _*)
      if (r ne null) Val(r)
      else if (method.getReturnType == java.lang.Void.TYPE)
        VoidVal
      else NullVal
    } catch {
      case x: Exception =>
        throw new EvalError(s"Native call threw ${x.getMessage}")
    }
  }
}

object Native {

  // Selects that method from a list which best matches given arguments.
  def select(ms: Array[Method], args: Array[Val]) = {
    val n = Native.bestMatch(ms.map(_.getParameterTypes), args)
    if (n >= 0) ms(n)
    else {
      val s = "No overload of method " + ms(0).getName +
        " matches argument list ("
      throw new EvalError(args.mkString(s, ", ", ")."))
    }
  }

  /** Converts argument list int to fit given parameter types.
    * @return Object array suitable for reflection call.
    */
  def convertArgs(types: Array[Class[_]], args: Array[Val]) = {
    val jargs = new Array[AnyRef](args.size)
    for (n <- 0 until args.size) {
      jargs(n) = args(n) convertTo types(n)
    }
    jargs
  }

  /** Scans list of parameter types for that one which best maches argument list.
    * @return index of best match, -1 if none matches.
    */
  def bestMatch(typeLists: Array[Array[Class[_]]], args: Array[Val]) = {
    var best = -1
    var bestRating = 0
    for (mIndex <- 0 until typeLists.size) {
      val types = typeLists(mIndex)
      var rating = 0
      var n = 0
      while (n < args.size && rating >= 0) {
        val fit = args(n) fits types(n)
        if (fit > 0) rating += fit
        else rating = -1 // stop on first mismatch
        n += 1
      }
      if (rating > bestRating) {
        bestRating = rating
        best = mIndex
      }
    }
    best
  }
}

