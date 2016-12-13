package lecture.macros

import _root_.scala.reflect.macros.blackbox
import scala.language.experimental.macros


object Macro {
  /**
    * b match {
    *   case v if v == h1.toString => h1
    *   ...
    * }
    */
  def foo[T](b: String, list: T*): T = macro fooImpl[T]

  def fooImpl[T : c.WeakTypeTag](c: blackbox.Context)(
                                b: c.Expr[String],
                                list: c.Expr[T]*
  ): c.Expr[T] = {
    import c.universe._
    println(b)
    b match {
      case q"$value" =>
        val tree = q"${list.head}"
        c.Expr(tree)
      case _ =>
        val cases = list.map { m =>
          val name = TermName("value")
          val patt = pq"$name @ _"
          val guard = q"$name == $m.toString()"
          cq"$patt if $guard => $m"
        }
        val tree = q"$b match { case ..$cases }"


        c.Expr(tree)
    }



  }
}
