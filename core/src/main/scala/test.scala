import scala.language.experimental.macros
import scala.reflect.runtime.{universe ⇒ u}
import u._


case class Ok(s: String, i: Int)
case class Test(d: Double, s: String, ok: Ok)
case class `Ok.Test`(d: Double)

case class A(a: A)

case class B(c: C)
case class C(b: B)

package some.random.pkg {
  case class Pkg(tst: String)
}

object Test {
  import liftableMacro._

  val s = Ok("s", 1)
  val tst = Test(2.0, "test", s)
  val dot = `Ok.Test`(3.0)
  val pkg = some.random.pkg.Pkg("test")

  def produceTrees = {
    liftableMacro.liftableCaseClass[A]
    liftableMacro.liftableCaseClass[B]
    liftableMacro.liftableCaseClass[Ok]
    println(showRaw(q"""$s"""))
    println(showRaw(q"""$tst"""))
    println(showRaw(q"""$dot"""))
    liftableMacro.liftableCaseClass[some.random.pkg.Pkg]
    println(showRaw(q"""$pkg"""))
  }

  def compileAndEval = {
    import scala.reflect.runtime._
    val cm = universe.runtimeMirror(getClass.getClassLoader)
    import scala.tools.reflect.ToolBox
    val tb = cm.mkToolBox()

    println(tb.eval(q"$s"))
    println(tb.eval(q"$tst"))
    println(tb.eval(q"$dot"))
    println(tb.eval(q"$pkg"))
  }
}
