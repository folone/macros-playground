import scala.language.experimental.macros
import scala.reflect.api.Liftable
import scala.reflect.runtime.{universe ⇒ u}
import u._


case class Ok(s: String, i: Int)
case class Test(d: Double, s: String, ok: Ok)
case class `Ok.Test`(d: Double)

case class A(b: B)
case class B(a: A)

package some.random.pkg {
  case class Pkg(tst: String)
}

object Test {
  import liftableMacro._

  val s = Ok("s", 1)
  val tst = Test(1.0, "test", s)
  val dot = `Ok.Test`(1.0)
  val pkg = some.random.pkg.Pkg("test")

  def ok = {
    implicit val ev = liftableMacro.liftableCaseClass[Ok]
    println(showRaw(q"""$s"""))
    println(showRaw(q"""$tst"""))
    println(showRaw(q"""$dot"""))
    // liftableMacro.liftableCaseClass[A] // Should not compile because of the recursivity
    liftableMacro.liftableCaseClass[some.random.pkg.Pkg]
    println(showRaw(q"""$pkg"""))
  }

  def eval = {
    import scala.reflect.runtime._
    val cm = universe.runtimeMirror(getClass.getClassLoader)
    import scala.tools.reflect.ToolBox
    val tb = cm.mkToolBox()

    tb.eval(q"$s")
    tb.eval(q"$tst")
    //tb.eval(q"$dot")
    //tb.eval(q"$pkg")
  }
}
