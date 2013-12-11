import scala.language.experimental.macros
import scala.reflect.api.Liftable
import scala.reflect.runtime.{universe ⇒ u}
import u._


case class Ok(s: String, i: Int)

object Test {
  import liftableMacro._

  val s = Ok("s", 1)

  def ok = {
    implicit val ev = liftableMacro.liftableCaseClass[Ok]
    showRaw(q"""$s""")
  }
}
