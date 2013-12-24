import scala.reflect.macros.WhiteboxContext
import scala.language.experimental.macros
import scala.reflect.runtime.universe.Liftable


object liftableMacro {
  implicit def liftableCaseClass[T]: Liftable[T] = macro impl_liftable[T]
  def impl_liftable[T: c.WeakTypeTag](c: WhiteboxContext): c.Tree = {
    import c.universe._
    val T = weakTypeOf[T]
    val symbol = T.typeSymbol
    if (!symbol.asClass.isCaseClass)
      c.abort(c.enclosingPosition, s"$symbol is not a case class")
    def fields(tpe: Type) = tpe.declarations.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get.paramss.head.map { field ⇒
      val name = field.name
      val typeSign = tpe.declaration(name).typeSignature
      name → typeSign
    }
    val constructor = q"""Select(select(${symbol.fullName}), TermName("apply"))"""
    val arguments = fields(T).map { case (name, typeSign) ⇒
      q"""
        val v : $typeSign = value.$name
        q"$$v"
      """
    }
    val reflect = q"Apply($constructor, List(..$arguments))"
    val implicitName = TermName(symbol.name.encoded ++ "Liftable")
    q"""
      implicit object $implicitName extends Liftable[$T] {
        private def select(fullName: String) = {
          val head :: tail = fullName.split("\\.").toList
          tail.foldLeft[Tree](Ident(TermName(head))){ (tree, name) ⇒
            Select(tree, TermName(name))
          }
        }
        def apply(value: $T): Tree = $reflect
      }
      $implicitName
    """
  }
}
