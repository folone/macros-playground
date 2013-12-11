import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.reflect.api.Liftable


object liftableMacro {
  implicit def liftableCaseClass[T]: Liftable[T] = macro impl_liftable[T]
  def impl_liftable[T: c.WeakTypeTag](c: Context): c.Expr[Liftable[T]] = {
    import c.universe._
    val T = weakTypeOf[T]
    val symbol = T.typeSymbol
    if (!symbol.asClass.isCaseClass)
      c.abort(c.enclosingPosition, s"$symbol is not a case class")
    else {
      val fields = T.declarations.collectFirst {
        case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
      }.get.paramss.head
      val spliced = fields.map { field ⇒
        val name = field.name
        val typeSign = T.declaration(name).typeSignature
        q"implicitly[Liftable[$typeSign]].apply(universe, value.$name)"
      }
      c.Expr[Liftable[T]] { q"""
        import scala.reflect.api.Universe
        new Liftable[$T] {
          def apply(universe: Universe, value: $T): universe.Tree = {
            import universe._
            Apply(Select(Ident(newTermName(${symbol.fullName})),
                         newTermName("apply")), List(..$spliced))
          }
        }
        """
      }
    }
  }

}
