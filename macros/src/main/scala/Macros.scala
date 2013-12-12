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
    def fields(tpe: Type) = tpe.declarations.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get.paramss.head.map { field ⇒
      val name = field.name
      val typeSign = tpe.declaration(name).typeSignature
      name → typeSign
    }
    def checkRecursivity(root: Type, flds: List[(Name, Type)]): Boolean = {
      lazy val isAllPrimitive = flds.map { case(name, tpe) ⇒
        val sym = tpe.typeSymbol
        sym.isClass && sym.asClass.isCaseClass
      }.forall(x ⇒ !x)
      lazy val containsBaseType = flds.map { case(name, tpe) ⇒
        tpe.exists(t ⇒ t =:= root) || root.exists(t ⇒ t =:= tpe) || tpe =:= root
      }.exists(identity)
      if(!isAllPrimitive && !containsBaseType) {
        checkRecursivity(root, flds.flatMap { case(name, tpe) ⇒
          val res = fields(tpe)
          if (res.isEmpty) List((name, tpe))
          else res
        })
      }
      else !isAllPrimitive || containsBaseType
    }
    val params = fields(T)
    if(checkRecursivity(T, params))
      c.abort(c.enclosingPosition, s"We've detected that $symbol is defined recursively.")
    val spliced = params.map { case (name, typeSign) ⇒
      q"implicitly[Liftable[$typeSign]].apply(universe, value.$name)"
    }
    c.Expr[Liftable[T]] { q"""
        import scala.reflect.api.Universe
        new Liftable[$T] {
          def apply(universe: Universe, value: $T): universe.Tree = {
            import universe._
            Apply(Select(Ident(newTermName(`${symbol.fullName}`)),
                         newTermName("apply")), List(..$spliced))
          }
        }
        """
    }
  }

}
