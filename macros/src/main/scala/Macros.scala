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
    def select(fullName: String) = {
      val head :: tail = fullName.split("\\.").toList
      tail.foldLeft[Tree](Ident(TermName(head))){ (tree, name) ⇒
        Select(tree, TermName(name))
      }
    }
    val constructor = Select(Apply(
                               Ident(TermName("reify")),
                               List(select(symbol.fullName))),
                        TermName("tree")) // q"reify(${select(symbol.fullName)}).tree"
    val arguments = fields(T).map { case (name, typeSign) ⇒
      // Implementing the following
      // q"implicitly[Liftable[$typeSign]].apply(value.$name)"
      Apply(Select(
              TypeApply(Ident(TermName("implicitly")),
                        List(AppliedTypeTree(Ident(TypeName("Liftable")),
                             List(TypeTree(typeSign))))),
              TermName("apply")),
           List(Select(Ident(TermName("value")), name)))
      // Another way to do this is
      /*q"""
        val v : $typeSign = value.$name
        q"$$v"
       """*/
    }
    val reflect = Apply(Ident(TermName("Apply")),
                        List(constructor,
                          Apply(Ident(TermName("List")),
                        arguments))) // q"Apply($constructor, List(..$arguments))"
    val implicitName = TermName(symbol.name.encoded ++ "Liftable")
    // Implements the following
    /*q"""
      implicit object $implicitName extends Liftable[$T] {
        def apply(value: $T): Tree = $reflect
      }
      $implicitName
    """*/
    import Flag._
    Block(List(ModuleDef(Modifiers(IMPLICIT),
                implicitName,
                Template(
                  List(AppliedTypeTree(Ident(TypeName("Liftable")),
                       List(TypeTree(T)))),
                  noSelfType,
                  List(DefDef(Modifiers(),
                        nme.CONSTRUCTOR,
                        List(), List(List()),
                        TypeTree(),
                        Block(
                          List(pendingSuperCall),
                          Literal(Constant(())))),
                        DefDef(Modifiers(),
                               TermName("apply"),
                               List(),
                               List(List(ValDef(Modifiers(PARAM),
                                         TermName("value"), TypeTree(T), EmptyTree))),
                               Ident(TypeName("Tree")), reflect))))),
          Ident(implicitName))
  }

}
