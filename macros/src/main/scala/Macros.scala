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
    def select(symbol: Symbol) = {
      def chainSymbol(symbol: Symbol, accum: List[Name]): List[Name] = {
        val owner = symbol.owner
        if(owner.name.decoded == "<none>" || owner.name.decoded == "<empty>")
          accum
        else if(owner.name.decoded == "<root>")
          TermName("_root_") :: accum
        else
          chainSymbol(owner, owner.name.toTermName :: accum)
      }
      val head :: tail = chainSymbol(symbol, List(symbol.companionSymbol.name))
      tail.foldLeft[Tree](Ident(head)) { (tree, name) ⇒
        Select(tree, name)
      }
    }
    val constructor = Select(Apply(
                               Ident(TermName("reify")),
                               List(select(symbol))),
                        TermName("tree")) // q"reify(${select(symbol)}).tree"
    val arguments = fields(T).map { case (name, typeSign) ⇒
      // Tree produced by the following quasiquote:
      // q"implicitly[Liftable[$typeSign]].apply(value.$name)"
      Apply(Select(
              TypeApply(Select(Select(Select(Ident(TermName("_root_")),
                                                   TermName("scala")),
                                                   TermName("Predef")),
                                                   TermName("implicitly")),
                        List(AppliedTypeTree(Select(Ident(TermName("u")),
                                                          TypeName("Liftable")),
                             List(TypeTree(typeSign))))),
              TermName("apply")),
           List(Select(Ident(TermName("value")), name)))
      // Another way to do this would be:
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
    // Tree produced by the following quasiquote:
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
                  List(AppliedTypeTree(Select(Ident(TermName("u")),
                                                    TypeName("Liftable")),
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
                               Select(Ident(TermName("u")),
                                            TypeName("Tree")), reflect))))),
          Ident(implicitName))
  }

}
