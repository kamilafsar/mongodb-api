package mongodbapi

import scala.language.experimental.macros

// Het is niet mogelijk om meerdere classes te genereren uit 1 macro
// maar wel om een object te genereren met daarin classes
// http://stackoverflow.com/questions/12295971/will-it-be-possible-to-generate-several-top-level-classes-with-one-macro-invocat
trait TopLevelMetadata[T] {
  def hoi: String
}


object MongoMacros {

  def generateMetadata[T]: TopLevelMetadata[T] = macro MongoMacrosImpl.generateMetadata[T]

}

import scala.reflect.macros.whitebox.Context

object MongoMacrosImpl {
  def generateMetadata[T: c.WeakTypeTag](c: Context): c.Expr[TopLevelMetadata[T]] = {

        import c.universe._
        import definitions._
        import Flag._


    val sym = c.weakTypeOf[T].typeSymbol
    if (!sym.isClass || !sym.asClass.isCaseClass) c.abort(c.enclosingPosition, s"$sym is not a case class")
    val fields = sym.typeSignature.decls.toList.collect{ case x: TermSymbol if x.isVal && x.isCaseAccessor => x }


    reify {
      new TopLevelMetadata[T] {
        def hoi: String = "hoi"
        def kaas: String = "kaas"
      }
    }

    
//
//
//    def mkTpt() = {
//      val core = Ident(TupleClass(fields.length) orElse UnitClass)
//      if (fields.length == 0) core
//      else AppliedTypeTree(core, fields map (f => TypeTree(f.typeSignature)))
//    }
//
//    def mkFrom() = {
//      if (fields.length == 0) Literal(Constant(Unit))
//      else Apply(Ident(TermName("Tuple" + fields.length)), fields map (f => Select(Ident(TermName("f")), TermName(f.name.toString.trim))))
//    }
//
//    val evidenceClass = ClassDef(
//      mods = Modifiers(FINAL),
//      name = TypeName("$anon"),
//      tparams = List(),
//      impl = Template(
//        parents = List(
//          AppliedTypeTree(
//            tpt = Ident(name = TypeName("Iso")),
//            args = List(Ident(sym = sym), mkTpt())
//          )
//        ),
//        self = noSelfType,
//        body = List(
//          DefDef(
//            mods = Modifiers(),
//            name = termNames.CONSTRUCTOR,
//            tparams = List(),
//            vparamss = List(List()),
//            tpt = TypeTree(),
//            rhs = Block(
//              stats = List(
//                Apply(
//                  fun = Select(
//                    qualifier = Super(
//                      qual = This(qual = typeNames.EMPTY),
//                      mix = typeNames.EMPTY
//                    ),
//                    termNames.CONSTRUCTOR
//                  ),
//                  args = List()
//                )
//              ),
//              expr = Literal(Constant(()))
//            )
//          ),
//          DefDef(
//            mods = Modifiers(),
//            name = TermName("to"),
//            tparams = List(),
//            vparamss = List(
//              List(
//                ValDef(
//                  mods = Modifiers(PARAM),
//                  name = TermName("f"),
//                  tpt = Ident(sym),
//                  rhs = EmptyTree
//                )
//              )
//            ),
//            tpt = TypeTree(),
//            rhs = mkFrom()
//          )
//        )
//      ))
//
//    c.Expr[TopLevelMetadata[T]](
//      Block(
//        stats = List(evidenceClass),
//        expr = Apply(
//          Select(
//            qualifier = New(sym = Ident(name = TypeName(s = "$anon"))),
//            sym = termNames.CONSTRUCTOR), List()
//        )
//      )
//    )
  }



}
