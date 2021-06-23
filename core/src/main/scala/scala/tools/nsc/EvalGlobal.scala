package scala.tools.nsc

import scala.collection.mutable
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.transform.{Transform, TypingTransformers}

private[nsc] class EvalGlobal(settings: Settings, reporter: Reporter, val line: Int) extends Global(settings, reporter) {
  private var defsByName: Map[TermName, ValDef] = Map()
  private var expression: Tree = _

  override protected def computeInternalPhases(): Unit = {
    super.computeInternalPhases()

    addToPhasesSet(new Extract, "extract")
    addToPhasesSet(new GenExpr, "generate expression")
  }

  class Extract extends Transform {
    override val global: EvalGlobal.this.type = EvalGlobal.this
    override val phaseName: String = "extract"
    override val runsAfter: List[String] = List("delambdafy")
    override val runsRightAfter: Option[String] = None

    override protected def newTransformer(unit: CompilationUnit): Transformer = {
      if (unit.source.file.name == "<source>") new ExtractValsTransformer
      else noopTransformer
    }

    class ExtractValsTransformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        // TODO: DefDef?
        case tree: ValDef =>
          if (tree.rhs.tpe.typeSymbol != NoSymbol) {
            defsByName += (tree.name -> tree)
          }
          tree
        case _ if tree.pos.line == line =>
          expression = tree
          tree
        case _ =>
          super.transform(tree)
      }
    }
  }

  class GenExpr extends Transform with TypingTransformers {

    import typer.typedPos

    override val global: EvalGlobal.this.type = EvalGlobal.this
    override val phaseName: String = "genexpr"
    override val runsAfter: List[String] = List()
    override val runsRightAfter: Option[String] = Some("extract")

    override protected def newTransformer(unit: CompilationUnit): Transformer = new GenExprTransformer(unit)

    class ExpressionTransformer(symbolsByName: Map[Name, Symbol]) extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case ident: Ident if symbolsByName.contains(ident.name) =>
          ident.setSymbol(symbolsByName(ident.name))
        case _ =>
          super.transform(tree)
      }
    }

    class DefFinder extends Traverser {
      val symbolsByName: mutable.Map[Name, Symbol] = mutable.Map()

      override def traverse(tree: Tree): Unit = {
        tree match {
          case tree: ValDef =>
            symbolsByName += (tree.name -> tree.symbol)
          case _ =>
            traverseTrees(tree.children)
        }
      }
    }

    class GenExprTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
      private var valuesByNameIdent: Ident = _

      override def transform(tree: Tree): Tree = tree match {
        case tree: Ident if tree.name == TermName("valuesByName") && valuesByNameIdent == null =>
          valuesByNameIdent = tree
          super.transform(tree)
        case DefDef(_, name, _, _, _, _) if name == TermName("evaluate") =>
          // firstly, transform the body of the method
          super.transform(tree)

          deriveDefDef(tree) { rhs =>
            // we can be sure that `rhs` is an instance of a `Block`
            val block = rhs.asInstanceOf[Block]

            // find all defs in the body of the `evaluate` method
            val defFinder = new DefFinder()
            defFinder.traverse(block)

//            val app = typedPos(tree.pos)(Apply(valuesByNameIdent, List(Literal(Constant("z")))))
            val app = Apply(valuesByNameIdent, List(Literal(Constant("z"))))
            val clazz = rootMirror.getRequiredClass("java.lang.String")
            val tpe = defsByName(TermName("z")).tpt.tpe
            val casted = gen.mkCast(app, clazz.tpe)
            val sym = NoSymbol.newTermSymbol(TermName("z"), tree.pos).setInfo(tpe)
            val tt = TypeTree().setType(tpe)
//            val z = ValDef(Modifiers(), TermName("z"), tt, casted).setSymbol(sym)
            val z = ValDef(Modifiers(), TermName("z"), tt, casted).setSymbol(sym)

            val symbolsByName = defFinder.symbolsByName.toMap + (z.name -> z.symbol)

            // replace symbols in the expression with those from the `evaluate` method
            val newExpression = new ExpressionTransformer(symbolsByName).transform(expression)

            val newRhs = new Block(block.stats :+ z, newExpression)

            val tpt = TypeTree().copyAttrs(newExpression)
            typedPos(tree.pos)(newRhs).setType(tpt.tpe)
          }
        case _ =>
          super.transform(tree)
      }
    }
  }
}
