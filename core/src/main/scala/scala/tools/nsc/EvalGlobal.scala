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

  class GenExpr extends Transform with TypingTransformers with ast.TreeDSL {

    import typer.typedPos

    override val global: EvalGlobal.this.type = EvalGlobal.this
    override val phaseName: String = "genexpr"
    override val runsAfter: List[String] = List()
    override val runsRightAfter: Option[String] = Some("extract")

    override protected def newTransformer(unit: CompilationUnit): Transformer = new GenExprTransformer(unit)

    class ExpressionTransformer(symbolsByName: Map[Name, Symbol]) extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case ident: Ident if symbolsByName.contains(ident.name) =>
          // TODO: update positions?
          val newSymbol = symbolsByName(ident.name)
          ident.setSymbol(newSymbol)
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
      override def transform(tree: Tree): Tree = tree match {
        case DefDef(mods0, name0, tparams0, vparamss0, _, rhs0) if name0.decode == "evaluate" =>
          // we can be sure that `rhs0` is an instance of a `Block`
          val block0 = rhs0.asInstanceOf[Block]

          // TODO: generate vals
          val tp = defsByName(TermName("z")).tpt.tpe
          val sym = NoSymbol.newTermSymbol(TermName("z"), block0.pos).setInfo(tp)
          val tt = TypeTree().setType(tp)

//          val tp2 = defsByName(TermName("y")).tpt.tpe
//          val sym2 = NoSymbol.newTermSymbol(TermName("y"), block0.pos).setInfo(tp2)
//          val typeTree2 = TypeTree().setType(tp2)
//          val zMethods = new TreeMethods(Literal(Constant("hello")))
//          val newZ = zMethods.AS(tp)

          val z = ValDef(block0.stats.last.asInstanceOf[ValDef].mods, TermName("z"), tt, Literal(Constant(20))).setSymbol(sym)

          // find all defs in `evaluate` method
          val defFinder = new DefFinder()
          defFinder.traverse(block0)

          // replace symbols in the expression with those from the `evaluate` method
          val newExpression = new ExpressionTransformer(defFinder.symbolsByName.toMap + (z.name -> z.symbol)).transform(expression)

          // update return type of the `evaluate` method
          val tpt = TypeTree().copyAttrs(newExpression)
          tree.symbol.asInstanceOf[MethodSymbol].modifyInfo(info => {
            val methodType = info.asInstanceOf[MethodType]
            methodType.copy(resultType = tpt.tpe)
          })

          // create a new body for the `evaluate` method
          val rhs = typedPos(rhs0.pos)(new Block(block0.stats :+ z, newExpression))
          DefDef(mods0, name0, tparams0, vparamss0, tpt, rhs).copyAttrs(tree)
        case _ =>
          super.transform(tree)
      }
    }
  }
}
