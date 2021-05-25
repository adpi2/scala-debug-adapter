package scala.tools.nsc

import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.transform.Transform

private [nsc] class EvalGlobal(settings: Settings, reporter: Reporter, val line: Int) extends Global(settings, reporter) {
  var variables: List[Variable] = List()
  var expression: Tree = _

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

    override protected def newTransformer(unit: CompilationUnit): Transformer = new ExtractValsTransformer

    class ExtractValsTransformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        // DefDef?
        case x: ValDef =>
          val name = x.name.decode
          val tpe = x.rhs.tpe.typeSymbol.name.decode

          if (tpe != "<none>") {
            variables +:= Variable(name, tpe)
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

  class GenExpr extends Transform {
    override val global: EvalGlobal.this.type = EvalGlobal.this
    override val phaseName: String = "genexpr"
    override val runsAfter: List[String] = List()
    override val runsRightAfter: Option[String] = Some("extract")

    override protected def newTransformer(unit: CompilationUnit): Transformer = new GenExprTransformer

    class GenExprTransformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case _ => super.transform(tree)
      }
    }
  }
}
