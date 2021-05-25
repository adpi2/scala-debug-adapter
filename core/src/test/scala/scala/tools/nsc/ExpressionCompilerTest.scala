package scala.tools.nsc

import utest._

object ExpressionCompilerTest extends TestSuite {
    def tests: Tests = Tests {
      "compile" - {
        val expressionCompiler = ExpressionCompiler(System.getProperty("java.class.path"), 6)

        val source =
          """object EvaluateTest {
            |  def main(args: Array[String]): Unit = {
            |    val a: Int = 1
            |    val b: Int = 2
            |    var c: String = "1 + 2 = "
            |    implicit val d: Int = 3
            |    println(c + (a + b))
            |  }
            |}
            |""".stripMargin

        expressionCompiler.compile(source,  "println(c + (a + b))")

//        val expression = expressionCompiler.global.expr2.toString()
//        val vals = expressionCompiler.global.vals2.map(val2 => "val " + val2.name + " = valuesByName(\"" + val2.name + "\").asInstanceOf[" + val2.tpe + "]").mkString("\n")
//        val source2 =
//          s"""class Expression {
//            |  def evaluate(names: Array[Any], values: Array[Any]) = {
//            |    val valuesByName = names.map(_.asInstanceOf[String]).zip(values).toMap
//            |    $vals
//            |    $expression
//            |  }
//            |}
//            |""".stripMargin
//        println(source2)
//        expressionCompiler.compile(source2, "")
        println(expressionCompiler.global.variables)
        println(expressionCompiler.global.expression)

        print(expressionCompiler.dir)
      }
    }
}
