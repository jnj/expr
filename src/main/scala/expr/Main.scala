package expr

import scala.io.Source
import scala.util.parsing.input.CharSequenceReader

object Main {
  def main(args: Array[String]) {
    var going = true
    while (going) {
      val line = Source.fromInputStream(System.in).getLines().next()
      if (line == "") {
        going = false
      } else {
        val result = ExprParser.parseAll(ExprParser.expr, new ExprParser.PackratReader(new CharSequenceReader(line)))
        result match {
          case ExprParser.Success(t, _) => println(ExprEvaluator.eval(t))
          case e => println(e)
        }
      }
    }
  }

}
