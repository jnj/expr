package expr

import util.parsing.combinator.{RegexParsers, PackratParsers}

sealed case class Node(children: Seq[Node])
case class Number(d: Double) extends Node(Seq.empty)
case class Add(a: Node, b: Node) extends Node(Seq(a, b))
case class Sub(a: Node, b: Node) extends Node(Seq(a, b))
case class Mul(a: Node, b: Node) extends Node(Seq(a, b))
case class Div(a: Node, b: Node) extends Node(Seq(a, b))

object ExprEvaluator {
  def eval(n: Node):Double = {
    n match {
      case Number(d) => d
      case Add(x, y) => eval(x) + eval(y)
      case Sub(x, y) => eval(x) - eval(y)
      case Mul(x, y) => eval(x) * eval(y)
      case Div(x, y) => eval(x) / eval(y)
    }
  }
}


object ExprParser extends RegexParsers with PackratParsers {
  lazy val expr: PackratParser[Node] = ((expr ~ "+" ~ term) | (expr ~ "-" ~ term) | term) ^^ {
    case ((e: Node) ~ "+" ~ (t: Node)) => Add(e, t)
    case ((e: Node) ~ "-" ~ (t: Node)) => Sub(e, t)
    case (t: Node) => t
  }

  lazy val term: PackratParser[Node] = ((term ~ "*" ~ factor) | (term <~ "/" ~ factor) | factor) ^^ {
    case ((t: Node) ~ "*" ~ (f: Node)) => Mul(t, f)
    case ((t: Node) ~ "/" ~ (f: Node)) => Div(t, f)
    case (f: Node) => f
  }

  lazy val factor = number | "(" ~> expr <~ ")"

  lazy val number: PackratParser[Node] = """\d+(\.\d+)?""".r ^^ { s => Number(s.toDouble) }
}