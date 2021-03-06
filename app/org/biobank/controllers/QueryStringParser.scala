package org.biobank.controllers

import scala.util.parsing.combinator.RegexParsers

/**
 * Definition of grammar used by [[QueryStringParser]].
 *
 * See: {@link https://github.com/jirutka/rsql-parser/blob/master/README.adoc}
 *
 */
object QueryStringParserGrammar {

  final case class Value(name: String) {
    override def toString: String = s"Value: $name"
  }

  final case class Name(name: String) {
    override def toString: String = s"Name: $name"
  }

  final case class Expression(name: String, value: String) {
    override def toString: String = s"Expression: $name = $value"
  }
}

/**
 * This parser for Query Strings is able to parse expressions using RSQL syntax.
 *
 * However, the comparsion operators are given in [[services.Comparator Comparator]]
 *
 * See: {@link https://github.com/jirutka/rsql-parser/blob/master/README.adoc}
 *
 */
object QueryStringParser extends RegexParsers {
  import QueryStringParserGrammar._

  def singleQuotedValue: Parser[Value] =
    """'[^\"=!~<>&]*'""".r ^^ { case v => Value(v.substring(1, v.size - 1)) }

  def doubleQuotedValue: Parser[Value] =
    """"[^'=!~<>&]*"""".r ^^ { case v => Value(v.substring(1, v.size - 1)) }

  def unquotedValue: Parser[Value] =
    """[^'\"\=!~<>&]+""".r ^^ { case v => Value(v) }

  def value: Parser[Value] =
    (unquotedValue | singleQuotedValue | doubleQuotedValue) ^^ { case v => v }

  def name: Parser[Name] =
    """[_a-zA-Z]+[_a-zA-Z0-9.]*""".r ^^ { case n => Name(n) }

  def expression: Parser[Expression] =
    name ~ "=" ~ value ^^ { case n ~ _ ~ v => Expression(n.name, v.name) }

  def expressions: Parser[List[Expression]] =
    repsep(expression, "&")

  def apply(str: String): Option[QueryStringExpressions] =
    if (str.trim.isEmpty) {
      Some(Map[String, String]())
    } else {
      parseAll(expressions, str) match {
        case NoSuccess(_, _) => None
        case Success(result, _) =>
          val map = result.map { e =>
            (e.name -> e.value)
          }.toMap
          Some(map)
      }
    }
}
