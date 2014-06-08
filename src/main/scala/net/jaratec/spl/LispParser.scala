package net.jaratec.spl

import scala.util.parsing.combinator._

class LispParser extends JavaTokenParsers {

  def id: Parser[LispId] = """[\p{Alnum}[<=>_!?+*/$%&|.\-\^]]+""".r ^^ {name => LispId(name)}

  def bool: Parser[LispBool] = ("#t" ^^ {_ => LispBool(true)}
                                | "#f" ^^ {_ => LispBool(false)}
                              )

  def char: Parser[LispChar] = "'" ~> """\p{Alnum}""".r <~ "'" ^^ {s => LispChar(s.head)}

  def string: Parser[LispString] = stringLiteral ^^ {s => LispString(s.init.tail)}

  def keyword: Parser[LispKeyword] = ":" ~> """\p{Alnum}+""".r ^^ {key => LispKeyword(key)}

  def integer: Parser[LispInt] = wholeNumber ^^ {n => LispInt(BigInt(n))}

  def decimal: Parser[LispDecimal] = floatingPointNumber ^^ {d => LispDecimal(BigDecimal(d))}

  def number: Parser[LispNumber] = decimal | integer

  def atom: Parser[LispAtom] = number | bool | char | string | keyword | id

  def expr: Parser[LispExpr] = atom | seq | lambda | `if` | let | gen | funcall

  def entry: Parser[LispEntry] = keyword ~ expr ^^ {
    case key ~ value => LispEntry(key, value)
  }

  def list: Parser[LispList] = "'(" ~> rep(expr) <~ ")" ^^ {LispList(_)}

  def set: Parser[LispSet] = "@(" ~> rep(expr) <~ ")" ^^ {elems => LispSet(Set() ++ elems)}

  def dict: Parser[LispDict] = "{" ~> rep(entry) <~ "}" ^^ {elems => LispDict(Map() ++ (for (LispEntry(k,v) <- elems) yield (k,v)))}

  // all collections
  def seq: Parser[LispSeq] = list | set | dict

  def arguments: Parser[List[LispId]] = "[" ~> rep(id) <~ "]"

  def lambda = "(" ~> "fn" ~> arguments ~ expr <~ ")" ^^ {
    case args ~ e => LispLambda(args, e)
  }

  def binding: Parser[LispBinding] = id ~ opt("<-") ~ expr ^^ {
    case name ~ _ ~ e => LispBinding(name, e)
  }

  def `if`: Parser[LispIf] = "(" ~> "if" ~> expr ~ expr ~ opt(expr) <~ ")" ^^ {
    case condition ~ thenBody ~ elseBody => LispIf(condition, thenBody, elseBody)
  }

  def let: Parser[LispLet] = "(" ~> "let" ~> "[" ~ rep(binding) ~ "]" ~ expr <~ ")" ^^ {
    case "[" ~ bindings ~ "]" ~ e => LispLet(bindings, e)
  }

  def gen: Parser[LispFor] = "(" ~> "for" ~> "[" ~ rep(binding) ~ opt(":if" ~> funcall) ~ ":yield" ~ expr ~ "]" <~ ")" ^^ {
    case "[" ~ bindings ~ b ~ ":yield" ~ e ~ "]" => LispFor(bindings, b, e) // b is an Option
  } 

  // (expr*) or (expr+) instead of rep(expr) ?
  def funcall: Parser[LispCall] = "(" ~> expr ~ rep(expr) <~ ")" ^^ {
    case fun ~ args => LispCall(fun, args)
  }

  def definition: Parser[LispDef] = "(" ~> "def" ~> id ~ expr <~ ")" ^^ {
    case name ~ e => LispDef(name,e)
  }

  def lisp: Parser[List[LispExpr]] = rep(definition | expr)

  def parse(expr: String): List[LispExpr] = {
    parseAll(lisp, expr) match {
      case Failure(msg, next) => println("Could not parse - " + msg); List()
      case Error(msg, next) => println("Could not parse - " + msg); List()
      case Success(result, next) => result
    }
  }

}
