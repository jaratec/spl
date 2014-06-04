package net.jaratec.spl

import scala.collection.mutable.{HashMap,Map}

class Interpreter {

  type Env = Map[LispId, LispExpr]
  val GLOBAL_ENV: Env = HashMap.empty[LispId, LispExpr]

  bindToEnv(LispId("true"), LispBool(true))
  bindToEnv(LispId("false"), LispBool(false))

  private case class LispClosure(val env: Env, val lambda: LispLambda) extends LispExpr {
  }

  val primitiveFunctions = List("head", "tail", "nil?", "atom?", "number?", "=", "<" , "<=", ">", ">=", "and", "or", "cons", "+", "-", "*", "/", "mod", "rem", "size", "empty?", "list?", "map?", "put", "remove", "get", "keys", "union", "diff", "intersect", "list", "apply", "type")

  val primitiveAtoms = List("true", "false")

  def bindsPrimitive(b: LispBinding) = primitiveAtoms.contains(b.id.id)

  def applyFunction(env: Env, fname: String, args: List[LispExpr]): LispExpr = {
    fname match {
      case "head" => if (args.size != 1) LispNil() else eval(env,args.head) match {
        case LispList(elements) => if (elements.size == 0) LispNil() else elements.head
        case LispSet(elements) => if (elements.size == 0) LispNil() else elements.head
        case _ => LispNil()
      }
      case "tail" => if (args.size != 1) LispNil() else eval(env,args.head) match {
        case LispList(elements) => if (elements.size == 0) LispNil() else LispList(elements.tail)
        case LispSet(elements) => if (elements.size == 0) LispNil() else LispSet(elements.tail)
        case _ => LispNil()
      }
      case "nil?" => if (args.size != 1) LispNil() else eval(env,args.head).isNil
      case "atom?" => if (args.size != 1) LispNil() else eval(env,args.head).isAtom
      case "number?" => if (args.size != 1) LispNil() else eval(env,args.head).isNumber
      case "=" => if (args.size < 2) LispNil() else {
        val head = eval(env, args.head)
        LispBool(args.tail.map{e => head.equals(eval(env,e))}.foldLeft(true){_ == _})
      }
      case "<" => if (args.size != 2) LispNil() else (eval(env,args.head), eval(env,args.last)) match {
        case (x: LispNumber, y: LispNumber) => LispBool(x < y)
        case (c1: LispChar, c2: LispChar) => LispBool(c1 < c2)
        case (s1: LispString, s2: LispString) => LispBool(s1 < s2)
        case _ => LispNil()
      }
      case "<=" => if (args.size != 2) LispNil() else (eval(env,args.head), eval(env,args.last)) match {
        case (x: LispNumber, y: LispNumber) => LispBool(x <= y)
        case (c1: LispChar, c2: LispChar) => LispBool(c1 <= c2)
        case (s1: LispString, s2: LispString) => LispBool(s1 <= s2)
        case _ => LispNil()
      }
      case ">" => if (args.size != 2) LispNil() else (eval(env,args.head), eval(env,args.last)) match {
        case (x: LispNumber, y: LispNumber) => LispBool(x > y)
        case (c1: LispChar, c2: LispChar) => LispBool(c1 > c2)
        case (s1: LispString, s2: LispString) => LispBool(s1 > s2)
        case _ => LispNil()
      }
      case ">=" => if (args.size != 2) LispNil() else (eval(env,args.head), eval(env,args.last)) match {
        case (x: LispNumber, y: LispNumber) => LispBool(x >= y)
        case (c1: LispChar, c2: LispChar) => LispBool(c1 >= c2)
        case (s1: LispString, s2: LispString) => LispBool(s1 >= s2)
        case _ => LispNil()
      }
      case "and" => if (args.size < 2) LispNil() else args.map{eval(env,_)} match {
        case bools: List[LispBool] => bools.foldLeft(LispBool(true)){_ && _}
        case _ => LispNil() // not a list of boolean expressions
      }
      case "or" => if (args.size < 2) LispNil() else args.map{eval(env,_)} match {
        case bools: List[LispBool] => bools.foldLeft(LispBool(false)){_ || _}
        case _ => LispNil() // not a list of boolean expressions
      }
      case "cons" => if (args.size != 2) LispNil() else eval(env,args.last) match {
        case LispList(elements) => LispList(eval(env,args.head) :: elements)
        case _ => LispNil()
      }
      case "+" => if (args.size != 2) LispNil() else (eval(env,args.head), eval(env,args.last)) match {
        case (x: LispNumber, y: LispNumber) => x + y
        case (s1: LispString, s2: LispString) => s1 + s2
        case (s: LispString, c: LispChar) => s + c
        case (c: LispChar, s: LispString) => c + s
        case (c1: LispChar, c2: LispChar) => c1 + c2
        case (s: LispString, n: LispInt) => s + n
        case (s: LispString, n: LispDecimal) => s + n
        case (n: LispInt, s: LispString) => s + n
        case (n: LispDecimal, s: LispString) => s + n
        case _ => LispNil()
      }
      case "-" => if (args.size != 2) LispNil() else (eval(env,args.head), eval(env,args.last)) match {
        case (x: LispNumber, y: LispNumber) => x - y
        case _ => LispNil()
      }
      case "*" => if (args.size != 2) LispNil() else (eval(env,args.head), eval(env,args.last)) match {
        case (x: LispNumber, y: LispNumber) => x * y
        case _ => LispNil()
      }
      case "/" => if (args.size != 2) LispNil() else (eval(env,args.head), eval(env,args.last)) match {
        case (x: LispNumber, y: LispNumber) => x / y
        case _ => LispNil()
      }
      case "mod" => if (args.size != 2) LispNil() else (eval(env,args.head), eval(env,args.last)) match {
        case (LispInt(x), LispInt(y)) => LispInt(x / y)
        case (LispDecimal(x), LispDecimal(y)) => LispInt((x / y).intValue)
        // case (x: LispNumber, y: LispNumber) => x / y
        case _ => LispNil()
      }
      case "rem" => if (args.size != 2) LispNil() else (eval(env,args.head), eval(env,args.last)) match {
        case (x: LispNumber, y: LispNumber) => x % y
        case _ => LispNil()
      }
      case "size" => if (args.size != 1) LispNil() else eval(env,args.head) match {
        case LispList(elements) => LispInt(elements.size)
        case LispSet(elements) => LispInt(elements.size)
        case LispDict(entries) => LispInt(entries.size)
        case _ => LispNil()
      }
      case "empty?" => if (args.size != 1) LispNil() else eval(env,args.head) match {
        case LispList(elements) => LispBool(elements.isEmpty)
        case LispSet(elements) => LispBool(elements.isEmpty)
        case LispDict(entries) => LispBool(entries.isEmpty)
        case _ => LispNil()
      }
      case "list?" => if (args.size != 1) LispNil() else eval(env,args.head) match {
        case LispList(_) => LispBool(true)
        case _ => LispBool(false)
      }
      case "map?" => if (args.size != 1) LispNil() else eval(env,args.head) match {
        case LispDict(_) => LispBool(true)
        case _ => LispBool(false)
      }
      case "put" => if (args.size != 3) LispNil() else eval(env,args.head) match {
        case LispDict(entries) => {
          val k = eval(env,args.tail.head)
          val v = eval(env,args.last)
          k match {
            case key @ LispKeyword(_) => LispDict(entries + (key -> v))
            case _ => LispNil()
          }
        }
        case _ => LispNil()
      }
      case "remove" => if (args.size != 2) LispNil() else eval(env,args.head) match {
        case LispDict(entries) => eval(env,args.last) match {
          case key @ LispKeyword(_) => LispDict(entries - key)
          case _ => LispNil()
        }
        case _ => LispNil()
      }
      case "get" => if (args.size != 2) LispNil() else eval(env,args.head) match {
        case LispDict(entries) => eval(env,args.last) match {
          case key @ LispKeyword(_) => entries.getOrElse(key, LispNil())
          case _ => LispNil()
        }
        case _ => LispNil()
      }
      case "keys" => if (args.size != 1) LispNil() else eval(env,args.head) match {
        case dict @ LispDict(entries) => LispList(entries.keys.toList)
        case _ => LispNil()
      }
      case "union" => if (args.size != 2) LispNil() else (eval(env,args.head), eval(env,args.last)) match {
        case (LispSet(s1), LispSet(s2)) => LispSet(s1.union(s2))
        case _ => LispNil()
      }
      case "diff" => if (args.size != 2) LispNil() else (eval(env,args.head), eval(env,args.last)) match {
        case (LispSet(s1), LispSet(s2)) => LispSet(s1.diff(s2))
        case _ => LispNil()
      }
      case "intersect" => if (args.size != 2) LispNil() else (eval(env,args.head), eval(env,args.last)) match {
        case (LispSet(s1), LispSet(s2)) => LispSet(s1.intersect(s2))
        case _ => LispNil()
      }
      case "list" => LispList(args.map{eval(env,_)})
      case "apply" => if (args.size != 2) LispNil() else eval(env,args.last) match {
        case LispList(elements) => eval(env,LispCall(args.head,elements))
        case _ => LispNil() // not a list
      }
      case "type" => if (args.size != 1) LispNil() else eval(env,args.head) match {
        case LispNil() => LispId("nil")
        case _: LispNumber => LispId("number")
        case LispBool(_) => LispId("boolean")
        case LispString(_) => LispId("string")
        case LispChar(_) => LispId("char")
        case LispKeyword(_) => LispId("keyword")
        case LispId(_) => LispId("identifier")
        case LispList(_) => LispId("list")
        case LispSet(_) => LispId("set")
        case LispDict(_) => LispId("dict")
        case LispLambda(_,_) => LispId("lambda/function")
        case LispCall(_,_) => LispId("function call")
        case LispClosure(_,_) => LispId("closure")
        case LispIf(_,_,_) => LispId("if expression")
        case LispLet(_,_) => LispId("let expression")
        case LispFor(_,_,_) => LispId("for comprehension")
        case _ => LispId("unknown/other")
      }
      case _ => LispNil() // unknown/unimplemented function
    }
  }

  def bindToEnv(name: LispId, expr: LispExpr): Unit = {
    GLOBAL_ENV.put(name, expr)
  }

  def isDefined(env: Env, expr: LispExpr) = expr match {
    case id @ LispId(_) => env.contains(id)
    case _ => false
  }

  def lookup(env: Env, s: LispId) = env.getOrElse(s, LispNil())

  def eval(expr: LispExpr): LispExpr = eval(GLOBAL_ENV, expr)

  def eval(env: Env, expr: LispExpr): LispExpr = {
    expr match {
      case s @ LispId(name) => if (primitiveFunctions.contains(name)) s else lookup(env,s)
      case lambda @ LispLambda(_,_) => LispClosure(env, lambda)
      case LispCall(funexp, actuals) => {
        funexp match {
          case LispId(name) if (primitiveFunctions.contains(name)) => applyFunction(env, name, actuals) // ex: tail, +
          case _ => {
            // if it is a defined function stored in the environment, evaluate to lookup the underlying expression bound to the id,
            // then evaluate the bound expression
            // else it is a lambda and the evaluation returns a closure, ex: ((fn [x] (...)) arg)
            val e = if (isDefined(env, funexp)) eval(env, eval(env, funexp)) else eval(env, funexp)
            e match {
              case LispClosure(closure_env, LispLambda(formals, body)) => {
                val args = actuals.map{eval(env,_)}
                // arity check -> number of actual arguments should match number of formal arguments
                if (args.size != formals.size) LispNil() else {
                  val new_env = closure_env ++ formals.zip(args).toMap
                  eval(new_env,body)
                }
              }
              // case where the expression e is not a closure, but a primitive function 
              // the case of higher order functions:
              // a primitive function is bound to a variable in the environment and variable is passed around as an argument
              case LispId(name) if (primitiveFunctions.contains(name)) => applyFunction(env, name, actuals)
              case _ => LispNil() // error: neither a closure, nor a primitive function
            }
          }
        }
      }
      case LispIf(condition, thenBody, elseBody) => {
        eval(env, condition) match {
          case LispBool(true) => eval(env, thenBody)
          case _ => elseBody match {
            case Some(b) => eval(env, b)
            case _ =>  LispNil()
          }
        }
      }
      case LispLet(bindings, body) => {
        if (bindings.exists(bindsPrimitive)) throw new RuntimeException("Attempt to bind primitive")
        val new_env = bindings.foldLeft(env){(acc_env,bind) => acc_env + (bind.id -> eval(acc_env,bind.e))}
        eval(new_env,body)
      }
      case LispFor(bindings, condition, ret) => {
        def gen_envs(new_env: Env, bs: List[LispBinding]): List[Env] = {
          if (bs.isEmpty) List(new_env) // finished recursing, return environment
          else {
            val elements = eval(new_env, bs.head.e) match {
              case LispList(es) => es
              case _ => Nil // empty list, if expression evaluates to anything else than LispList
            }
            if (elements.isEmpty) Nil // return empty list
            else elements.flatMap{e:LispExpr => 
              val var_binding = LispBinding(bs.head.id, e)
              gen_envs(new_env + (var_binding.id -> var_binding.e), bs.tail)
            }
          }
        }
        val values = gen_envs(env, bindings).filter{new_env =>
          condition match {
            case Some(c) => eval(new_env, c) match {
              case LispBool(b) => b
              case _ => false
            }
            case None => true
          }
        }.map{new_env =>
          eval(new_env, ret)
        }
        LispList(values)
      }
      case LispDef(name,expr) => {
        val e = eval(env,expr)
        e match {
          case LispLambda(_,_) => bindToEnv(name,expr)
          case LispClosure(_,_) => bindToEnv(name,expr)
          case _ => bindToEnv(name,e)
        }
        name // LispNil()
      }
      case _ => expr // catch all clause (atoms, nil evaluate to themselves)
    }
  }

}

