package net.jaratec.spl

import java.io._
import scala.io.Source
import scala.tools.jline.console.ConsoleReader

object Repl {

  val parser = new LispParser()
  val interpreter = new Interpreter()
  val reader = new ConsoleReader()

  def main(args: Array[String]) = {
    // load the prelude/stdlib
    load("stdlib.spl")
    if (args.size > 0) {
      load(args(0))
    }
    repl2
  }

  def load(fileName: String): LispExpr =  {
    try {
      // val str = Source.fromFile(new File(fileName)).mkString
      val str = Source.fromInputStream(getClass.getResourceAsStream("/" + fileName)).mkString
      val lst = parser.parse(str)
      println("expressions read:" + lst.size)
      lst.foreach{println(_)}
      parser.parse(str).foreach{interpreter.eval(_)}
    } catch {
      case e: Exception => e.printStackTrace()
    }
    LispNil()
  }

  def repl(): Unit = {
    var exit = false
    while (! exit) {
      print("lisp>>")
      val raw = readLine()
      raw match {
        case ":quit" => {
          println("bye for now")
          exit = true
        }
        case ":show" => println(interpreter.GLOBAL_ENV)
        case s if s.startsWith(":load") => {
          val fileName = s.stripPrefix(":load").trim
          val result = load(fileName)
          println(result)
        }
        case s if "".equals(s.trim) => ; // noop
        case _ => {
          try {
            val result = interpreter.eval(parser.parse(raw).head)
            println(result)
          } catch {
            case e: Exception => e.printStackTrace()
          }
        }
      }
    }
  }

  // JLine powered repl
  def repl2(): Unit = {
    var exit = false
    reader.setPrompt("lisp>>")
    reader.setHistoryEnabled(true)
    val out = new PrintWriter(reader.getOutput)
    while (! exit) {
      val raw = reader.readLine
      raw match {
        case ":quit" => {
          out.println("bye for now")
          exit = true
        }
        case ":show" => println(interpreter.GLOBAL_ENV)
        case s if s.startsWith(":load") => {
          val fileName = s.stripPrefix(":load").trim
          val result = load(fileName)
          out.println(result)
        }
        case s if "".equals(s.trim) => ; // noop
        case _ => {
          try {
            val result = interpreter.eval(parser.parse(raw).head)
            out.println(result)
          } catch {
            case e: Exception => e.printStackTrace()
          }
        }
      }
      out.flush()
    }
  }

}
