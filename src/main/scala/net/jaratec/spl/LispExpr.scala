package net.jaratec.spl

import scala.math._

trait LispExpr {
  def isNil = LispBool(false)
  def isAtom = LispBool(false)
  def isNumber = LispBool(false)
}

case class LispNil extends LispExpr {
  override def isNil = LispBool(true)
  override def toString(): String = "nil"
}

trait LispAtom extends LispExpr {
  override def isAtom = LispBool(true)
}

case class LispBool(val b: Boolean) extends LispAtom {
  def &&(other: LispBool) = LispBool(b && other.b)
  def ||(other: LispBool) = LispBool(b || other.b)
  override def toString(): String = if (b) "true" else "false"
}

abstract trait LispNumber extends LispAtom with Ordered[LispNumber] {
  override def isNumber = LispBool(true)
  def compare(n: LispNumber): Int
  def +(n: LispNumber): LispNumber
  def *(n: LispNumber): LispNumber
  def -(n: LispNumber): LispNumber
  def /(n: LispNumber): LispNumber
  def %(n: LispNumber): LispNumber
}

case class LispInt(val i: BigInt) extends LispNumber {
  def compare(other: LispNumber) = other match {
    case LispInt(oi) => if (i < oi) -1 else if (i > oi) 1 else 0
    case LispDecimal(od) => if (BigDecimal(i) < od) -1 else if (BigDecimal(i) > od) 1 else 0
  }
  def +(other: LispNumber) = other match {
    case LispInt(oi) => LispInt(i + oi)
    case LispDecimal(od) => LispDecimal(BigDecimal(i) + od)
  }
  def -(other: LispNumber) = other match {
    case LispInt(oi) => LispInt(i - oi)
    case LispDecimal(od) => LispDecimal(BigDecimal(i) - od)
  }
  def *(other: LispNumber) = other match {
    case LispInt(oi) => LispInt(i * oi)
    case LispDecimal(od) => LispDecimal(BigDecimal(i) * od)
  }
  def /(other: LispNumber) = other match {
    case LispInt(oi) => LispInt(i / oi)
    case LispDecimal(od) => LispDecimal(BigDecimal(i) / od)
  }
  def %(other: LispNumber) = other match {
    case LispInt(oi) => LispInt(i % oi)
    case LispDecimal(od) => LispDecimal(BigDecimal(i) % od)
  }
  override def toString(): String = i.toString
}

case class LispDecimal(val d: BigDecimal) extends LispNumber {
  def compare(other: LispNumber) = other match {
    case LispInt(oi) => if (d < BigDecimal(oi)) -1 else if (d > BigDecimal(oi)) 1 else 0
    case LispDecimal(od) => if (d < od) -1 else if (d > od) 1 else 0
  }
  def +(other: LispNumber) = other match {
    case LispInt(oi) => LispDecimal(d + BigDecimal(oi))
    case LispDecimal(od) => LispDecimal(d + od)
  }
  def -(other: LispNumber) = other match {
    case LispInt(oi) => LispDecimal(d - BigDecimal(oi))
    case LispDecimal(od) => LispDecimal(d - od)
  }
  def *(other: LispNumber) = other match {
    case LispInt(oi) => LispDecimal(d * BigDecimal(oi))
    case LispDecimal(od) => LispDecimal(d * od)
  }
  def /(other: LispNumber) = other match {
    case LispInt(oi) => LispDecimal(d / BigDecimal(oi))
    case LispDecimal(od) => LispDecimal(d / od)
  }
  def %(other: LispNumber) = other match {
    case LispInt(oi) => LispDecimal(d % BigDecimal(oi))
    case LispDecimal(od) => LispDecimal(d % od)
  }
  override def toString(): String = d.toString
}

case class LispChar(val c: Char) extends LispAtom with Ordered[LispChar] {
  def compare(other: LispChar): Int = if (c < other.c) -1 else if (c > other.c) 1 else 0
  def +(other: LispChar) = LispString(c.toString + other.c.toString)
  def +(other: LispString) = LispString(c.toString + other.s)
  override def toString(): String = "'" + c +  "'"
}

case class LispString(val s: String) extends LispAtom with Ordered[LispString] {
  def compare(other: LispString): Int = if (s < other.s) -1 else if (s > other.s) 1 else 0
  def +(other: LispChar) = LispString(s + other.c.toString)
  def +(other: LispString) = LispString(s + other.s)
  def +(other: LispInt) = LispString(s + other.i.toString)
  def +(other: LispDecimal) = LispString(s + other.d.toString)
  override def toString(): String = "\"" + s +  "\""
}

case class LispKeyword(val key: String) extends LispAtom {
  override def toString(): String = ":" + key
}

case class LispId(val id: String) extends LispAtom {
  override def toString(): String = id
}

case class LispEntry(val key: LispKeyword, val value: LispExpr) extends LispExpr {
  override def toString(): String = key + " " + value
}

// LispSeq doesn't pull its weight; keep it?
trait LispSeq extends LispExpr {
}

case class LispList(val elements: List[LispExpr]) extends LispSeq {
  override def toString(): String = "(" + elements.mkString(" ") + ")"
}

case class LispSet(val elements: Set[LispExpr]) extends LispSeq {
  override def toString(): String = "@(" + elements.mkString(" ") + ")"
}

case class LispDict(val entries: Map[LispKeyword,LispExpr]) extends LispSeq {
  override def toString(): String = "{" + entries.map{_.toString}.mkString(" ") + "}"
}

case class LispCall(val funexp:LispExpr, actuals: List[LispExpr]) extends LispExpr {
  override def toString(): String = "(" + funexp + " " + actuals.mkString(" ") + ")"
}

case class LispLambda(val formals: List[LispId], val body: LispExpr) extends LispExpr {
  override def toString(): String = "(fn [" + formals.mkString(" ") + "] " + body + ")"
}

case class LispDef(val id: LispId, val e: LispExpr) extends LispExpr {
  override def toString(): String = id + " : " + e
}

case class LispBinding(val id: LispId, val e: LispExpr) extends LispExpr {
  override def toString(): String = id + " <- " + e
}

case class LispLet(val bindings: List[LispBinding], val body: LispExpr) extends LispExpr {
  override def toString(): String = "(let [" + bindings.mkString(" ") + "] " + body + ")"
}

case class LispFor(val bindings: List[LispBinding], val condition: Option[LispCall], val ret: LispExpr) extends LispExpr {
  override def toString(): String = "(for [" + bindings.mkString(" ") + " :if " + condition + " :yield " + ret + "])"
}
