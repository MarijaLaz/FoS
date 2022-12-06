package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
sealed abstract class Term extends Positional

case object True extends Term {
  override def toString() = "true"
}
case object False extends Term {
  override def toString() = "false"
}
case object Zero extends Term {
  override def toString() = "0"
}
case class Succ(t: Term) extends Term {
  override def toString() = s"(succ $t)"
}
case class Pred(t: Term) extends Term {
  override def toString() = s"(pred $t)"
}
case class IsZero(t: Term) extends Term {
  override def toString() = s"(iszero $t)"
}
case class If(cond: Term, t1: Term, t2: Term) extends Term {
  override def toString() = s"if $cond then $t1 else $t2"
}

case class Var(name: String) extends Term {
  override def toString() = name
}
case class Abs(v: String, tp: Type, t: Term) extends Term {
  // override def toString() = s"(\\$v: $tp. $t)"
}
case class App(t1: Term, t2: Term) extends Term {
  // override def toString() = s"($t1 $t2)"
}
case class TermPair(t1: Term, t2: Term) extends Term {
  override def toString() = s"{ $t1, $t2 }"
}

case class First(t: Term) extends Term {
  override def toString() = s"(fst $t)"
}

case class Second(t: Term) extends Term {
  override def toString() = s"(snd $t)"
}

case class Inl(t: Term, tpe: Type) extends Term {
  override def toString() = s"(inl $t as $tpe)"
}

case class Inr(t: Term, tpe: Type) extends Term {
  override def toString() = s"(inr $t as $tpe)"
}

case class Case(t: Term, x1: String, t1: Term, x2: String, t2: Term) extends Term {
  override def toString() = s"(case $t of inl $x1 => $t1 | inr $x2 => $t2)"
}

case class Fix(t: Term) extends Term {
  override def toString() = s"(fix $t)"
}

// Imperative bundle

case object UnitVal extends Term {
  override def toString(): String = s"unit"
}

case class Sequence(t1: Term, t2: Term) extends Term {
  override def toString(): String = s"($t1; $t2)"
}

case class Ref(t1: Term) extends Term {
  override def toString(): String = s"(ref $t1)"
}

case class Deref(t1: Term) extends Term {
  override def toString(): String = s"(!$t1)"
}

case class Assign(t1: Term, t2: Term) extends Term {
  override def toString(): String = s"($t1 := $t2)"
}

case class Loc(location: Location) extends Term {
  override def toString(): String = s"$location"
}

// Functional bundle

case class TypeAbs(x: String, t2: Term) extends Term {
  override def toString(): String = s"(/\\$x. $t2)"
}

case class TypeApp(t1: Term, tp: Type) extends Term {
  override def toString(): String = s"($t1 [$tp])"
}

case class NilVal(tp: Type) extends Term {
  override def toString(): String = s"nil"
}

case class Cons(tp: Type, t1: Term, t2: Term) extends Term {
  override def toString(): String = s"(cons $t1 $t2)"
}

case class ListCase(t1: Term, t2: Term, x: String, y: String, t3: Term) extends Term {
  override def toString(): String = s"(case $t1 of nil => $t2 | cons $x $y => $t3)"
}

// Function/subtyping bundle

case class Record(fields: List[(String, Term)]) extends Term {
  override def toString(): String =
    if (fields.isEmpty) "{}"
    else fields.map((l, t) => s"$l = $t").mkString("{", ", ", ",}")
}

case class RecordProj(t1: Term, l: String) extends Term {
  override def toString(): String = s"($t1.$l)"
}

// Subtyping bundle

case class Ascription(t: Term, tp: Type) extends Term {
  override def toString(): String = s"($t as $tp)"
}

/** Abstract Syntax Trees for types. */
abstract class Type extends Positional

case object TypeBool extends Type {
  override def toString() = "Bool"
}
case object TypeNat extends Type {
  override def toString() = "Nat"
}
case class TypeFun(t1: Type, t2: Type) extends Type {
  override def toString() = s"($t1 -> $t2)"
}
case class TypePair(t1: Type, t2: Type) extends Type {
  override def toString() = s"($t1 * $t2)"
}
case class TypeSum(t1: Type, t2: Type) extends Type {
  override def toString() = s"($t1 + $t2)"
}

// Imperative bundle

case object TypeUnit extends Type {
  override def toString(): String = s"Unit"
}

case class TypeRef(t1: Type) extends Type {
  override def toString(): String = s"(Ref $t1)"
}

/** Location in the store */
final class Location private (private val id: Int) {
  override def toString(): String = s"<loc $id>"
}

object Location {
  private val nextID = new java.util.concurrent.atomic.AtomicInteger(0)

  def fresh(): Location = new Location(nextID.incrementAndGet())

  implicit object LocationOrdering extends Ordering[Location] {
    def compare(x: Location, y: Location): Int = Integer.compare(x.id, y.id)
  }
}

/** Store */
final class Store private (underlying: Map[Location, Term]) {
  def get(location: Location): Option[Term] = underlying.get(location)

  def addOrReplace(location: Location, value: Term): Store =
    new Store(underlying + (location -> value));
    // val x = new Store(underlying + (location -> value));
    // println("Store looks like (during) " + x);
    // return x

  override def toString(): String =
    if (underlying.isEmpty) "{}"
    else underlying.toList.sortBy(_._1).map((l, v) => s"$l -> $v").mkString("{\n", ",\n", "\n}")
}

object Store {
  def empty: Store = new Store(Map.empty)
}

// Functional bundle

case class TypeVar(name: String) extends Type {
  override def toString(): String = s"$name"
}

case class TypeUniversal(x: TypeVar, t1: Type) extends Type {
  override def toString(): String = s"(\\/$x. $t1)"
}

case class TypeList(t1: Type) extends Type {
  override def toString(): String = s"(List $t1)"
}

// Function/subtyping bundle

case class TypeRecord(fieldTypes: List[(String, Type)]) extends Type {
  override def toString(): String =
    if (fieldTypes.isEmpty) "{}"
    else fieldTypes.map((l, t) => s"$l: $t").mkString("{", ", ", ",}")
}
