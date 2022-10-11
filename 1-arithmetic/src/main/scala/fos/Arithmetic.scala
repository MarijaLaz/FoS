package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/** This object implements a parser and evaluator for the NB
 *  language of booleans and numbers found in Chapter 3 of
 *  the TAPL book.
 */
object Arithmetic extends StandardTokenParsers {
  lexical.reserved ++= List("true", "false", "if", "then", "else", "succ", "pred", "iszero") // TOASK about adding zero

  import lexical.NumericLit

  /** term ::= 'true'
             | 'false'
             | 'if' term 'then' term 'else' term
             | numericLit
             | 'succ' term
             | 'pred' term
             | 'iszero' term
   */
  def term: Parser[Term] =  "true"^^^True 
                          | "false"^^^False 
                          | "if"~term~"then"~term~"else"~term^^{ case _ ~ cond ~ _ ~ ifTrue ~ _ ~ ifFalse => If(cond, ifTrue, ifFalse)}
                          | numericLit^^(x=>numericLitRecursive(x.toInt))
                          | "succ"~term^^{ case _~term => Succ(term)}
                          | "pred"~term^^{ case _~term => Pred(term)}
                          | "iszero"~term^^{ case _~term => IsZero(term) }

  def numericLitRecursive(x: Int): Term = x match {
    case 0 => Zero
    case _ => Succ(numericLitRecursive(x-1))
  }
 
  // note: you should use `numericLit` to parse numeric literals

  case class NoReductionPossible(t: Term) extends Exception(t.toString)

  /** Return a list of at most n terms, each being one step of reduction. */
  def path(t: Term, n: Int = 64): List[Term] =
    if (n <= 0) Nil
    else
      t :: {
        try {
          path(reduce(t), n - 1)
        } catch {
          case NoReductionPossible(t1) =>
            Nil
        }
      }

  /** Perform one step of reduction, when possible.
   *  If reduction is not possible NoReductionPossible exception
   *  with corresponding irreducible term should be thrown.
   */
  def reduce(t: Term): Term = t match {
    case If(True, t1, _) => t1
    case If(False, _, t2) => t2
    case IsZero(Zero) => True
    case IsZero(Succ(nv)) => False
    case Pred(Zero) => Zero
    case Pred(Succ(nv)) => nv
    case If(t1, t2, t3) => If(reduce(t1), t2, t3) 
    case IsZero(t1) => IsZero(reduce(t1))
    case Pred(t1) => Pred(reduce(t1))
    case Succ(t1) => Succ(reduce(t1))
    case _ => throw new NoReductionPossible(t)
  }

  case class TermIsStuck(t: Term) extends Exception(t.toString)

  /** Perform big step evaluation (result is always a value.)
   *  If evaluation is not possible TermIsStuck exception with
   *  corresponding inner irreducible term should be thrown.
   */
  def eval(t: Term): Term = t match {
    case True => True
    case False => False
    case Zero => Zero
    case Succ(t1) => Succ(eval(t1))
    case If(t1,t2,t3) => { if(eval(t1))==True{eval(t2)}else{eval(t3)}}
  }

  def main(args: Array[String]): Unit = {
    val stdin = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
    val tokens = new lexical.Scanner(stdin.readLine())
    phrase(term)(tokens) match {
      case Success(trees, _) =>
        for (t <- path(trees))
          println(t)
        try {
          print("Big step: ")
          println(eval(trees))
        } catch {
          case TermIsStuck(t) => println("Stuck term: " + t)
        }
      case e =>
        println(e)
    }
  }
}
