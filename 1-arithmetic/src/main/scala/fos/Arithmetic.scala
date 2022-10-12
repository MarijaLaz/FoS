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

  def isnumeric(x: Term): Boolean = x match {
    case Zero => true
    case Succ(t) => isnumeric(t)
    case Pred(t) => isnumeric(t) // !AM Should this be here? I really think so
    case _ => false
  }

  /** Perform big step evaluation (result is always a value.)
   *  If evaluation is not possible TermIsStuck exception with
   *  corresponding inner irreducible term should be thrown.
   */
  def eval(t: Term): Term = t match {
    
    // Value to Value
    case True => True // B-VALUE
    case False => False // B-VALUE
    case Zero => Zero // B-VALUE
    

    // B-SUCC
    case Succ(t1) => {if(isnumeric(t1)){Succ(eval(t1))}else if(t1==True | t1==False){throw new TermIsStuck((t))}else{throw new TermIsStuck((t))} } // Can do this in two ways, but clearly one is better
    // case Succ(t1) => {if(eval(t1)==True|eval(t1)==False){throw new TermIsStuck(t)}else{Succ(eval(t1))} }// B-SUCC: !AM throw error if t1 is true or false?
    // In the second method successor of a term, true and false have been hard-coded to throw an error. (Is that okay?)
   
   // Predecessor: B-PREDZERO & B-PREDSUCC; have to know if the term inside is a numeric value, hence the extra function
    case Pred(t1) => eval(t1) match {
      case Zero => {Zero}
      case Succ(nv) => {if(isnumeric(nv)){eval(nv)}else{throw new TermIsStuck(eval(t1))}}
      case _ => throw new TermIsStuck(t)
    }

    
    // B-ISZEROZERO & B-ISZEROSUCC
    case IsZero(t) => {if (eval(t)==Zero){True}else if(isnumeric(t)){False}else{throw new TermIsStuck(t)}} // Can do this in two ways, but clearly one is better
    // case IsZero(t) => {if (eval(t)==Zero){True}else if(eval(t)==True|eval(t)==False){throw new TermIsStuck(t)}else{False}} // B-ISZEROZERO
    
    // B-IFTRUE & B-IFFALSE
    case If(t1, t2, t3) => {if(eval(t1)==True){eval(t2)}else if(eval(t1)==False){eval(t3)}else{throw new TermIsStuck(t)}} // else if(isnumeric(t1))
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
