package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.quoted.Expr

/** This object implements a parser and evaluator for the
 *  untyped lambda calculus found in Chapter 5 of
 *  the TAPL book.
 */
object Untyped extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".")
  import lexical.Identifier

  /** t ::= x
          | '\' x '.' t
          | t t
          | '(' t ')'
   */

  /* Use fold left for this
  * Useful link:
  * https://commitlogs.com/2016/09/10/scala-fold-foldleft-and-foldright/
  * 
  * foldleft implementation: rep(terms) -> reducelist.foldleft()(App(_, _))
  */

  // Let this be either the smallest unit, ie, either variable, abstraction or let it be ( term ) 
  def termlet =
    ident^^{x=>Var(x)}
    | ("\\"~>ident)~("."~>term)^^{ case variable ~ term1=> Abs(variable, term1)}
    | "("~term~")"^^{case _~x~_=>x}

    
  def term: Parser[Term] =
    termlet ~ rep(termlet) ^^ {case tlet ~ tlist => tlist.foldLeft(tlet)(App(_, _))}
    // in its full form: 
    // termlet ~ rep(termlet) ^^ {case tlet ~ tlist =>  tlist.foldLeft(tlet)((expsofar, newpart) => App(expsofar, newpart))}

  /** <p>
   *    Alpha conversion: term <code>t</code> should be a lambda abstraction
   *    <code>\x. t</code>.
   *  </p>
   *  <p>
   *    All free occurences of <code>x</code> inside term <code>t/code>
   *    will be renamed to a unique name.
   *  </p>
   *
   *  @param t the given lambda abstraction.
   *  @return  the transformed term with bound variables renamed.
   */
  var counter: Int = 0
  def alpha(t: Abs): Abs = {
    var new_name: String = t.v+counter.toString
    counter+=1
    
    def replace(term:Term, valueToBeReplaced: String, new_value: String): Term = {
      term match {
        case Var(x) => if(x==valueToBeReplaced){Var(new_name)}else{Var(x)}
        case Abs(x,t1) => if(x==valueToBeReplaced){Abs(x,t1)}else{Abs(x, replace(t1, valueToBeReplaced, new_value))}
        case App(t1, t2) => App(replace(t1, valueToBeReplaced, new_value),replace(t2, valueToBeReplaced, new_value))
      }
    } 
    var new_term: Term = replace(t.t, t.v, new_name)
    return Abs(new_name, new_term)

  }
  /** Straight forward substitution method
   *  (see definition 5.3.5 in TAPL book).
   *  [x -> s]t
   *
   *  @param t the term in which we perform substitution
   *  @param x the variable name
   *  @param s the term we replace x withsv
   *  @return  ...
   */
  def subst(t: Term, x: String, s: Term): Term = {
    
    def isFV(s: Term, y: String): Boolean = {
      s match{
        case Var(z) => if(z == y){return true}else{return false}
        case Abs(z, t1) => if (z==y){return false}else{return isFV(t1, y)}
        case App(t1, t2) => return isFV(t1, y) || isFV(t2, y)
      }
    }

    t match {
      case Var(y) => if(y==x){s}else{Var(y)}
      case Abs(y, t1) => if(y==x){Abs(y,t1)}else{if(!isFV(s, y)){Abs(y, subst(t1, x, s))}else{
         subst(alpha(Abs(y, t1)), x, s)
      }}
      case App(t1, t2) => App(subst(t1, x, s), subst(t2, x, s))
    }
  }

  // full beta reduction
  def full_beta(t: Term): Term = {
    t match {
      case Var(x) => throw new NoReductionPossible(t)
      case App(t1, t2) => t1 match
        case Var(_) => App(t1, full_beta(t2))
        case App(t11, t12) => App(full_beta(t1), t2)
        case Abs(v, t) => subst(t, v, t2)
      case Abs(x, t) => Abs(x, full_beta(t))
    }
  }

  // return true if can't reduce further, false if can
  def normal_form(t: Term): Boolean = {
    t match
      case Var(_) => return true

      case Abs(_, t1) => return normal_form(t1)

      case App(t1, t2) => t1 match
        case Abs(_, _) => return false
        case _ => return (normal_form(t1) && normal_form(t2)) // iff both t1 and t2 can't be reduced
  }

  
   def reduceNormalOrder(t: Term): Term = {
    t match
      case App(t1, t2) => t1 match
        case Abs(v, t3) => subst(t3, v, t2)
        case _ => // non-abstraction forms
          if(normal_form(t1))
            App(t1, reduceNormalOrder(t2))
          else
            App(reduceNormalOrder(t1), t2)

      case Abs(v, t1) => 
        if(normal_form(t1))
          throw new NoReductionPossible(t)
        else
          Abs(v, reduceNormalOrder(t1))

      case _ => throw new NoReductionPossible(t)

  }


  /** Term 't' does not match any reduction rule. */
  case class NoReductionPossible(t: Term) extends Exception(t.toString)

  /** Normal order (leftmost, outermost redex first).
   *
   *  @param t the initial term
   *  @return  the reduced term
   */

  // Can the term potentially evaluate by call by value
  def shouldSubst(term: Term): Boolean = term match {
    case Var(_) => false 
    case Abs(_,_) => false 
    case App(t1,t2) => t1 match {
      case Abs(_,_) => t2 match
        case Abs(_, _) => true // the value that is being substituted should also be a "value", not a variable
        case _ => shouldSubst(t2)
      
      case _ => shouldSubst(t1)
    }
  }


  /** Call by value reducer. */
  def reduceCallByValue(t: Term): Term = {
    t match
      case App(tt, s) => tt match
        case Abs(x1, t1) => 
          if(shouldSubst(s))
            App(tt, reduceCallByValue(s))
          else s match
            case Var(_) => throw new NoReductionPossible(t)
            case _ => subst(t1, x1, s)
        case Var(_) => throw new NoReductionPossible(t)
        case App(_, _) => App(reduceCallByValue(tt), s)
      case _ => throw new NoReductionPossible(t)
  }

  /** Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the method that reduces a term by one step.
   *  @return       the stream of terms representing the big reduction.
   */
  def path(t: Term, reduce: Term => Term): LazyList[Term] =
    try {
      var t1 = reduce(t)
      LazyList.cons(t, path(t1, reduce))
    } catch {
      case NoReductionPossible(_) =>
        LazyList.cons(t, LazyList.empty)
    }

  def main(args: Array[String]): Unit = {
    val stdin = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
    val tokens = new lexical.Scanner(stdin.readLine())
    phrase(term)(tokens) match {
      case Success(trees, _) =>
        println("normal order: ")
        for (t <- path(trees, reduceNormalOrder))
          println(t)
        println("call-by-value: ")
        for (t <- path(trees, reduceCallByValue))
          println(t)

      case e =>
        println(e)
    }
  }
}
