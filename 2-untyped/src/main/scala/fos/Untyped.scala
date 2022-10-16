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
  * Useful links:
  * http://allaboutscala.com/tutorials/chapter-8-beginner-tutorial-using-scala-collection-functions/scala-foldleft-example/
  * https://stackoverflow.com/questions/7764197/difference-between-foldleft-and-reduceleft-in-scala
  * https://commitlogs.com/2016/09/10/scala-fold-foldleft-and-foldright/
  * 
  * Last one is especially useful! Do read!
  * 
  * foldleft implementation: rep(terms) -> reducelist.foldleft()(App(_, _))
  * 
  */
  def term: Parser[Term] =  
                           ident^^{x=>Var(x)}
                          | "\\"~ident~"."~term^^{ case _ ~ variable ~ _ ~ term1=> Abs(variable, term1)}
                          | "("~term~")"^^{case "("~x~")"=>x}
                          | term~term^^{case term1~term2 => App(term1,term2)}

  
  // def parentRecursive(t:Term): Term = t match{
  //   case "("~t1~")" => parentRecursive(t1)
  //   case Var(t1) => Var(t1)
  //   case Abs(x,t1) => Abs(x,t1)
  //   case
  // }



                            

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
  def alpha(t: Abs): Abs =
    ???

  /** Straight forward substitution method
   *  (see definition 5.3.5 in TAPL book).
   *  [x -> s]t
   *
   *  @param t the term in which we perform substitution
   *  @param x the variable name
   *  @param s the term we replace x with
   *  @return  ...
   */
  def subst(t: Term, x: String, s: Term): Term =
    ???

  /** Term 't' does not match any reduction rule. */
  case class NoReductionPossible(t: Term) extends Exception(t.toString)

  /** Normal order (leftmost, outermost redex first).
   *
   *  @param t the initial term
   *  @return  the reduced term
   */
  def reduceNormalOrder(t: Term): Term =
    ???

  /** Call by value reducer. */
  def reduceCallByValue(t: Term): Term =
    ???

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
