package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/** This object implements a parser and evaluator for the
 *  simply typed lambda calculus found in Chapter 9 of
 *  the TAPL book.
 */
object SimplyTypedExtended extends  StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+",
                              "=>", "|")
  lexical.reserved   ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
                              "pred", "iszero", "let", "in", "fst", "snd", "fix", "letrec",
                              "case", "of", "inl", "inr", "as")

//------------------- [START] TERMS ----------------------------------
  /** t ::=          "true"
   *               | "false"
   *               | number
   *               | "succ" t
   *               | "pred" t
   *               | "iszero" t
   *               | "if" t "then" t "else" t
   *               | ident
   *               | "\" ident ":" T "." t
   *               | t t
   *               | "(" t ")"
   *               | "let" ident ":" T "=" t "in" t
   *               | "{" t "," t "}"
   *               | "fst" t
   *               | "snd" t
   *               | "inl" t "as" T
   *               | "inr" t "as" T
   *               | "case" t "of" "inl" ident "=>" t "|" "inr" ident "=>" t
   *               | "fix" t
   *               | "letrec" ident ":" T "=" t "in" t
   */

  def termlet: Parser[Term] =  "true"^^^True 
                          | "false"^^^False 
                          | "if"~term~"then"~term~"else"~term^^{ case _ ~ cond ~ _ ~ ifTrue ~ _ ~ ifFalse => If(cond, ifTrue, ifFalse)}
                          | numericLit^^(x=>numericLitRecursive(x.toInt))
                          | "succ"~term^^{ case _~term => Succ(term)}
                          | "pred"~term^^{ case _~term => Pred(term)}
                          | "iszero"~term^^{ case _~term => IsZero(term) }
                          | ident^^{x=>Var(x)}
                          | ("\\"~>ident)~(":"~>lambda_type)~("."~>term)^^{ case variable ~ l_type ~ term1=> Abs(variable, l_type, term1)} 
                          | "("~term~")"^^{case _~x~_=>x}
                          | ("let"~>ident)~(":"~>lambda_type)~("="~>term)~("in"~>term)^^{case variable ~ l_type ~ t1 ~ t2 => App(Abs(variable,l_type,t2),t1)}
                          | ("{"~>term)~","~(term<~"}")^^{case t1~_~t2 => TermPair(t1, t2)}
                          | ("fst"~>term)^^{case t => First(t)}
                          | ("snd"~>term)^^{case t => Second(t)}
                          | ("inl"~>term)~("as"~>lambda_type)^^{case t~l_type => Inl(t, l_type)}                                    
                          | ("inr"~>term)~("as"~>lambda_type)^^{case t~l_type => Inr(t, l_type)}                                    
                          | ("case"~>term)~("of"~"inl"~>ident)~("=>"~>term)~("|"~"inr"~>ident)~("=>"~>term)^^{case t~x1~t1~x2~t2 => Case(t, x1, t1, x2, t2)} 


  def numericLitRecursive(x: Int): Term = x match {
    case 0 => Zero
    case _ => Succ(numericLitRecursive(x-1))
  }

  def term: Parser[Term] =
    termlet ~ rep(termlet) ^^ {case tlet ~ tlist => tlist.foldLeft(tlet)(App(_, _))}

//------------------- [END] TERMS ----------------------------------
//------------------- [START] TYPES --------------------------------
  def lambda_typelet: Parser[Type] = "Bool"^^^TypeBool
                                  | "Nat"^^^TypeNat
                                  | "("~lambda_type~")"^^{case _~x~_=>x}

  def pairOrSum(left, right): Type = left match {
    case leftside ~ str if str == "+" => TypeSum(leftside, right)
    case leftside ~ str if str == "*" => TypePair(leftside, right)
  }

  def pair_sum_type: Parser[Type] = ???
    //rep(lambda_typelet ~ ("*"|"+")) ~ lambda_typelet ^^{case types~nextType => types.foldRight(nextType)(pairOrSum(_, _))} //doesn't work
  
  def lambda_type: Parser[Type] =
    rep1sep(pair_sum_type,"->")^^{case types => types.reduceRight(TypeFun(_, _))}

//------------------- [END] TYPES --------------------------------

  /** Call by value reducer with a store. */
  def reduce(t: Term, store: Store): (Term, Store) = {
    /* If you implement the imperative bundle, implement this method instead
     * of `reduce(t: Term): Term` below.
     * The default implementation is to always ignore the store.
     */
    (reduce(t), store)
  }

  /** Call by value reducer. */
  def reduce(t: Term): Term =
    ???

  /** Thrown when no reduction rule applies to the given term. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** Print an error message, together with the position where it occured. */
  case class TypeError(t: Term, msg: String) extends Exception(msg) {
    override def toString = msg + "\n" + t
  }

  /** The context is a list of variable names paired with their type. */
  type Context = List[(String, Type)]

  /** Returns the type of the given term <code>t</code>.
   *
   *  @param ctx the initial context
   *  @param t   the given term
   *  @return    the computed type
   */
  def typeof(ctx: Context, t: Term): Type =
    ???

  def typeof(t: Term): Type = try {
    typeof(Nil, t)
  } catch {
    case err @ TypeError(_, _) =>
      Console.println(err)
      null
  }

  /** Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the evaluation strategy used for reduction.
   *  @return       the stream of terms representing the big reduction.
   */
  def path(t: Term, reduce: Term => Term): LazyList[Term] =
    try {
      var t1 = reduce(t)
      LazyList.cons(t, path(t1, reduce))
    } catch {
      case NoRuleApplies(_) =>
        LazyList.cons(t, LazyList.empty)
    }

  def main(args: Array[String]): Unit = {
    val stdin = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
    val tokens = new lexical.Scanner(stdin.readLine())
    phrase(term)(tokens) match {
      case Success(trees, _) =>
        try {
          println("parsed: " + trees)
          println("typed: " + typeof(Nil, trees))
          for (t <- path(trees, reduce))
            println(t)
        } catch {
          case tperror: Exception => println(tperror.toString)
        }
      case e =>
        println(e)
    }
  }
}
