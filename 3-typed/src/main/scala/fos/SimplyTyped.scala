package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.annotation.newMain

/** This object implements a parser and evaluator for the
 *  simply typed lambda calculus found in Chapter 9 of
 *  the TAPL book.
 */
object SimplyTyped extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*")
  lexical.reserved   ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
                              "pred", "iszero", "let", "in", "fst", "snd")


  // -------------- TERMS ----------------------------//
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


  def numericLitRecursive(x: Int): Term = x match {
    case 0 => Zero
    case _ => Succ(numericLitRecursive(x-1))
  }

  def term: Parser[Term] =
    termlet ~ rep(termlet) ^^ {case tlet ~ tlist => tlist.foldLeft(tlet)(App(_, _))}

  
  // -------------- TYPES ----------------------------//

  def lambda_typelet: Parser[Type] = "Bool"^^^TypeBool
                                  | "Nat"^^^TypeNat
                                  | "("~lambda_type~")"^^{case _~x~_=>x}
  
  // why foldRight is not working?
  // def lambda_type: Parser[Type] =
  //   lambda_typelet ~ repsep(lambda_typelet,"->")^^{case type1 ~ types => types.foldRight(type1)(TypeFun(_, _))}
 
  // rep1sep[T](p: ⇒ Parser[T], q: ⇒ Parser[Any]) 
  // repeatedly applies p interleaved with q to parse the input, until p fails. The parser p must succeed at least once
  // so p=the term and q="->"

  def pair_type: Parser[Type] = 
    rep1sep(lambda_typelet,"*")^^{case types => types.reduceRight(TypePair(_, _))}
  //first applying the * because it has precedence 
  def lambda_type: Parser[Type] =
    rep1sep(pair_type,"->")^^{case types => types.reduceRight(TypeFun(_, _))}


  /** Thrown when no reduction rule applies to the given term. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** Print an error message, together with the position where it occured. */
  case class TypeError(t: Term, msg: String) extends Exception(msg) {
    override def toString =
      msg + "\n" + t
  }

  /** The context is a list of variable names paired with their type. */
  type Context = List[(String, Type)]

  //----------- [START] functions from previous HW -----------------------//
  var counter: Int = 0
  def alpha(t: Abs): Abs = {
    var new_name: String = t.v+counter.toString
    counter+=1
    
    def replace(term:Term, valueToBeReplaced: String, new_value: String): Term = {
      term match {
        case Var(x) => if(x==valueToBeReplaced){Var(new_name)}else{Var(x)}
        case Abs(x,type1,t1) => if(x==valueToBeReplaced){Abs(x,type1,t1)}else{Abs(x, type1, replace(t1, valueToBeReplaced, new_value))}
        case App(t1, t2) => App(replace(t1, valueToBeReplaced, new_value),replace(t2, valueToBeReplaced, new_value))

        //arithmetic
        case True => True
        case False => False
        case Zero => Zero
        case Succ(t) => Succ(replace(t, valueToBeReplaced, new_value))
        case Pred(t) => Pred(replace(t, valueToBeReplaced, new_value))
        case If(cond, t1, t2) => If(replace(cond, valueToBeReplaced, new_value), replace(t1, valueToBeReplaced, new_value), replace(t2, valueToBeReplaced, new_value))
        case IsZero(t) => IsZero(replace(t, valueToBeReplaced, new_value))

        //new terms from typed
        case TermPair(t1, t2) => TermPair(replace(t1, valueToBeReplaced, new_value), replace(t2, valueToBeReplaced, new_value))
        case First(t) => First(replace(t, valueToBeReplaced, new_value))
        case Second(t) => Second(replace(t, valueToBeReplaced, new_value))
      }
    } 
    var new_term: Term = replace(t.t, t.v, new_name)
    return Abs(new_name,t.tp,new_term)

  }

  def subst(t: Term, x: String, s: Term): Term = {
    
    def isFV(s: Term, y: String): Boolean = {
      s match{
        case Var(z) => if(z == y){return true}else{return false}
        case Abs(z,type1,t1) => if (z==y){return false}else{return isFV(t1, y)}
        case App(t1, t2) => return isFV(t1, y) || isFV(t2, y)
        
        // arithmetics
        case True => false
        case False => false
        case Zero => false
        case Succ(t) => isFV(t,y)
        case Pred(t) => isFV(t,y)
        case If(cond, t1, t2) => isFV(cond,y) || isFV(t1,y) || isFV(t2,y)
        case IsZero(t) => isFV(t,y)

        // pairs
        case TermPair(t1, t2) => isFV(t1, y) || isFV(t2, y)
        case First(t) => isFV(t, y)
        case Second(t) => isFV(t, y)
      }
    }

    t match {
      case Var(y) => if(y==x){s}else{Var(y)}
      case Abs(y,type1,t1) => if(y==x){Abs(y,type1,t1)}else{if(!isFV(s, y)){Abs(y,type1,subst(t1, x, s))}else{
         subst(alpha(Abs(y,type1,t1)), x, s)
      }}
      case App(t1, t2) => App(subst(t1, x, s), subst(t2, x, s))
      
      // Arithmetics
      case True => True
      case False => False
      case Zero => Zero
      case Succ(t) => Succ(subst(t,x,s))
      case Pred(t) => Pred(subst(t,x,s))
      case If(cond, t1, t2) => If(subst(cond, x, s), subst(t1, x, s), subst(t2, x, s))
      case IsZero(t) => IsZero(subst(t,x,s))

      // Pairs
      case TermPair(t1, t2) => TermPair(subst(t1, x, s), subst(t2, x, s))
      case First(t) => First(subst(t, x, s))
      case Second(t) => Second(subst(t, x, s))
    }
  }

  // Can the term potentially evaluate by call by value
  def canReduce(term: Term): Boolean = term match {
    case App(t1,t2) => t1 match {
      case Abs(_,_,_) => t2 match
        case Abs(_,_,_) => true // the value that is being substituted should also be a "value", not a variable
        case True => true 
        case False => true 
        case nv => true
        case _ => canReduce(t2)
      
      case _ => canReduce(t1)
    }
    //arithmetics  Don't know if we should add the values from arithmetics here
    case Succ(t) => canReduce(t)
    case If(cond, t1, t2) => {
      cond match {
        case True => canReduce(t1) && canReduce(t2)
        case False => canReduce(t1) && canReduce(t2)
        case _ => canReduce(If(reduce(cond), t1, t2))
        
      }
    }
    case Pred(t) => is_num(t)
    case IsZero(t) => is_num(t)
    case TermPair(t1, t2) => {
      if(canReduce(t1)){
        true
      }else{
        if(is_val(t1))
        canReduce((t2))
        else
          false
      }
    }
        
    case First(t) => canReduce(t)
    case Second(t) => canReduce(t)
    case _ => false
  }

//----------- [END] functions from previous HW -----------------------//

  def is_num(x: Term): Boolean = x match {
      case Zero => true
      case Succ(t) => is_num(t)
      case _ => false
    }

  def is_val(t: Term): Boolean = t match{
    case True => true
    case False => true
    case Zero => true
    case Succ(x) => is_num(x)
    case Abs(_, _, _) => true
    case TermPair(t1, t2) => is_val(t1) && is_val(t2)
    case _ => false
  }
  /** Call by value reducer. */
  def reduce(t: Term): Term =
    
    t match {
    //arithmetic
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


    //untyped
    case App(tt, s) =>
      tt match {
      case Abs(x1, type1, t1) => {
        if(is_val(s)){
            subst(t1, x1, s)
        }
        else{
          App(tt, reduce(s))
        }
      }
      case _ =>
        if(is_val(tt)){
          App(tt, reduce(s))
        }
        else
          App(reduce(tt), s)
    }
    //pairs
    case First(TermPair(t1,t2))
      if(is_val(t1) && is_val(t2))
        => t1
      // else{
      //   throw new NoRuleApplies(t)
      // }

    case Second(TermPair(t1,t2))  
      if(is_val(t1) && is_val(t2))
        => t2
      
      // else{
      //   throw new NoRuleApplies(t)
      // }
    
         
    case First(t1) => First(reduce(t1))
    case Second(t1) => Second(reduce(t1))
    case TermPair(t1, t2) => 
      if(is_val(t1))
        TermPair(t1, reduce(t2))
      else
        TermPair(reduce(t1),t2)

    case _ =>
      throw new NoRuleApplies(t)
  }
    
  def error_msg(t_found: Term, t_exp: Term): String = {
    return t_exp.toString + " type expected but " + t_found.toString
  }

  def gamma_has(ctx: Context, var_search: String): Boolean = {
    // println("Looking for (in func) " + var_search) // Search has a problem
    ctx.foreach {
      case(vname, typ) if(vname == var_search) =>
        // print("Found " + var_search)
        return true
    }
    // print("Couldn't find " + var_search)
    return false
  }

  def gamma_get(ctx: Context, var_search: String): Type = {
    ctx.foreach {
      case(vname, typ) if(vname == var_search) =>
        return typ
    }
    return TypeBool // will never reach here
  }

  /** Returns the type of the given term <code>t</code>.
   *
   *  @param ctx the initial context
   *  @param t   the given term
   *  @return    the computed type
   */
  def typeof(ctx: Context, t: Term): Type =
    // println(t)
    t match{
    case True => TypeBool
    case False => TypeBool
    case Zero => TypeNat
    case Pred(t1) => 
      if(typeof(ctx, t1)==TypeNat)
        TypeNat
      else
        throw new TypeError(t, "")
    case Succ(t1) => 
      if(typeof(ctx, t1)==TypeNat)
        TypeNat
      else
        throw new TypeError(t, "")
    case IsZero(t1) => 
      if(typeof(ctx, t1)==TypeNat)
        TypeBool
      else
        throw new TypeError(t, "")
    case If(cond, t1, t2) => 
      if((typeof(ctx,cond)==TypeBool) && (typeof(ctx,t1)==typeof(ctx,t2)))
        typeof(ctx,t1)
      else
        throw new TypeError(t, "")
      
    case Var(x)
    if ctx.exists(_._1 == x) => // This works
    // if gamma_has(ctx, x) => // This doesn't for some reason
      // println("Looking for " + x) // Search has a problem
      ctx.find(_._1 == x).get._2 // This works
      // gamma_get(ctx, x) // This doesn't for some reason

      
    case Abs(x, type1, t1) =>
      // ctx :+ List(x, type1) // Adding x to our context
      // println("-abs-")
      // println(x)
      // println(type1)
      // println(t1)
      // println("-abs-")
      // println("Adding " + x + " to context")
      // TypeFun(type1, typeof(ctx.++(x, type1), t1))
      //if ctx.exists(_._1 == x) => // This works
      var n_term = alpha(t.asInstanceOf[fos.Abs])
      var new_name = n_term.v
      var new_term = n_term.t
      TypeFun(type1, typeof(ctx :+ (new_name, type1) , new_term))
      // Prepend works, not append, probably because the first element stays null otherwise

    case App(t1, t2) =>
      typeof(ctx, t1) match {// T1 -> T2, this will be when t1 is an abstraction, right?
        case TypeFun(type11, type12) => {
            if(type11== typeof(ctx, t2)) {
              type12
            }else{
              throw new TypeError(t,"")
            }
          }
      }
      // case _: Type => throw new TypeError(t, "")

    case TermPair(t1, t2) => TypePair(typeof(ctx, t1), typeof(ctx, t2))

    // case First(tt) => tt match
    //   case TermPair(t1, _) => typeof(ctx, t1)
    //   case _ => throw new TypeError(t, "")

    case First(tt) => typeof(ctx, tt) match
      case TypePair(t1, _) => t1
      case _ => throw new TypeError(t, "")

    case Second(tt) => typeof(ctx, tt) match
      case TypePair(_, t2) => t2
      case _ => throw new TypeError(t, "")
    
    case _ => throw new TypeError(t,"")
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
        // for (t <- path(trees, reduce))
        //     println(t)
        try {
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
