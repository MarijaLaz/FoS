package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/** This object implements a parser and evaluator for the
 *  simply typed lambda calculus found in Chapter 9 of
 *  the TAPL book.
 */
object SimplyTypedExtended extends  StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+",
                              "=>", "|", "!", ":=", ";")
  lexical.reserved   ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
                              "pred", "iszero", "let", "in", "fst", "snd", "fix", "letrec",
                              "case", "of", "inl", "inr", "as", "unit", "ref", "Ref", "Unit")

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
                          | ("let"~>ident)~(":"~>lambda_type)~("="~>assn_term)~("in"~>assn_term)^^{case variable ~ l_type ~ t1 ~ t2 => App(Abs(variable,l_type,t2),t1)}
                          | ("{"~>term)~","~(term<~"}")^^{case t1~_~t2 => TermPair(t1, t2)}
                          | ("fst"~>term)^^{case t => First(t)}
                          | ("snd"~>term)^^{case t => Second(t)}
                          | ("inl"~>term)~("as"~>lambda_type)^^{case t~l_type => Inl(t, l_type)}                                    
                          | ("inr"~>term)~("as"~>lambda_type)^^{case t~l_type => Inr(t, l_type)}                                    
                          | ("case"~>term)~("of"~"inl"~>ident)~("=>"~>term)~("|"~"inr"~>ident)~("=>"~>term)^^{case t~x1~t1~x2~t2 => Case(t, x1, t1, x2, t2)} 
                          | ("fix"~>term)^^{case t => Fix(t)}
                          | ("letrec"~>ident)~(":"~>lambda_type)~("="~>term)~("in"~>term)^^{case x~tpe1~t1~t2 => App(Abs(x,tpe1,t2),Fix(Abs(x,tpe1,t1)))}
                          | "unit"^^^UnitVal
                          | ("ref"~>term)^^{case t1 => Ref(t1)}
                          | ("!"~>term)^^{case t1 => Deref(t1)}


  def numericLitRecursive(x: Int): Term = x match {
    case 0 => Zero
    case _ => Succ(numericLitRecursive(x-1))
  }

  def assn_term: Parser[Term] =
    termlet ~ rep(termlet) ^^ {case tlet ~ tlist => tlist.foldLeft(tlet)(App(_, _))}

  def seq_term: Parser[Term] =
    repsep(assn_term, ":=") ^^ {case assn_list => assn_list.reduceLeft(Assign(_, _))}
  
  def term: Parser[Term] =
    repsep(seq_term, ";") ^^ {case seq_list => seq_list.reduceLeft(Sequence(_, _))}

//------------------- [END] TERMS ----------------------------------
//------------------- [START] TYPES --------------------------------
  def lambda_typelet: Parser[Type] = "Bool"^^^TypeBool
                                  | "Nat"^^^TypeNat
                                  | "("~lambda_type~")"^^{case _~x~_=>x}
                                  | "Unit"^^^TypeUnit
                                  | "Ref"~>lambda_type^^{case x => TypeRef(x)}

  def pairOrSum(left: ~[Type,String], right: Type): Type = left match {
    case leftside ~ str if str == "+" => TypeSum(leftside, right)
    case leftside ~ str if str == "*" => TypePair(leftside, right)
  }

  def pair_sum_type: Parser[Type] = 
    rep(lambda_typelet ~ ("*"|"+")) ~ lambda_typelet ^^{case types~nextType => types.foldRight(nextType)(pairOrSum(_, _))}
  
  def lambda_type: Parser[Type] =
    rep1sep(pair_sum_type,"->")^^{case types => types.reduceRight(TypeFun(_, _))}

//------------------- [END] TYPES --------------------------------
//------------------- [START] Functions from last assignement ----
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
      case Inl(tt, tpe) => is_val(tt)
      case Inr(tt, tpe) => is_val(tt)
      case UnitVal => true
      case Loc(_) => true
      case _ => false
  }

  var counter: Int = 0
  def alpha(t: Abs): Abs = {
    var new_name: String = t.v+counter.toString
    counter+=1
    
    def replace(term:Term, valueToBeReplaced: String, new_value: String): Term = {
      term match {
        // untyped
        case Var(x) => if(x==valueToBeReplaced){Var(new_name)}else{Var(x)}
        case Abs(x,type1,t1) => if(x==valueToBeReplaced){Abs(x,type1,t1)}else{Abs(x, type1, replace(t1, valueToBeReplaced, new_value))}
        case App(t1, t2) => App(replace(t1, valueToBeReplaced, new_value),replace(t2, valueToBeReplaced, new_value))

        // arithmetic
        case True => True
        case False => False
        case Zero => Zero
        case Succ(t) => Succ(replace(t, valueToBeReplaced, new_value))
        case Pred(t) => Pred(replace(t, valueToBeReplaced, new_value))
        case If(cond, t1, t2) => If(replace(cond, valueToBeReplaced, new_value), replace(t1, valueToBeReplaced, new_value), replace(t2, valueToBeReplaced, new_value))
        case IsZero(t) => IsZero(replace(t, valueToBeReplaced, new_value))

        // pairs
        case TermPair(t1, t2) => TermPair(replace(t1, valueToBeReplaced, new_value), replace(t2, valueToBeReplaced, new_value))
        case First(t) => First(replace(t, valueToBeReplaced, new_value))
        case Second(t) => Second(replace(t, valueToBeReplaced, new_value))

        // sum
      }
    } 
    var new_term: Term = replace(t.t, t.v, new_name)
    return Abs(new_name,t.tp,new_term)

  }

  def subst(t: Term, x: String, s: Term): Term = {
    
    def isFV(s: Term, y: String): Boolean = {
      s match{
        // untyped
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

        // sum
        case Inl(tt, tpe) => isFV(tt, y)
        case Inr(tt, tpe) => isFV(tt, y)
        case Case(tt, x1, t1, x2, t2) => isFV(tt, y) || isFV(t1, y) || isFV(t2, y)

        // fix
        case Fix(tt) => isFV(tt, y)
      }
    }
    // println("");
    // println("To be substituted into: t=" +t);
    // println("Variable of substitution: x=" + x);
    // println("To substitute with: s=" + s);
    // println("");
    t match {
      // Untyped
      case Var(y) => {if(y==x){s}else{Var(y)}}
      // case Abs(y,type1,t1) => if(y==x){Abs(y,type1,t1)}else{if(!isFV(s, y)){Abs(y,type1,subst(t1, x, s))}else{
      //    subst(alpha(Abs(y,type1,t1)), x, s)
      // }}
      case Abs(y,type1,t1) => if(y==x){Abs(y,type1,t1)}else{Abs(y,type1,subst(t1, x, s))}
      case App(t1, t2) => App(subst(t1, x, s), subst(t2, x, s))
      
      // Arithmetics
      case True => True
      case False => False
      case Zero => Zero
      case Succ(t1) => Succ(subst(t1,x,s))
      case Pred(t1) => Pred(subst(t1,x,s))
      case If(cond, t1, t2) => If(subst(cond, x, s), subst(t1, x, s), subst(t2, x, s))
      case IsZero(t1) => IsZero(subst(t1,x,s))

      // Pairs
      case TermPair(t1, t2) => TermPair(subst(t1, x, s), subst(t2, x, s))
      case First(t1) => First(subst(t1, x, s))
      case Second(t1) => Second(subst(t1, x, s))

      // Sum
      case Inl(t1, tpe) => Inl(subst(t1,x,s), tpe)
      case Inr(t1, tpe) => Inr(subst(t1,x,s), tpe)
      case Case(tt, x1, t1, x2, t2) if x1!=x && x2!=x => Case(subst(tt,x,s), x1, subst(t1,x,s), x2, subst(t2,x,s))
      case Case(tt, x1, t1, x2, t2) if x1!=x && x2==x => Case(subst(tt,x,s), x1, subst(t1,x,s), x2, t2)
      case Case(tt, x1, t1, x2, t2) if x1==x && x2!=x => Case(subst(tt,x,s), x1, t1, x2, subst(t2,x,s))
      case Case(tt, x1, t1, x2, t2) if x1==x && x2==x => Case(subst(tt,x,s), x1, t1, x2, t2)

      // Fix
      case Fix(t1) => Fix(subst(t1,x,s))
      
      // Location
      case Loc(_) => t

      // Ref
      case Ref(t1) => Ref(subst(t1, x, s))

      // Deref
      case Deref(t1) => Deref(subst(t1, x, s))

      // Assign
      case Assign(t1, t2) => Assign(subst(t1, x, s), subst(t2, x, s))

      // Sequence
      case Sequence(t1, t2) => Sequence(subst(t1, x, s), subst(t2, x, s))

      // Unit
      case UnitVal => UnitVal
    }
  }
//------------------- [END] Functions from last assignement ------

  /** Call by value reducer with a store. */
  def reduce(t: Term, store: Store): (Term, Store) = {
    /* If you implement the imperative bundle, implement this method instead
     * of `reduce(t: Term): Term` below.
     * The default implementation is to always ignore the store.
     */



    t match {
      case Sequence(t1, t2) => t1 match
        case UnitVal => reduce(t2, store)
        case _ => (Sequence(reduce(t1, store)._1, t2), store)
      case Ref(t1) => {
        if(is_val(t1)){
          var new_l = Location.fresh()
          println("Value " + t1 + " stored at " + new_l);
          println("Store looks like (before): " + store)
          store.addOrReplace(new_l, t1);
          println("Store looks like (after): " + store)
          println(store.get(new_l))
          (Loc(new_l), store)
        }
        else{
          (Ref(reduce(t1, store)._1), store)
        }
      }
      case Deref(t1) => t1 match
        case Loc(l) => {
          println("Term in question: " + t);
          println("Label: " + l);
          println("Store looks like: " + store)
          println("Term in store at above location: " + store.get(l));
          (store.get(l).get, store)
        }
        case _ => (Deref(reduce(t1, store)._1), store)

      case Assign(t1, t2) => {
        if(is_val(t1)){
          t1 match {
            case Loc(l) if(is_val(t2)) => (UnitVal, store.addOrReplace(l, t2))
            case _ => (Assign(t1, reduce(t2, store)._1), store)
          }
        }
        else{
          (Assign(Deref(reduce(t1, store)._1), t2), store)
        }
      }
      
      
      case If(True, t1, _) => (t1, store)
      case If(False, _, t2) => (t2, store)
      case IsZero(Zero) => (True, store)
      case IsZero(Succ(nv)) => (False, store)
      case Pred(Zero) => (Zero, store)
      case Pred(Succ(nv)) => (nv, store)
      case If(t1, t2, t3) => (If(reduce(t1, store)._1, t2, t3), store)
      case IsZero(t1) => (IsZero(reduce(t1, store)._1), store)
      case Pred(t1) => (Pred(reduce(t1, store)._1), store)
      case Succ(t1) => (Succ(reduce(t1, store)._1), store)

      //untyped
      case App(tt, s) =>

        tt match {
        case Abs(x1, type1, t1) => {
          if(is_val(s)){
              (subst(t1, x1, s), store)
          }
          else{
            reduce(App(tt, reduce(s, store)._1), store)
          }
        }
        case _ =>
          if(is_val(tt)){
            reduce(App(tt, reduce(s, store)._1), store)
          }
          else
            reduce(App(reduce(tt, store)._1, s), store)
      }

      //pairs
      case First(TermPair(t1,t2))
        if(is_val(t1) && is_val(t2))
          => (t1, store)

      case Second(TermPair(t1,t2))  
        if(is_val(t1) && is_val(t2))
          => (t2, store)
      
      case First(t1) => (First(reduce(t1, store)._1), store)
      case Second(t1) => (Second(reduce(t1, store)._1), store)
      case TermPair(t1, t2) => 
        if(is_val(t1))
          (TermPair(t1, reduce(t2, store)._1), store)
        else
          (TermPair(reduce(t1, store)._1,t2), store)
      
      //sum
      case Case(tt, x1, t1, x2, t2) => tt match {
        case Inl(v0, tpe) => (subst(t1,x1,v0), store)
        case Inr(v0, tpe) => (subst(t2,x2,v0), store)
        case _ => (Case(reduce(tt, store)._1, x1, t1, x2, t2), store)
      }
      case Inl(tt, tpe) => (Inl(reduce(tt, store)._1,tpe), store)
      case Inr(tt, tpe) => (Inr(reduce(tt, store)._1,tpe), store)

      //fix
      case Fix(tt) => tt match {
        case Abs(x, type1, t2) => (subst(t2, x, Fix(tt)), store)
        case _ => (Fix(reduce(tt, store)._1), store)
      }

      case _ => throw new NoRuleApplies(t)
    }
  }

  /** Call by value reducer. */
  // def reduce(t: Term): Term = t match {
  //   //arithmetic
  //   case If(True, t1, _) => t1
  //   case If(False, _, t2) => t2
  //   case IsZero(Zero) => True
  //   case IsZero(Succ(nv)) => False
  //   case Pred(Zero) => Zero
  //   case Pred(Succ(nv)) => nv
  //   case If(t1, t2, t3) => If(reduce(t1), t2, t3)
  //   case IsZero(t1) => IsZero(reduce(t1))
  //   case Pred(t1) => Pred(reduce(t1))
  //   case Succ(t1) => Succ(reduce(t1))

  //   //untyped
  //   case App(tt, s) =>
  //     tt match {
  //     case Abs(x1, type1, t1) => {
  //       if(is_val(s)){
  //           subst(t1, x1, s)
  //       }
  //       else{
  //         App(tt, reduce(s))
  //       }
  //     }
  //     case _ =>
  //       if(is_val(tt)){
  //         App(tt, reduce(s))
  //       }
  //       else
  //         App(reduce(tt), s)
  //   }

  //   //pairs
  //   case First(TermPair(t1,t2))
  //     if(is_val(t1) && is_val(t2))
  //       => t1

  //   case Second(TermPair(t1,t2))  
  //     if(is_val(t1) && is_val(t2))
  //       => t2
    
  //   case First(t1) => First(reduce(t1))
  //   case Second(t1) => Second(reduce(t1))
  //   case TermPair(t1, t2) => 
  //     if(is_val(t1))
  //       TermPair(t1, reduce(t2))
  //     else
  //       TermPair(reduce(t1),t2)
    
  //   //sum
  //   case Case(tt, x1, t1, x2, t2) => tt match {
  //     case Inl(v0, tpe) => subst(t1,x1,v0)
  //     case Inr(v0, tpe) => subst(t2,x2,v0)
  //     case _ => Case(reduce(tt), x1, t1, x2, t2)
  //   }
  //   case Inl(tt, tpe) => Inl(reduce(tt),tpe)
  //   case Inr(tt, tpe) => Inr(reduce(tt),tpe)

  //   //fix
  //   case Fix(tt) => tt match {
  //     case Abs(x, type1, t2) => subst(t2, x, Fix(tt))
  //     case _ => Fix(reduce(tt))
  //   }

  //   case _ => throw new NoRuleApplies(t)
  // }

  /** Thrown when no reduction rule applies to the given term. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** Print an error message, together with the position where it occured. */
  case class TypeError(t: Term, msg: String) extends Exception(msg) {
    override def toString = msg + "\n" + t
  }

  /** The context is a list of variable names paired with their type. */
  type Context = List[(String, Type)]

  def error_msg(t_exp: String, t_found: String): String = {
    t_exp + " type expected but " + t_found + " found"
  }

  /** Returns the type of the given term <code>t</code>.
   *
   *  @param ctx the initial context
   *  @param t   the given term
   *  @return    the computed type
   */
  def typeof(ctx: Context, t: Term): Type = t match {
    // arithmetics
    case True => TypeBool
    case False => TypeBool
    case Zero => TypeNat
    case Pred(t1) => 
      if(typeof(ctx, t1)==TypeNat)
        TypeNat
      else
        throw new TypeError(t, error_msg(TypeNat.toString(), typeof(ctx, t1).toString()))
    case Succ(t1) => 
      if(typeof(ctx, t1)==TypeNat)
        TypeNat
      else
        throw new TypeError(t, error_msg(TypeNat.toString(), typeof(ctx, t1).toString()))
    case IsZero(t1) => 
      if(typeof(ctx, t1)==TypeNat)
        TypeBool
      else
        throw new TypeError(t, error_msg(TypeNat.toString(), typeof(ctx, t1).toString()))
    case If(cond, t1, t2) => 
      if((typeof(ctx,cond)==TypeBool) && (typeof(ctx,t1)==typeof(ctx,t2)))
        typeof(ctx,t1)
      else if(typeof(ctx,cond)!=TypeBool)
        throw new TypeError(t, error_msg(TypeBool.toString(), typeof(ctx, t1).toString()))
      else
        throw new TypeError(t, "then has type " + typeof(ctx, t1) + " else has type " + typeof(ctx, t2))
    
    // untyped
    case Var(x) => {if (ctx.exists(_._1 == x))  ctx.find(_._1 == x).get._2 else throw new TypeError(t, "Doesn't exist")}

    case Abs(x, type1, t1) =>
      TypeFun(type1, typeof((x, type1) +: ctx, t1))

    case App(t1, t2) =>
      typeof(ctx, t1) match {// T1 -> T2, this will be when t1 is an abstraction, right?
        case TypeFun(type11, type12) => {
            if(type11 == typeof(ctx, t2)) {
              type12
            }
            else{
              throw new TypeError(t, error_msg(type11.toString(), typeof(ctx, t2).toString())) // term on the right of app does not match input stipulated by the abs on the left
            }
          }
          case _ => throw new TypeError(t, error_msg("Fun", typeof(ctx, t1).toString())) // always need an abs on the left on app
      }

    // pairs
    case TermPair(t1, t2) => TypePair(typeof(ctx, t1), typeof(ctx, t2))

    case First(tt) => typeof(ctx, tt) match
      case TypePair(t1, _) => t1
      case _ => throw new TypeError(t, error_msg("Pair", typeof(ctx, tt).toString()))

    case Second(tt) => typeof(ctx, tt) match
      case TypePair(_, t2) => t2
      case _ => throw new TypeError(t, error_msg("Pair", typeof(ctx, tt).toString()))

    // sum
    case Inl(t1, tpe) => tpe match {
      case TypeSum(tt1, tt2) if typeof(ctx, t1)==tt1 => tpe
      case _ => throw new TypeError(t, error_msg("Inl", typeof(ctx, t1).toString()))
    }
    case Inr(t1, tpe) => tpe match {
      case TypeSum(tt1, tt2) if typeof(ctx, t1)==tt2 => tpe
      case _ => throw new TypeError(t, error_msg("Inr", typeof(ctx, t1).toString()))
    }
    case Case(t0, x1, t1, x2, t2) => typeof(ctx, t0) match {
      case TypeSum(type1, type2) => {
        val type_t1 = typeof((x1, type1) +: ctx, t1)
        val type_t2 = typeof((x2, type2) +: ctx, t2)
        if(type_t1 == type_t2)
          type_t1
        else
          throw new TypeError(t, error_msg("Case", typeof(ctx, t1).toString()))
      }
    }

    // fix
    case Fix(tt) => typeof(ctx, tt) match {
      case TypeFun(tpe1, tpe2) if tpe1 == tpe2 => tpe1
      case _ => throw new TypeError(t, error_msg("Fix", typeof(ctx, tt).toString()))
    }

    // imperative
    case UnitVal => TypeUnit
    case Sequence(t1, t2) if(typeof(ctx, t1) == TypeUnit) => typeof(ctx, t2)
    case Ref(t1) => TypeRef(typeof(ctx, t1))
    case Deref(t1) => typeof(ctx, t1) match
      case TypeRef(tt1) => tt1
    case Assign(t1, t2) => typeof(ctx, t1) match
      case TypeRef(tt1) if(tt1 == typeof(ctx, t2)) => TypeUnit

    // case x
    //   if ctx.exists(_._1 == x) => 
    //     ctx.find(_._1 == x).get._2 
    
    case _ => throw new TypeError(t, "Parameter Type mismatch")
  }

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
  var st = fos.Store.empty
  def path(t: Term, reduce: (Term, Store) => (Term, Store)): LazyList[Term] =
    try {
      var t1 = reduce(t, st)
      LazyList.cons(t, path(t1._1, reduce))
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
      case e => {println(e); println("Hey")}
    }
  }
}
