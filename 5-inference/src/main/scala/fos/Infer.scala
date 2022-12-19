package fos

var ctr = 0

object Infer {
  case class TypeScheme(params: List[TypeVar], tp: Type)
  type Env = List[(String, TypeScheme)]
  type Constraint = (Type, Type)

  case class TypeError(msg: String) extends Exception(msg)

  def collect(env: Env, t: Term): (Type, List[Constraint]) = t match {
    case True => (BoolType, Nil)
    case False => (BoolType, Nil)
    case Zero => (NatType, Nil)
    case Pred(tt) => {
      var type_constraint = collect(env, tt)
      var C_prime = type_constraint._2 :+ (type_constraint._1, NatType)
      (NatType, C_prime)
    }
    case Succ(tt) => {
      var type_constraint = collect(env, tt)
      var C_prime = type_constraint._2 :+ (type_constraint._1, NatType)
      (NatType, C_prime)
    }
    case IsZero(tt) => {
      var type_constraint = collect(env, tt)
      var C_prime = type_constraint._2 :+ (type_constraint._1, NatType)
      (BoolType, C_prime)
    }

    case If(cond, t1, t2) => {
      var type_constraint_cond = collect(env, cond)
      var type_constraint_t1= collect(env, t1)
      var type_constraint_t2 = collect(env, t2)
      var C_prime = type_constraint_cond._2 ++ type_constraint_t1._2 ++ type_constraint_t2._2
      C_prime = C_prime :+ (type_constraint_cond._1, BoolType)
      C_prime = C_prime :+ (type_constraint_t1._1, type_constraint_t2._1)
      (type_constraint_t1._1, C_prime)
    }
    case Var(name) if env.exists(_._1 == name) => (env.find(_._1 == name).get._2.tp, Nil)

    case Abs(v, tp, tt) => {
      tp match
        // Empty tree from parser
        case EmptyTypeTree() => {
          var new_type_var = new TypeVar(freshX("T")) // create custom type T, not sure about v though. 2nd last para Phase 1 course webpage
          // typeavars need to be unique, maybe v is unique? Probably doesn't matter
          var new_type_scheme = new TypeScheme(Nil, new_type_var) // X could be anything, no constraints yet
          var new_elem = (v, new_type_scheme)
          var new_env = env :+ new_elem // Gamma updated with new type scheme
          var type_constraint = collect(new_env, tt)
          (FunType(new_type_var, type_constraint._1), type_constraint._2)
        }
        case _ => {
          var new_type_scheme = new TypeScheme(Nil, tp.tpe) // not sure
          var new_elem = (v, new_type_scheme)
          var new_env = env :+ new_elem
          var type_constraint = collect(new_env, tt)
          (FunType(tp.tpe, type_constraint._1), type_constraint._2)
        }
      
    }
    
    case App(t1, t2) => {
      var type_constraint_t1= collect(env, t1)
      var type_constraint_t2 = collect(env, t2)
      var C_prime = type_constraint_t1._2 ++ type_constraint_t2._2
      var new_type_var = new TypeVar(freshX("X"))
      C_prime = C_prime :+ (type_constraint_t1._1, FunType(type_constraint_t2._1, new_type_var))
      (new_type_var, C_prime)
    }

    case _=> throw new TypeError(" ")
  }

  def freshX(x:String):String={
    ctr+=1
    return x+ctr.toString()
  }

  def unify(c: List[Constraint]): Type => Type = {
    
    if(c.isEmpty){
      return tpe=>tpe
    }

    c.head match {
      case (s, t) if s == t => unify(c.tail)
      case (s, t) => s match {
        case TypeVar(x) => {
          if(!appears(s, t)){
            var changed_constraints = mapList(c.tail,s,t)
            unify(changed_constraints).compose{tpe=>substitute(tpe,s,t)}
          } 
          else {
            throw new TypeError("")
          }
        }
        case FunType(s1, s2)=>{
          t match {
            case FunType(t1, t2) => {
              unify((s1,t1) :: (s2,t2) :: c.tail)
            }
            case TypeVar(y) => { // Y can be a type variable
              if(!appears(t, s)){
                var changed_constraints = mapList(c.tail,t,s)
                unify(changed_constraints).compose{tpe=>substitute(tpe,t,s)}
              } else {
                throw new TypeError("")
              }
            }
            case _ => throw new TypeError("")
          }
        }
        case _ => t match {
          case TypeVar(y)=>{
            if(!appears(t, s)){
              var changed_constraints = mapList(c.tail,t,s)
              unify(changed_constraints).compose{tpe=>substitute(tpe,t,s)}
            } else {
              throw new TypeError("")
            }
          }
          case _ => throw new TypeError("") // X is Bool, Y is Nat
        }
      }
    }
  }
  

  def appears(s: Type, t: Type): Boolean = {
    if(s==t){
      true
    }else{
      t match{
        case FunType(t1,t2) => (appears(s,t1) || appears(s,t2))
        case _ => false
      }
    }
  }

  def mapList(list: List[Constraint],s:Type,t:Type)={
    var changed_list = List.empty[Constraint]
    for(el<-list){
      var subst_tp1 = substitute(el._1,s,t)
      var subst_tp2 = substitute(el._2,s,t)
      changed_list = changed_list:+(subst_tp1,subst_tp2)
    }
    changed_list
  }

  def substitute(c_prime:Type, s:Type, t:Type):Type = {
    if(c_prime==s){
      t
    }else{
      c_prime match{
        case FunType(t1,t2) => FunType(substitute(t1,s,t), substitute(t2,s,t))
        case _ => c_prime
      }
    }

  }

}


