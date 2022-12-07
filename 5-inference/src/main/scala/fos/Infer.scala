package fos

object Infer {
  case class TypeScheme(params: List[TypeVar], tp: Type)
  type Env = List[(String, TypeScheme)]
  type Constraint = (Type, Type)

  case class TypeError(msg: String) extends Exception(msg)

  def collect(env: Env, t: Term): (Type, List[Constraint]) = t match {
    case True => (BoolType, List.empty)
    case False => (BoolType, List.empty)
    case Zero => (NatType, List.empty)
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
    case Var(name) if env.exists(_._1 == name) => (env.find(_._1 == name).get._2.tp, List.empty)
    case Abs(v, tp, tt) => {
      var new_type_scheme = new TypeScheme(List.empty, tp.tpe) // not sure
      var new_elem = (v, new_type_scheme)
      var new_env = env :+ new_elem
      var type_constraint = collect(new_env, tt)
      (FunType(tp.tpe, type_constraint._1), type_constraint._2)
    }
    case Abs
    

    
  }
  def unify(c: List[Constraint]): Type => Type = ???
}
