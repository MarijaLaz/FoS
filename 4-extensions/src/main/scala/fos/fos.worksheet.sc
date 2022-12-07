import fos.SimplyTypedExtended._
// import fos.Store
import fos.Location
def input(str: String) = new lexical.Scanner(str)
def parse[T](str: String, parser: Parser[T]) =
    phrase(parser)(input(str))

// Parse
parse("true", term)
parse("\\x:Nat+Bool*Nat->Nat*Nat+Nat. x", term)
parse("\\x:Nat+Bool*Nat->Nat*Nat+Nat. x; \\x:Nat+Bool*Nat->Nat*Nat+Nat. x", term)
parse("true 0; false", term)
parse("true (0; false)", term)
// parse("true;", term) // Should this work?
parse("true;0 0", term)


// Reduce
def store = fos.Store.empty
// parse("let x: Bool=true in let y: Bool = x in x",term)
parse("\\x:(Ref Nat). x", term)
parse("let x: Ref Nat = ref 0 in x",term)
parse("x:=2;y:=true;z:=4",term)
parse("x y;y z",term)
reduce(parse("if true then false else true", term).get, store)
parse("let x: Ref Nat=ref 0 in let y: Ref Nat = x in x:=(succ (!x));(!y)",term)
parse("let x: Nat = (x := 0) in (y := 1); let x: Nat = (x := 0) in (y := 1)",term)
parse("let x: Ref Nat=ref 0 in let y: Ref Nat = x in (x:=(succ (!x));(!y))",term)
parse("ref 0 := 0", term)
// reduce(parse("ref 0 := 0", term).get, store)
// parse("ref 0; x := ref 0")
reduce(parse("ref 0", term).get, store)
parse("let x: Ref Nat=ref 0 in let y: Ref Nat = x in (x:=(succ (!x));(!y))",term)
reduce(parse("let x: Ref Nat=ref 0 in let y: Ref Nat = x in (x:=(succ (!x));(!y))",term).get, store)
reduce(parse("let x: Ref Nat=ref 0 in let y: Ref Nat = x in (x:=(succ (!x));(!y))",term).get, store)
parse("let x : Ref Nat = ref 0 in y := true", term)
parse("let x: Ref Nat=ref 0 in let y: Ref Nat = ref 1 in (x:=(succ (!x));(y:=succ (!y)); if (false) then iszero(!y) else iszero(pred(!x)))",term)
// (\x:Ref Nat. (\y:Nat. iszero !y) !x) ref 0
// reduce(parse("\\x:Nat+Bool*Nat->Nat*Nat+Nat. x", term).get, store)
// store.addOrReplace()
// reduce("App(Abs(y,TypeRef(Nat),Sequence(Assign(Loc(fos.Location@3785837c),(succ Deref(Loc(fos.Location@3785837c)))),Deref(y))),Loc(fos.Location@3785837c))", st)

// Sequence(Assign(((\x: TypeRef(Nat). ((\y: TypeRef(Nat). x) x)) Ref(0)),(succ Deref(x))),Deref(y))
// Abs(y,TypeRef(Nat),Sequence(Assign(x,(succ Deref(x))),Deref(y)))
// App(Abs(x,TypeRef(Nat),App(Abs(y,TypeRef(Nat),Sequence(Assign(x,(succ Deref(x))),Deref(y))),x)),Ref(0))
// App(Abs(x,TypeRef(Nat),App(Abs(y,TypeRef(Nat),Sequence(Assign(x,(succ Deref(x))),Deref(y))),x)),Loc(fos.Location@7d85eed))
// App(Abs(y,TypeRef(Nat),Sequence(Assign(Loc(fos.Location@3785837c),(succ Deref(Loc(fos.Location@3785837c)))),Deref(y))),Loc(fos.Location@3785837c))