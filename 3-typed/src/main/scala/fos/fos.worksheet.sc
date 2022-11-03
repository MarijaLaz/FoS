import fos.SimplyTyped._
def input(str: String) = new lexical.Scanner(str)
def parse[T](str: String, parser: Parser[T]) =
    phrase(parser)(input(str))



parse("x", term)
parse("succ 0", term)
parse("iszero 0", term)
parse("4", term)
parse("(x x)", term)
parse("(x (x x))", term)
parse("\\x:Bool. x", term)
parse("\\x:Bool->Bool. x", term)
parse("\\x:Bool->(Bool->Bool). x", term)
parse("(\\x:Nat->Bool. (\\y:Nat.(x y))) (\\x:Nat.(iszero x)) 0", term)
parse("let x:Bool = true in iszero x", term)
parse("{true, false}", term)
parse("snd {true, false}", term)
parse("\\x:Nat*Nat->Bool. x", term)
parse("\\x:Nat*Nat->Bool*Nat. x", term)

reduce(parse("(\\x:Nat->Bool. (\\y:Nat.(x y))) (\\x:Nat.(iszero x)) 0", term).get)