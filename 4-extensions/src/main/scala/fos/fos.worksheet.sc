import fos.SimplyTypedExtended._
def input(str: String) = new lexical.Scanner(str)
def parse[T](str: String, parser: Parser[T]) =
    phrase(parser)(input(str))


parse("true.1", term)
parse("true", term)
parse("\\x:Nat+Bool*Nat->Nat*Nat+Nat. x", term)
parse("(false as True) . 3", term)
parse("((true false) as True) . 3", term)
// t1 t2.l -> (t1 t2).l and not t1 t2.l