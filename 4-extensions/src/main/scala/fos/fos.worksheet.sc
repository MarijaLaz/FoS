import fos.SimplyTypedExtended._
def input(str: String) = new lexical.Scanner(str)
def parse[T](str: String, parser: Parser[T]) =
    phrase(parser)(input(str))


parse("true", term)
parse("\\x:Nat+Bool*Nat->Nat*Nat+Nat. x", term)