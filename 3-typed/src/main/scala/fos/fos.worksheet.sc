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