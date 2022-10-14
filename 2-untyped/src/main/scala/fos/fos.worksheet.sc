import fos.Untyped._
def input(str: String) = new lexical.Scanner(str)
def parse[T](str: String, parser: Parser[T]) =
    phrase(parser)(input(str))


parse("x", term)
parse("y", term)
parse("\\x. x", term)
