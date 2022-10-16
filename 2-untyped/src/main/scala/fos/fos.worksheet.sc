import fos.Untyped._
def input(str: String) = new lexical.Scanner(str)
def parse[T](str: String, parser: Parser[T]) =
    phrase(parser)(input(str))


parse("x", term)
parse("x y", term)
parse("(y)", term)
parse("\\x. x", term)
parse("\\x. \\y. x y x", term)
parse("\\x. \\y. x y ", term)
// parse("\\x.(\\y.x y)", term)
