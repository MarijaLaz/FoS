import fos.Untyped._
def input(str: String) = new lexical.Scanner(str)
def parse[T](str: String, parser: Parser[T]) =
    phrase(parser)(input(str))


parse("x", term)
parse("((((x))))", term)
parse("x y", term)
parse("x y z", term)
parse("(x y) z", term)
parse("x (y z)", term)
parse("(x (y z))", term)
parse("w (x (y z))", term)
parse("(y)", term)
parse("\\x. x", term)
parse("\\x. y x x", term)
parse("\\x. (y x) x", term)
parse("\\x. y (x x)", term)
parse("\\x. \\y. x y x", term)
parse("\\x. \\y. x y ", term)
// parse("\\x.(\\y.x y)", term)
parse("\\z.(\\x.x) z",  term)
parse("(\\x.x)((\\x.x)(\\z.(\\w.w)z))", term)
parse("\\x. (\\y. x) y", term)
parse("\\y. ((\\x. x) y)", term)

alpha(parse("\\x. (\\y. x) y", term).get.asInstanceOf[fos.Abs])
alpha(parse("(\\y. x)", term).get.asInstanceOf[fos.Abs])

subst(parse(" (\\y. x)", term).get, "x", parse("y", term).get)
subst(parse(" (\\y. x) y", term).get, "x", parse("y", term).get)

parse("(\\x.x)((\\x.x)(\\z.(\\w.w)z))", term)
parse("\\z.(\\w.w)z", term)
// reduceNormalOrder((parse("(\\y.y)x", term).get))
// reduceNormalOrder((parse("\\x.(\\y.y)x", term).get))
// reduceNormalOrder((parse("\\z.(\\w.w)z", term).get))
reduceNormalOrder((parse("(\\x.x)((\\x.x)(\\z.(\\w.w)z))", term).get))
reduceNormalOrder((parse("\\y. ((\\x. x) y)", term).get))
// reduceCallByValue((parse("\\y. ((\\x. x) y)", term).get))
// reduceCallByValue((parse("(\\x. (\\y. x)) y", term).get))

// print(path((parse("(\\x. (\\y. x)) y", term).get), reduceCallByValue))

parse("(\\x.(\\y.x(y)))", term)
