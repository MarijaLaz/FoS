import fos.Arithmetic._
def input(str: String) = new lexical.Scanner(str)
def parse[T](str: String, parser: Parser[T]) =
    phrase(parser)(input(str))

parse("true", term)
parse("false", term)
parse("3", term)
parse("0", term)
parse("iszero zero", term) 
parse("iszero 0", term)
parse("if true then true else false", term)


reduce(parse("if true then true else false", term).get)
reduce(parse("iszero succ 2", term).get)
reduce(parse("pred succ 0", term).get)
reduce(parse("if if true then false else true then true else false", term).get)
reduce(parse("iszero pred succ 2", term).get)
reduce(parse("iszero pred succ 0", term).get)
reduce(parse("iszero 0", term).get)
// reduce(parse("true", term).get)
// reduce(Zero) TOASK why not working

eval(parse("succ succ 0", term).get)



