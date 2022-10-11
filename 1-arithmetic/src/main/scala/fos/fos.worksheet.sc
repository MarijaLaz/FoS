import fos.Arithmetic._
def input(str: String) = new lexical.Scanner(str)
def parse[T](str: String, parser: Parser[T]) =
    phrase(parser)(input(str))

parse("true", term)
parse("false", term)
parse("pred pred 3", term)
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
// eval(parse("succ true", term).get)
// eval(parse("succ succ succ false", term).get)
eval(parse("iszero succ 0", term).get)
eval(parse("iszero succ pred pred 0", term).get)
// eval(parse("iszero succ true", term).get)
eval(parse("if iszero 0 then succ succ succ 0 else false", term).get)
eval(parse("if iszero succ 0 then succ succ succ 0 else pred 0", term).get)
// eval(parse("if 0 then succ succ succ 0 else pred 0", term).get)
eval(parse("pred succ succ pred pred 0", term).get)
// eval(parse("pred False", term).get)
eval(parse("pred pred 2", term).get)
eval(parse("pred pred succ 2", term).get)
eval(parse("succ succ pred pred pred pred 2", term).get)
// eval(parse("pred succ succ succ false", term).get)
eval(parse("pred succ succ pred succ succ succ pred pred 0", term).get)
eval(parse("if iszero pred pred 2 then if iszero 0 then true else false else false", term).get)

