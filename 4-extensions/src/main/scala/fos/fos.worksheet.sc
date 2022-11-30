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
// var store = new Store()

// reduce(parse("\\x:Nat+Bool*Nat->Nat*Nat+Nat. x", term).get, store)
