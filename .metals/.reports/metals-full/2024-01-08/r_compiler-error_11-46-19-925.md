file:///C:/Users/Arnav/Documents/GitHub/apl/apl/index.scala
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

action parameters:
offset: 1162
uri: file:///C:/Users/Arnav/Documents/GitHub/apl/apl/index.scala
text:
```scala
package utility

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.HashMap

object ParserFormula extends JavaTokenParsers {
    var variables = HashMap("x"->1, "y"->2);

    def line:Parser[Double] = setter | form

    def setter:Parser[Double] = variable ~ "=" ~ form ^^ {
        case v~"="~f => {
            v = f
        }
    }

    def form:Parser[Double] = term ~ rep(("+" | "-") ~ term) ^^ {
        case t1~lst => lst.foldLeft(t1)(
            (acc, t) => if (t._1 == "+") acc + t._2 else acc - t._2
        )
    }
    def term:Parser[Double] = factor ~ rep(("*" | "/") ~ factor) ^^ {
        case f1~lst => {
            lst.foldLeft(f1)(
                (acc, f) => if (f._1 == "*") acc * f._2 else acc / f._2
            )
        }
    }
    def factor:Parser[Double] =
        "(" ~ form ~ ")" ^^ (x => x._1._2)
 number
 variable

    // def array: Parser[List[Double]] = "[" ~ number ~ rep("," ~ number) ~ "]" ^^ {
    //     case "["~n~lst~"]" => {
    //         List(n) ::: lst.map(p => p._2)
    //     }
    // }
    def variable: Parser[Reference[@@Double]   = """[a-z]+""".r ^^ {
        v => variables(v)
    }
    def number: Parser[Double] = """(0|[1-9]\d*)""".r ^^ {
        s => s.toDouble
    }


    def main(args: Array[String]) = {
        println(parseAll(form, "y+x").get)
        
        // 2+3*5 
        // => term(2) [+ term(3*5)]
        // => (factor(2), []) [+ (factor(3), [* factor(5)])]
        // => (float(2), []) [+ (float(3), [* float(5)])]
    }
}
```



#### Error stacktrace:

```
dotty.tools.dotc.core.SymDenotations$NoDenotation$.owner(SymDenotations.scala:2582)
	scala.meta.internal.pc.SignatureHelpProvider$.isValid(SignatureHelpProvider.scala:83)
	scala.meta.internal.pc.SignatureHelpProvider$.notCurrentApply(SignatureHelpProvider.scala:94)
	scala.meta.internal.pc.SignatureHelpProvider$.$anonfun$1(SignatureHelpProvider.scala:48)
	scala.collection.StrictOptimizedLinearSeqOps.loop$3(LinearSeq.scala:280)
	scala.collection.StrictOptimizedLinearSeqOps.dropWhile(LinearSeq.scala:282)
	scala.collection.StrictOptimizedLinearSeqOps.dropWhile$(LinearSeq.scala:278)
	scala.collection.immutable.List.dropWhile(List.scala:79)
	scala.meta.internal.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:48)
	scala.meta.internal.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:388)
```
#### Short summary: 

java.lang.AssertionError: NoDenotation.owner