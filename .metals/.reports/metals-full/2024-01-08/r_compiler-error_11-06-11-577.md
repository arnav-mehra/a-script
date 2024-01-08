file:///C:/Users/Arnav/Documents/GitHub/apl/apl/index.scala
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

action parameters:
offset: 156
uri: file:///C:/Users/Arnav/Documents/GitHub/apl/apl/index.scala
text:
```scala
package utility

import scala.util.parsing.combinator.JavaTokenParsers

object ParserFormula extends JavaTokenParsers {
    variables = HashMap[String,@@]

    def walk(a:Any):Unit = a match {
        case s:String => print(s)
        case ~(a,b) =>
            walk(a)
            walk(b)
        case lst:List[Any] =>
            lst foreach walk
    }

    def form:Parser[Double] = term ~ rep(("+" | "-") ~ term) ^^ {
        case t1~lst => lst.foldLeft(t1)(
            (acc, t) => if (t._1 == "+") acc + t._2 else acc - t._2
        )
    }

    def term:Parser[Double] = factor ~ rep(("*" | "/") ~ factor) ^^ {
        case f1~lst => lst.foldLeft(f1)(
            (acc, f) => if (f._1 == "*") acc * f._2 else acc / f._2
        )
    }

    def factor:Parser[Double] =
        "(" ~ form ~ ")" ^^ (x => x._1._2)
 array
 number
 variable

    def array: Parser[List[Double]] = "[" ~ number ~ rep("," ~ number) ~ "]" ^^ {
        case wd ~ fr => WordFreq(wd,fr)
    }

    def variable: Parser[String]   = """[a-z]+""".r       ^^ { _.toString }
    def number: Parser[Double]    = """(0|[1-9]\d*)""".r ^^ { _.toDouble }


    def main(args: Array[String]) = {
        println(parseAll(form, "2+3/8/2").get)
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
	scala.meta.internal.pc.SignatureHelpProvider$.notCurrentApply(SignatureHelpProvider.scala:96)
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