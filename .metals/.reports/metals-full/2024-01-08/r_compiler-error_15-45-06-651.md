file:///C:/Users/Arnav/Documents/GitHub/apl/apl/index.scala
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

action parameters:
offset: 521
uri: file:///C:/Users/Arnav/Documents/GitHub/apl/apl/index.scala
text:
```scala
package utility

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.HashMap
import scala.runtime.RichDouble

class DoubleObj(private var _v : Double) {
    def v = _v
    def v_=(v: Double) = {
        _v = v
    }
}

object ControlFlowParser extends JavaTokenParsers {
    def if_block:Parser[List[String]] = "if(" ~ block ")"

    def block:Parser[List[String]] = "{" ~ line ~ rep(";" ~ line) ~ "}" ^^ {
        case "{"~l~lst~"}" => l
    }

    def line:Parser[]@@
}

object ParserFormula extends JavaTokenParsers {
    val variables : HashMap[String, DoubleObj] = HashMap();

    

    def line:Parser[DoubleObj] = setter | form

    def setter:Parser[DoubleObj] = variable ~ ("="|"+="|"-="|"*="|"/=") ~ form ^^ {
        case x~"="~f =>  x.v = f.v;  x
        case x~"+="~f => x.v += f.v; x
        case x~"-="~f => x.v -= f.v; x
        case x~"*="~f => x.v *= f.v; x
        case x~"/="~f => x.v /= f.v; x
    }

    def form:Parser[DoubleObj] = term ~ rep(("+" | "-") ~ term) ^^ {
        case t1~lst => {
            new DoubleObj(
                lst.foldLeft(t1.v)(
                    (acc, t) => if (t._1 == "+") acc + t._2.v else acc - t._2.v
                )
            )
        }
    }
    def term:Parser[DoubleObj] = factor ~ rep(("*" | "/") ~ factor) ^^ {
        case f1~lst => {
            new DoubleObj(
                lst.foldLeft(f1.v)(
                    (acc, f) => if (f._1 == "*") acc * f._2.v else acc / f._2.v
                )
            )
        }
    }
    def factor:Parser[DoubleObj] =
        "(" ~ form ~ ")" ^^ (x => x._1._2)
 number
 variable

    // def array: Parser[List[Double]] = "[" ~ number ~ rep("," ~ number) ~ "]" ^^ {
    //     case "["~n~lst~"]" => {
    //         List(n) ::: lst.map(p => p._2)
    //     }
    // }
    def variable: Parser[DoubleObj]   = """[a-z]+""".r ^^ {
        s => {
            if (!variables.contains(s)) {
                variables(s) = new DoubleObj(0)   
            }
            variables(s)
        }
    }
    def number: Parser[DoubleObj] = """(0|[1-9]\d*)""".r ^^ {
        s => new DoubleObj(s.toDouble)
    }

    def run(code: String) = {
        code.replaceAll("\\s", "")
            .split(";")
            .filter(x => x.length() > 0)
            .foreach(l => {
                println(l)
                val res = parseAll(line, l)
                println(res.get.v)
            })
    }

    def main(args: Array[String]) = {
        val code : String = """
            x = 1;
            y = 1;
            x *= 2;
            x *= 2;
            x;
        """

        run(code)
        
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