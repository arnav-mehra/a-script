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
        | number
        | variable

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