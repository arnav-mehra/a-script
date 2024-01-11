package parsers.line

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import types.util.*
import parsers.block.*
import runner.Runner.{functions, variables, run_function}

object LineParser extends JavaTokenParsers {
    def parse(ln: String): Data = {
        parseAll(line, ln).get
    }

    def line:Parser[Data] = setter | getter

    // SETTER / MUTATOR

    def setter:Parser[Data] = variable ~ ("="|"+="|"-="|"*="|"/=") ~ getter ^^ {
        case v~s~op2 => {
            // println("setter")
            if (!variables.contains(v)) {
                variables(v) = Data.Number(0)
            }
            def op1: Data = variables(v)

            (op1) match {
                case Data.Number(v1) => { // immutable structures 
                    s match {
                        case "="  => variables(v) = op2
                        case "+=" => variables(v) = op1 + op2
                        case "-=" => variables(v) = op1 - op2
                        case "*=" => variables(v) = op1 * op2
                        case "/=" => variables(v) = op1 / op2
                    }
                }
                case default => { // mutable structures
                    s match {
                        case "="  => variables(v) = op2
                        case "+=" => op1 += op2
                        case default => println("wtf")
                    }
                }
            }

            op1
        }
    }

    // GETTER / EVALUATOR

    def getter:Parser[Data] = expression ~ rep(("=="|"!=") ~ expression) ^^ {
        case e1~lst => {
            // println("getter: comparison")
            lst.foldLeft(e1)(
                (acc, t) => if (t._1 == "==") acc == t._2 else acc != t._2
            )
        }
    }

    def expression:Parser[Data] = term ~ rep(("+"|"-") ~ term) ^^ {
        case t1~lst => {
            // print("expr: "); print(t1); print(", "); println(lst)
            lst.foldLeft(t1)(
                (acc, t) => if (t._1 == "+") acc + t._2 else acc - t._2
            )
        }
    }

    def term:Parser[Data] = factor ~ rep(("*"|"/") ~ factor) ^^ {
        case f1~lst => {
            // println("term")
            lst.foldLeft(f1)(
                (acc, f) => if (f._1 == "*") acc * f._2 else acc / f._2
            )
        }
    }

    def factor:Parser[Data] =
        "(" ~ getter ~ ")" ^^ (x => x._1._2)
        | literal
        | caller
        | variable ^^ (s => {
            if (!variables.contains(s)) {
                println("wtf. invalid variable: " + s)
                variables(s) = Data.Number(0)
            }
            variables(s)
        })

    def caller: Parser[Data] = variable ~ "()" ^^ {
        case v~"()" => {
            run_function(v)
        }
    }

    // DATA TYPES / LITERALS

    def variable: Parser[String] = "[a-z]+".r

    def literal: Parser[Data] = number | array

    def number: Parser[Data] = """(0|[1-9]\d*)""".r ^^ {
        s => Data.Number(s.toDouble)
    }

    def array: Parser[Data] =
        "[]" ^^ {
            _ => Data.Array(ArrayBuffer())
        }
        | "[" ~ literal ~ rep("," ~ literal) ~ "]" ^^ {
            case "["~n~lst~"]" => {
                val arr = ArrayBuffer(n) ++ lst.map(p => p._2)
                Data.Array(arr)
            }
            case default => {
                println("wtf");
                Data.Array(ArrayBuffer())
            }
        }
}