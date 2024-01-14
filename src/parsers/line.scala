package parsers.line

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import types.util.*
import parsers.block.*
import runner.Runner.{functions, vars, var_to_idx}

object LineParser extends JavaTokenParsers {
    def parse(ln: String): AST = parseAll(line, ln).get

    def line:Parser[AST] = printer | setter | getter

    // PRINTER

    def printer:Parser[AST] = getter ~ "!" ^^ {
        case v~"!" => (() => {
            val x = v()
            println(x)
            x
        })
    }

    // SETTER / MUTATOR

    def setter:Parser[AST] = var_setter | field_setter

    def var_setter:Parser[AST] = variable ~ ("="|"+="|"-="|"*="|"/=") ~ getter ^^ {
        case v~s~op2 => {
            s match {
                case "="  => (() => { vars(v) = op2();           vars(v) })
                case "-=" => (() => { vars(v) = vars(v) - op2(); vars(v) })
                case "*=" => (() => { vars(v) = vars(v) * op2(); vars(v) })
                case "/=" => (() => { vars(v) = vars(v) / op2(); vars(v) })
                case "+=" => (() => {
                    vars(v) match {
                        case Data.Number(n) => vars(v) = vars(v) + op2()
                        case default        => vars(v) += op2()
                    }
                    vars(v)
                })
            }
        }
    }

    def field_setter:Parser[AST] =
        variable ~ "[" ~ getter ~ "]" ~ rep("[" ~ getter ~ "]")
        ~ ("="|"+="|"-="|"*="|"/=") ~ getter
    ^^ {
        case v~"["~ls_0~"]"~ls_rem~s~op2 => {
            val ls: List[AST] = List(ls_0) ::: ls_rem.map(s => s._1._2)

            (() => {
                var op1 = vars(v)
                for (i <- 0 to ls.length - 2) op1 = op1->(ls(i)())
                val fin = ls.last()

                op1 match {
                    case Data.Number(v1) => { // immutable structures 
                        println("wtf")
                    }
                    case default => { // mutable structures
                        s match {
                            case "="  => op1.set(fin, op2())
                            case "+=" => op1.set(fin, op2())
                            case default => println("wtf")
                        }
                    }
                }

                op1
            })
        }
    }

    // GETTER / EVALUATOR

    def getter:Parser[AST] = expression ~ rep(("=="|"!="|"<"|">"|"<="|">=") ~ expression) ^^ {
        case e1~lst => {
            lst.foldLeft(e1)((acc, t) => {
                t._1 match {
                    case "==" => () => acc() == t._2()
                    case "!=" => () => acc() != t._2()
                    case ">=" => () => acc() >= t._2()
                    case ">"  => () => acc() >  t._2()
                    case "<=" => () => acc() <= t._2()
                    case "<"  => () => acc() <  t._2()
                }
            })
        }
    }

    def expression:Parser[AST] = term ~ rep(("+"|"-") ~ term) ^^ {
        case t1~lst => {
            lst.foldLeft(t1)((acc, t) => {
                t._1 match {
                    case "+" => (() => acc() + t._2())
                    case "-" => (() => acc() - t._2())
                }
            })
        }
    }

    def term:Parser[AST] = factor ~ rep(("*"|"/") ~ factor) ^^ {
        case f1~lst => {
            lst.foldLeft(f1)((acc, f) => {
                f._1 match {
                    case "*" => (() => acc() * f._2())
                    case "/" => (() => acc() / f._2())
                }
            })
        }
    }

    def factor:Parser[AST] =
        "(" ~ getter ~ ")" ^^ (x => x._1._2)
        | literal          ^^ {s => (() => s)}
        | caller
        | accessor

    def accessor:Parser[AST] = variable ~ rep("[" ~ getter ~ "]") ^^ {
        case v~ls => {
            if (ls.length == 1) (() => vars(v) -> ls(0)._1._2()) // inline for common case: var[f]
            val base = (() => vars(v))
            ls.foldLeft(base)((acc, f) => (() => acc() -> f._1._2()))
        }
    }

    def caller: Parser[AST] = variable ~ "()" ^^ {
        case v~"()" => functions(v)
    }

    def variable: Parser[Int] = "[a-zA-Z_][a-zA-Z_0-9]*".r ^^ {
        s => {
            if (!var_to_idx.contains(s)) {
                val i: Int = var_to_idx.size
                val d: Data = Data.Number(0)
                var_to_idx(s) = i
                vars.append(d)
            }
            var_to_idx(s)
        }
    }

    // LITERALS

    def literal: Parser[Data] = number | array | string | obj

    def number: Parser[Data] = hex | binary | decimal

    def decimal: Parser[Data] = "\\d+\\.?\\d*".r ^^ {
        s => Data.Number(s.toDouble)
    }
    
    def hex: Parser[Data] = "0x[0-9a-fA-F]*".r ^^ {
        s => Data.Number(Integer.parseInt(s.substring(2), 16))
    }

    def binary: Parser[Data] = "0b(0|1)*".r ^^ {
        s => Data.Number(Integer.parseInt(s.substring(2), 2))
    }

    def string: Parser[Data] = "\"" ~ "[^\"]*".r ~ "\"" ^^ {
        case "\""~s~"\"" => Data.String(s)
    }

    def array: Parser[Data] =
        "[]" ^^ (_ => Data.Array(ArrayBuffer()))
        | "[" ~ literal ~ rep("," ~ literal) ~ "]" ^^ {
            case "["~n~lst~"]" => {
                Data.Array(ArrayBuffer(n) ++ lst.map(p => p._2))
            }
        }

    def obj: Parser[Data] = 
        "##" ^^ (_ => Data.Object(HashMap()))
        "#" ~ obj_ent ~ rep("," ~ obj_ent) ~ "#" ^^ {
            case "#"~e1~els~"#" => {
                val hm: HashMap[Data, Data] = HashMap()
                hm(e1._1) = e1._2
                els.foreach(p => hm(p._2._1) = p._2._2)
                Data.Object(hm)
            }
        }
    
    def obj_ent: Parser[(Data, Data)] = literal ~ ":" ~ literal ^^ {
        case l1~":"~l2 => (l1, l2)
    }
}