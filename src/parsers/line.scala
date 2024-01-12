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

    def setter:Parser[Data] = var_setter | field_setter

    def var_setter:Parser[Data] = variable ~ ("="|"+="|"-="|"*="|"/=") ~ getter ^^ {
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

    def field_setter:Parser[Data] =
        variable
        ~ "[" ~ getter ~ "]"
        ~ rep("[" ~ getter ~ "]")
        ~ ("="|"+="|"-="|"*="|"/=")
        ~ getter
    ^^ {
        case v~"["~ls_0~"]"~ls_rem~s~op2 => {
            println("field_setter")
            val ls: List[Data] = List(ls_0) ::: ls_rem.map(s => s._1._2)

            var op1 = variables(v)
            for (i <- 0 to ls.length - 2) op1 = op1->ls(i)
            val fin = ls.last

            op1 match {
                case Data.Number(v1) => { // immutable structures 
                    println("wtf")
                }
                case default => { // mutable structures
                    s match {
                        case "="  => op1.set(fin, op2)
                        case "+=" => op1.set(fin, op2)
                        case default => println("wtf")
                    }
                }
            }

            op1
        }
        case default => {
            println("wtf")
            Data.Number(0)
        }
    }

    // GETTER / EVALUATOR

    def getter:Parser[Data] = expression ~ rep(("=="|"!="|"<"|">"|"<="|">=") ~ expression) ^^ {
        case e1~lst => {
            // println("getter: comparison")
            lst.foldLeft(e1)((acc, t) => {
                t._1 match {
                    case "==" => acc == t._2
                    case "!=" => acc != t._2
                    case ">=" => acc >= t._2
                    case ">"  => acc >  t._2
                    case "<=" => acc <= t._2
                    case "<"  => acc <  t._2
                }
            })
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
        | accessor

    def accessor:Parser[Data] = variable ~ rep("[" ~ getter ~ "]") ^^ {
        case v~ls => {
            // print("fielder")
            def x = variables(v)
            ls.foldLeft(x)((acc, f) => acc->(f._1._2))
        }
    }

    def caller: Parser[Data] = variable ~ "()" ^^ {
        case v~"()" => {
            run_function(v)
        }
    }

    // DATA TYPES / LITERALS

    def variable: Parser[String] = "[a-zA-Z_][a-zA-Z_0-9]*".r

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

    def string: Parser[Data] = "\"" ~ "[^\"]*".r ~ "\"" ^^ {
        case "\""~s~"\"" => Data.String(s)
    }

    def obj: Parser[Data] = 
        "##" ^^ (_ => {
            println("doing it")
            Data.Object(HashMap())
        })
        "#" ~ obj_ent_ls ~ obj_ent ~ "#" ^^ {
            case "#"~hm~obj_ent~"#" => {
                hm(obj_ent._1) = obj_ent._2
                Data.Object(hm)
            }
            case default => {
                Data.Object(HashMap())
            }
        }

    def obj_ent_ls: Parser[HashMap[Data, Data]] = rep(obj_ent ~ ",") ^^ {
        ls => {
            val hm: HashMap[Data, Data] = HashMap()
            ls.foreach(p => hm(p._1._1) = p._1._2)
            hm
        }
    }
    
    def obj_ent: Parser[(Data, Data)] = literal ~ ":" ~ literal ^^ {
        case l1~":"~l2 => (l1, l2)
        case default => (Data.Number(0), Data.Number(0))
    }
}