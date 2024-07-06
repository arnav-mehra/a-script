package parsers.line

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import types.util.*
import types.data.*
import parsers.block.*

object LineParser extends JavaTokenParsers {
    def parse(ln: String): Node = {
        parseAll(line, ln).get
    }

    def line:Parser[Node] = printer | setter | getter

    // PRINTER

    def printer:Parser[Node] = getter ~ "!" ^^ (
        s => Node.Print(s._1)
    )

    // SETTER / MUTATOR

    def setter:Parser[Node] = 
        variable ~ rep(field) ~ ("="|"+="|"-="|"*="|"/=") ~ getter
    ^^ {
        case v~ls~op~e => Node.Set(v, ls.to(ArrayBuffer), op, e)
    }

    // GETTER / EVALUATOR

    def nestBinOps(e1: Node, lst: List[String~Node]): Node = {
        lst.foldLeft(e1)((acc, t) => Node.BinOp(acc, t._1, t._2))
    }

    def getter:Parser[Node] = expression ~ rep(("=="|"!="|"<="|">="|"<"|">") ~ expression) ^^ {
        case e1~lst => nestBinOps(e1, lst)
    }

    def expression:Parser[Node] = term ~ rep(("+"|"-") ~ term) ^^ {
        case e1~lst => nestBinOps(e1, lst)
    }

    def term:Parser[Node] = factor ~ rep(("*"|"/") ~ factor) ^^ {
        case e1~lst => nestBinOps(e1, lst)
    }

    def factor:Parser[Node] =
        "(" ~ getter ~ ")" ^^ (s => s._1._2)
        | literal          ^^ (x => Node.Const(x))
        | caller
        | accessor

    def accessor:Parser[Node] = variable ~ rep(field) ^^ {
        case v~ls => {
            ls.length match {
                case 0 => Node.Var(v)
                case _ => Node.Get(v, ls.to(ArrayBuffer))
            }
        }
    }

    def field:Parser[Node] = "[" ~ getter ~ "]" ^^ (s => s._1._2)

    def caller: Parser[Node] = variable ~ "(" ~ arg_list ~ ")" ^^ {
        case v~"("~bt~")" => Node.Call(v, bt)
    }

    def arg_list =
        "" ^^ (_ => ArrayBuffer())
        getter ~ rep("," ~ getter) ^^ {
            case ent~ent_ls => ArrayBuffer(ent) ++ ent_ls.map(s => s._2).to(ArrayBuffer)
        }

    def variable: Parser[String] = "[a-zA-Z_][a-zA-Z_0-9]*".r

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

    def array: Parser[Data] = "[" ~ array_ent_ls ~ "]" ^^ {
        case "["~ent_ls~"]" => Data.Array(ent_ls)
    }

    def array_ent_ls: Parser[ArrayBuffer[Data]] =
        "" ^^ (_ => ArrayBuffer())
        | literal ~ rep("," ~ literal) ^^ {
            case ent~ent_ls => ArrayBuffer(ent) ++ ent_ls.map(s => s._2).to(ArrayBuffer)
        }

    def obj: Parser[Data] = "#" ~ obj_ent_ls ~ "#" ^^ {
        case "#"~ent_ls~"#" => Data.Object(ent_ls.to(HashMap))
    }

    def obj_ent_ls: Parser[ArrayBuffer[(Data, Data)]] = 
        "" ^^ (_ => ArrayBuffer())
        | obj_ent ~ rep("," ~ obj_ent) ^^ {
            case ent~ent_ls => ArrayBuffer(ent) ++ ent_ls.map(s => s._2).to(ArrayBuffer)
        }

    def obj_ent: Parser[(Data, Data)] = literal ~ ":" ~ literal ^^ {
        case l1~":"~l2 => (l1, l2)
        case default => {
            throw new java.lang.Exception("Error parsing object entry (this shouldn't happen)");
            (Data.Number(0), Data.Number(0))
        }
    }
}