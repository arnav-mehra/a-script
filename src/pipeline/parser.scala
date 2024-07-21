package pipeline.parser

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import types.util.*
import types.data.*

object ProgramParser extends JavaTokenParsers {
    def parse(code: String): (Node.Fn, Node.Call) = {
        val t: Nodes = parseAll(program, code).get
        (
            Node.Fn("_", ArrayBuffer(), t),
            Node.Call("_", ArrayBuffer())
        )
    }

    def program:Parser[Nodes] = rep(node) ^^ (
        ls => ls.to(ArrayBuffer)
    )

    def node:Parser[Node] = line | block

    // BLOCK

    def block:Parser[Node] = 
        function_block
        | while_block
        | if_block
        | for_block
        | match_block

    def function_block:Parser[Node] = "fn~" ~ variable ~ param_list ~ block_core ^^ {
        case "fn~"~v~ps~b => Node.Fn(v, ps, b)
    }

    def param_list: Parser[ArrayBuffer[String]] = 
        "()" ^^ {s => ArrayBuffer()}
        | "(" ~ variable ~ rep("," ~ variable) ~ ")" ^^ {
            case "("~v~ls~")" => {
                ArrayBuffer(v) ++ ls.map(s => s._2).to(ArrayBuffer)
            }
            case default => println("wtf"); ArrayBuffer()
        }

    def while_block:Parser[Node] = "while~" ~ statement ~ block_core ^^ {
        case "while~"~c~bt => Node.While(c, bt)
        case default => print("wtf"); Node.Const(Data.Number(0))
    }

    def if_block:Parser[Node] = "if~" ~ statement ~ block_core ^^ {
        case "if~"~c~bt => Node.If(c, bt)
        case default => print("wtf"); Node.Const(Data.Number(0))
    }

    def match_block:Parser[Node] = "match~" ~ statement ~ "{" ~ rep(match_case) ~ "}" ^^ {
        case "match~"~s~"{"~ls~"}" => Node.Match(s, ls.to(ArrayBuffer))
        case default => print("wtf"); Node.Const(Data.Number(0))
    }

    def match_case:Parser[(Node, Nodes)] = statement ~ "=>" ~ block_core ^^ (
        s => (s._1._1, s._2)
    )

    def for_block:Parser[Node] = "for~" ~ (for_in_block | for_to_block) ^^ (
        s => s._2
    )

    def for_in_block:Parser[Node] = variable ~ ":" ~ statement ~ block_core ^^ {
        case v~":"~arr~bt => Node.ForIn(v, arr, bt)
        case default => print("wtf"); Node.Const(Data.Number(0))
    }

    def for_to_block:Parser[Node] = variable ~ ":" ~ statement ~ "~to~" ~ statement ~ block_core ^^ {
        case v~":"~s~"~to~"~e~bt => Node.ForTo(v, s, e, bt)
        case default => print("wtf"); Node.Const(Data.Number(0))
    }

    def block_core:Parser[Nodes] = "{" ~ program ~ "}" ^^ {
        case "{"~bt~"}" => bt
    }

    // LINE/STATEMENT

    def line:Parser[Node] = statement ~ ";" ^^ (s => s._1)

    def statement:Parser[Node] = setter

    def setter:Parser[Node] = rep(accessor ~ ("="|"+="|"-="|"*="|"/=")) ~ getter ^^ {
        case lst~e1 => lst.foldRight(e1)((v, acc) => Node.Set(v._1, v._2, acc))
    }

    def nestBinOps(e1: Node, lst: List[String~Node]): Node = {
        lst.foldLeft(e1)((acc, t) => Node.BinOp(acc, t._1, t._2))
    }

    def getter:Parser[Node] = expression ~ rep(("=="|"!="|"<="|">="|"<"|">") ~ expression) ^^ {
        case e1~lst => nestBinOps(e1, lst)
    }

    def expression:Parser[Node] = term ~ rep(("+"|"-") ~ term) ^^ {
        case e1~lst => nestBinOps(e1, lst)
    }

    def term:Parser[Node] = printable_factor ~ rep(("*"|"/") ~ printable_factor) ^^ {
        case e1~lst => nestBinOps(e1, lst)
    }

    def printable_factor:Parser[Node] = factor ~ opt("!") ^^ {
        case f~Some(x) => Node.Print(f)
        case f~None => f
    }

    def factor:Parser[Node] =
        "(" ~ setter ~ ")" ^^ (s => s._1._2)
        | literal          ^^ (x => Node.Const(x))
        | caller
        | accessor

    def accessor:Parser[Node.Get] = variable ~ rep(field) ^^ {
        case v~ls => Node.Get(v, ls.to(ArrayBuffer))
    }

    def variable: Parser[String] = "[a-zA-Z_][a-zA-Z_0-9]*".r

    def field:Parser[Node] = "[" ~ getter ~ "]" ^^ (s => s._1._2)

    def caller: Parser[Node.Call] = variable ~ "(" ~ arg_list ~ ")" ^^ {
        case v~"("~bt~")" => Node.Call(v, bt)
    }

    def arg_list =
        "" ^^ (_ => ArrayBuffer())
        getter ~ rep("," ~ getter) ^^ {
            case ent~ent_ls => ArrayBuffer(ent) ++ ent_ls.map(s => s._2).to(ArrayBuffer)
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

    def array: Parser[Data] = "[" ~ array_ent_ls ~ "]" ^^ {
        case "["~ent_ls~"]" => Data.Array(ent_ls)
    }

    def array_ent_ls: Parser[ArrayBuffer[Data]] = opt(literal ~ rep("," ~ literal)) ^^ {
        case None => ArrayBuffer()
        case Some(ent~ent_ls) => ArrayBuffer(ent) ++ ent_ls.map(s => s._2).to(ArrayBuffer)
    }

    def obj: Parser[Data] = "#" ~ obj_ent_ls ~ "#" ^^ {
        case "#"~ent_ls~"#" => Data.Object(ent_ls.to(HashMap))
    }

    def obj_ent_ls: Parser[ArrayBuffer[(Data, Data)]] = opt(obj_ent ~ rep("," ~ obj_ent)) ^^ {
        case None => ArrayBuffer()
        case Some(ent~ent_ls) => ArrayBuffer(ent) ++ ent_ls.map(s => s._2).to(ArrayBuffer)
    }

    def obj_ent: Parser[(Data, Data)] = literal ~ ":" ~ literal ^^ {
        case l1~":"~l2 => (l1, l2)
        case default => {
            throw new java.lang.Exception("Error parsing object entry (this shouldn't happen)");
            (Data.Number(0), Data.Number(0))
        }
    }
}