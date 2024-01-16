package parsers.block

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import types.util.*
import types.data.*
import parsers.line.LineParser

object ProgramParser extends JavaTokenParsers {
    def parse(code: String): Node = {
        val t: Tree = parseAll(program, code).get
        Node.Fn("_", ArrayBuffer(), t)
    }

    def program:Parser[Tree] = rep(node) ^^ (
        ls => ls.to(ArrayBuffer)
    )

    def node:Parser[Node] = line | block

    // BLOCK

    def block:Parser[Node] = 
        function_block
        | while_block
        | if_block
        | for_block

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

    def block_core:Parser[Tree] = "{" ~ program ~ "}" ^^ {
        case "{"~bt~"}" => bt
    }

    // PRIMS

    def line:Parser[Node] = statement ~ ";" ^^ (
        s => s._1
    )

    def statement:Parser[Node] = "[^;{}$~]+".r ^^ {
        s => LineParser.parse(s)
    }

    def variable:Parser[String] = "[a-zA-Z_][a-zA-Z_0-9]*".r
}