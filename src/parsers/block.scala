package parsers.block

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import types.util.*
import parsers.line.LineParser
import runner.Runner.{functions, variables}

object ProgramParser extends JavaTokenParsers {
    def keywords = Array("if", "while", "for", "to", "in", "fn")
    def kw_regexes = keywords.map(s => (
        "\\s" + s + "\\s", "~" + s + "~"
    ))

    def parse(code: String): BlockTree = {
        // remove comments
        var min_code = code.split("\n")
                           .filter(s => s.length() < 2 || s.subSequence(0, 2) != "//")
                           .mkString("\n")
        // clarify keywords
        kw_regexes.foreach(p => min_code = min_code.replaceAll(p._1, p._2))
        // remove whitespace
        min_code = min_code.replaceAll("\\s", "")

        println(min_code)
        // run
        parseAll(program, min_code).get
    }

    def program:Parser[BlockTree] = rep(node) ^^ (
        lst => lst.to(ArrayBuffer)
    )

    def node = line | block

    // BLOCKS

    def block:Parser[BlockNode] = 
        function_block
        | while_block
        | if_block
        | for_block

    def function_block:Parser[BlockNode] = "~fn~" ~ variable ~ block_core ^^ {
        case "~fn~"~v~b => BlockNode.Fn(v, b)
        case default => println("wtf"); BlockNode.Null()
    }

    def while_block:Parser[BlockNode] = "~while~" ~ statement ~ block_core ^^ {
        case "~while~"~s~b => BlockNode.While(s, b)
        case default => println("wtf"); BlockNode.Null()
    }

    def if_block:Parser[BlockNode] = "~if~" ~ statement ~ block_core ^^ {
        case "~if~"~s~bt => BlockNode.If(s, bt)
        case default => println("wtf"); BlockNode.Null()
    }

    def for_block:Parser[BlockNode] =
        "~for~" ~ (for_in_block | for_to_block)
    ^^ (s => s._2)

    def for_in_block:Parser[BlockNode] =
        variable ~ ":" ~ statement ~ block_core
    ^^ {
        case v~":"~arr~bt => BlockNode.ForIn(v, arr, bt)
        case default => println("wtf"); BlockNode.Null()
    }

    def for_to_block:Parser[BlockNode] =
        variable ~ ":" ~ statement ~ "~to~" ~ statement ~ block_core
    ^^ {
        case v~":"~s~"~to~"~e~bt => BlockNode.ForTo(v, s, e, bt)
        case default => println("wtf"); BlockNode.Null()
    }

    def block_core:Parser[BlockTree] = "{" ~ program ~ "}" ^^ {
        case "{"~bt~"}" => bt
        case default    => ArrayBuffer()
    }

    // PRIMS

    def line:Parser[AST] = statement ~ ";" ^^ (s => s._1)

    def statement:Parser[AST] = "[^;{}$~]+".r ^^ (s => {
        LineParser.parse(s)
    })

    def variable:Parser[String] = "[a-zA-Z_][a-zA-Z_0-9]*".r ^^ (s => {
        variables(s) = Data.Number(0); s
    })
}