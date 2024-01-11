package parsers.block

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import types.util.*
import parsers.line.LineParser
import runner.Runner.{functions, variables}

object ProgramParser extends JavaTokenParsers {
    def parse(code: String): BlockTree = {
        val min_code = code.replaceAll("\\s", "")
                           .replaceAll("if", "if ")
                           .replaceAll("while", "while ")
                           .replaceAll("fn", "fn ")
        parseAll(program, min_code).get
    }

    def program:Parser[BlockTree] = rep(line | block) ^^ (lst => lst.to(ArrayBuffer))

    // BLOCKS

    def block:Parser[BlockNode] = 
        function_block
        | while_block
        | if_block

    def function_block:Parser[BlockNode] = "fn " ~ variable ~ block_core ^^ {
        case "fn "~v~b => functions(v) = b; BlockNode(BlockType.Fn)
        case default   => println("wtf");   BlockNode()
    }

    def while_block:Parser[BlockNode] = "while " ~ statement ~ block_core ^^ {
        case "while "~s~b => BlockNode(BlockType.While, s, b)
        case default      => println("wtf"); BlockNode()
    }

    def if_block:Parser[BlockNode] = "if " ~ statement ~ block_core ^^ {
        case "if "~s~b => BlockNode(BlockType.If, s, b)
        case default   => println("wtf"); BlockNode()
    }

    def block_core:Parser[BlockTree] = "{" ~ program ~ "}" ^^ {
        case "{"~lst~"}" => lst
        case default     => ArrayBuffer()
    }

    // PRIMS

    def line:Parser[String] = statement ~ ";" ^^ (s => s._1)

    def statement:Parser[String] = "[^;{}]*".r ^^ (s => s)

    def variable:Parser[String] = "[a-z]+".r ^^ (s => s)
}