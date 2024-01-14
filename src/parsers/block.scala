package parsers.block

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import types.util.*
import parsers.line.LineParser
import runner.Runner.{functions, var_to_idx, vars}

object ProgramParser extends JavaTokenParsers {
    def parse(code: String): AST = parseAll(program, code).get

    def program:Parser[AST] = rep(node) ^^ (raw_lst => {
        val lst: List[AST] = raw_lst.collect { case Some(a) => a }
        val ls = lst.to(ArrayBuffer)

        (() => {
            ls.foreach(ast => ast())
            Data.Number(0)
        })
    })

    def node:Parser[Option[AST]] = line ^^ (Some.apply) | block

    // BLOCKS

    def block:Parser[Option[AST]] = 
        function_block ^^ (_ => None)
        | while_block  ^^ (Some.apply)
        | if_block     ^^ (Some.apply)
        | for_block    ^^ (Some.apply)

    def function_block:Parser[Unit] = "~fn~" ~ variable ~ block_core ^^ {
        case "~fn~"~v~b => functions(v) = b;
    }

    def while_block:Parser[AST] = "~while~" ~ statement ~ block_core ^^ {
        case "~while~"~c~bt => (() => {
            while (c()._is_truthy) bt()
            Data.Number(0)
        })
    }

    def if_block:Parser[AST] = "~if~" ~ statement ~ block_core ^^ {
        case "~if~"~c~bt => (() => {
            if (c()._is_truthy) bt()
            Data.Number(0)
        })
    }

    def for_block:Parser[AST] =
        "~for~" ~ (for_in_block | for_to_block)
    ^^ (s => s._2)

    def for_in_block:Parser[AST] =
        variable ~ ":" ~ statement ~ block_core
    ^^ {
        case v~":"~sarr~bt => (() => {
            val varr: Data = sarr()
            varr match {
                case Data.Array(arr) => {
                    for (x <- arr) {
                        vars(v) = x
                        bt()
                    }
                }
                case default => println("wtf")
            }
            varr
        })
    }

    def for_to_block:Parser[AST] =
        variable ~ ":" ~ statement ~ "~to~" ~ statement ~ block_core
    ^^ {
        case v~":"~s~"~to~"~e~bt => (() => {
            val st: Data = s()
            val ed: Data = e()
            (st, ed) match {
                case (Data.Number(si), Data.Number(ei)) => {
                    for (i <- si.toInt to ei.toInt) {
                        vars(v) = Data.Number(i)
                        bt()
                    }
                }
                case default => println("wtf")
            }
            Data.Number(0)
        })
    }

    def block_core:Parser[AST] = "{" ~ program ~ "}" ^^ {
        case "{"~bt~"}" => bt
    }

    // PRIMS

    def line:Parser[AST] = statement ~ ";" ^^ (s => s._1)

    def statement:Parser[AST] = "[^;{}$~]+".r ^^ (LineParser.parse)

    def variable:Parser[Int] = "[a-zA-Z_][a-zA-Z_0-9]*".r ^^ (s => {
        if (!var_to_idx.contains(s)) {
            val i: Int = var_to_idx.size
            val d: Data = Data.Number(0)
            var_to_idx(s) = i
            vars.append(d)
        }
        var_to_idx(s)
    })
}