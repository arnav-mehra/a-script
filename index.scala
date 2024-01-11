package index

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import util.{Data, BlockType, BlockNode, BlockTree}
import line.LineParser

object ProgramParser extends JavaTokenParsers {
    val functions: HashMap[String, BlockTree] = HashMap();

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

    // RUNNERS

    def run_iter(tree: BlockTree): Unit = {
        for node <- tree do {
            node match {
                case node: BlockNode => {
                    node.bt match {
                        case BlockType.If => {
                            println("cond: " + node.cond)
                            if (LineParser.parseAll(LineParser.line, node.cond).get != Data.Number(0)) {
                                run_iter(node.ls)
                            }
                        }
                        case BlockType.While => {
                            while (LineParser.parseAll(LineParser.line, node.cond).get != Data.Number(0)) {
                                run_iter(node.ls)
                            }
                        }
                        case BlockType.Fn   => functions(node.cond) = node.ls
                        case BlockType.Null => println("wtf")
                    }
                }
                case ln: String => {
                    print("line: " + ln + ". ")
                    val res = LineParser.parseAll(LineParser.line, ln).get
                    println(res)
                }
            }
        }
    }

    def run_function(fn_name: String): Data = {
        val tree: BlockTree = functions(fn_name)
        val (exec_tree, last_ln) = tree.splitAt(tree.length - 1)
        run_iter(exec_tree)

        last_ln(0) match {
            case ln: String => LineParser.parseAll(LineParser.line, ln).get
            case default    => println("wtf. missing return type"); Data.Number(0)
        }
    }

    def run(code: String) = {
        val min_code = code.replaceAll("\\s", "")
                           .replaceAll("if", "if ")
                           .replaceAll("while", "while ")
                           .replaceAll("fn", "fn ")
        val res = parseAll(program, min_code)
        run_iter(res.get)
    }

    def main(args: Array[String]) = {
        // val code : String = """
        //     x = 10;
        //     while x {
        //         x -= 1;
        //         if x == 5 {
        //             x -= 1;
        //         }
        //     }
        // """

        val code : String = """
            x = [];
            x += 5;
        """
        run(code)
    }
}