package runner

import scala.collection.mutable.HashMap

import types.util.*
import parsers.block.*
import parsers.line.*

object Runner {
    val functions: HashMap[String, BlockTree] = HashMap();
    val variables : HashMap[String, Data] = HashMap();

    def run_iter(tree: BlockTree): Unit = {
        for node <- tree do {
            node match {
                case node: BlockNode => {
                    node.bt match {
                        case BlockType.If => {
                            println("cond: " + node.cond)
                            if (LineParser.parse(node.cond) != Data.Number(0)) {
                                run_iter(node.ls)
                            }
                        }
                        case BlockType.While => {
                            while (LineParser.parse(node.cond) != Data.Number(0)) {
                                run_iter(node.ls)
                            }
                        }
                        case BlockType.Fn   => functions(node.cond) = node.ls
                        case BlockType.Null => println("wtf")
                    }
                }
                case ln: String => {
                    print("line: " + ln + ". ")
                    val res = LineParser.parse(ln)
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
            case ln: String => LineParser.parse(ln)
            case default    => println("wtf. missing return type"); Data.Number(0)
        }
    }

    def run(code: String) = {
        val res: BlockTree = ProgramParser.parse(code)
        run_iter(res)
    }
}
