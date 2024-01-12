package runner

import scala.collection.mutable.HashMap

import types.util.*
import parsers.block.*
import parsers.line.*
import scala.collection.mutable.ArrayBuffer

object Runner {
    val functions: HashMap[String, BlockTree] = HashMap();
    val variables : HashMap[String, Data] = HashMap();

    def run_iter(tree: BlockTree): Unit = {
        for node <- tree do {
            node match {
                case bn: BlockNode => run_block(bn)
                case ln: String => {
                    print(ln + "; ")
                    val res = LineParser.parse(ln)
                    println(res)
                }
            }
        }
    }

    def run_block(bn: BlockNode): Unit = {
        bn match {
            case BlockNode.If(c, bt) => {
                // println("cond: " + c)
                if (LineParser.parse(c) != Data.Number(0)) {
                    run_iter(bt)
                }
            }
            case BlockNode.While(c, bt) => {
                while (LineParser.parse(c) != Data.Number(0)) {
                    run_iter(bt)
                }
            }
            case BlockNode.ForIn(v, sarr, bt) => {
                val varr: Data = LineParser.parse(sarr)
                varr match {
                    case Data.Array(arr) => {
                        for (x <- arr) {
                            variables(v) = x
                            run_iter(bt)
                        }
                    }
                    case default => println("wtf")
                }
            }
            case BlockNode.ForTo(v, s, e, bt) => {
                val st: Data = LineParser.parse(s)
                val ed: Data = LineParser.parse(e)
                (st, ed) match {
                    case (Data.Number(si), Data.Number(ei)) => {
                        for (i <- si.toInt to ei.toInt) {
                            variables(v) = Data.Number(i)
                            run_iter(bt)
                        }
                    }
                    case default => println("wtf")
                }
            }
            case BlockNode.Fn(v, bt) => functions(v) = bt
            case BlockNode.Null() => println("wtf")
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
