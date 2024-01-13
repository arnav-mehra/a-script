package runner

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

import types.util.*
import parsers.block.*
import parsers.line.*

object Runner {
    val functions: HashMap[Int, BlockTree] = HashMap();

    val var_to_idx: HashMap[String, Int] = HashMap();
    val vars: ArrayBuffer[Data] = ArrayBuffer();

    def run_iter(tree: BlockTree): Unit = {
        for node <- tree do {
            node match {
                case bn: BlockNode => run_block(bn)
                case ln: AST => ln()
            }
        }
    }

    def run_block(bn: BlockNode): Unit = {
        bn match {
            case BlockNode.If(c, bt) => {
                if (c()._is_truthy) {
                    run_iter(bt)
                }
            }
            case BlockNode.While(c, bt) => {
                while (c()._is_truthy) {
                    run_iter(bt)
                }
            }
            case BlockNode.ForIn(v, sarr, bt) => {
                val varr: Data = sarr()
                varr match {
                    case Data.Array(arr) => {
                        for (x <- arr) {
                            vars(v) = x
                            run_iter(bt)
                        }
                    }
                    case default => println("wtf")
                }
            }
            case BlockNode.ForTo(v, s, e, bt) => {
                val st: Data = s()
                val ed: Data = e()
                (st, ed) match {
                    case (Data.Number(si), Data.Number(ei)) => {
                        for (i <- si.toInt to ei.toInt) {
                            vars(v) = Data.Number(i)
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

    def run_function(fn_idx: Int): Data = {
        val tree: BlockTree = functions(fn_idx)
        val (exec_tree, last_ln) = tree.splitAt(tree.length - 1)
        run_iter(exec_tree)

        last_ln(0) match {
            case ast: AST => ast()
            case default  => println("wtf. missing return type"); Data.Number(0)
        }
    }

    def run(code: String) = {
        val res: BlockTree = ProgramParser.parse(code)

        // variables.zipWithIndex.foreach((p, i) => {
        //     val (s, d) = p
        //     var_to_idx(s) = i
        //     vars.append(d);
        // })

        run_iter(res)
    }
}
