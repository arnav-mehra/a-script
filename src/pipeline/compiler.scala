package pipeline.compiler

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import java.util.IdentityHashMap

import types.util.*
import types.data.*
import types.ops.*

// Note: Not actual compiling.
// Follows "compiling" to closures strategy.

object Compiler {
    val cache = IdentityHashMap[Node, Program]()

    def digest(caller: Node): Ast = {
        Compiler(caller).gen_caller()
    }
}

class Compiler(caller: Node) {
    val Node.Call(f, arg_bt) = caller: @unchecked
    val fn: Function = Functions.data(f)
    val call: Call = Calls.get(caller)

    def gen_caller(args: AstList = ArrayBuffer(), stack_shift: Int = 0): Ast = {
        lazy val cb = gen_function_cached

        () => {
            // NOTE: only inc stk ptr after running each arg ast (args run in old ctx).
            for (i <- 0 to args.length - 1) {
                Env.set_var(i + stack_shift, args(i)())
            }

            Env.stack_ptr += stack_shift
            val ret = cb()
            Env.stack_ptr -= stack_shift

            ret
        }
    }

    def gen_function_cached: Program = {
        if (!Compiler.cache.containsKey(caller)) {
            val p: Program = Compiler(caller).gen_function
            Compiler.cache.put(caller, p)
        }
        Compiler.cache.get(caller)
    }

    def gen_function: Program = {
        val ast_ls: AstList = gen_ast_list(fn.bt)
        val (ls, ret_ls) = ast_ls.splitAt(ast_ls.length - 1)
        val ret = ret_ls(0)

        () => {
            ls.foreach(ast => ast())
            ret()
        }
    }

    def gen_ast_list(bt: Tree): AstList = {
        bt.filter{
            case Node.Fn(_, _, _) => false
            case default => true
        }.map(gen_ast)
    }

    def gen_ast(bn: Node): Ast = {
        bn match {
            case Node.Call(_, arg_bt) => {
                val args = gen_ast_list(arg_bt)
                val stack_shift = fn.vars.size
                Compiler(bn).gen_caller(args, stack_shift)
            }
            case Node.Const(n) => {
                () => n
            }
            case Node.Var(v) => {
                val vi = fn.get_var(v)
                () => Env.get_var(vi)
            }
            case Node.Print(v_bn) => {
                val v = gen_ast(v_bn)
                () => {
                    println(v())
                    Data.Number(0)
                }
            }
            case Node.Get(v, fs_bt) => {
                val vi = fn.get_var(v)
                val fs = gen_ast_list(fs_bt)
                () => {
                    var v = Env.get_var(vi)
                    fs.foreach(f => v = v -> f())
                    v
                }
            }
            case Node.Set(v, fs_bt, op, e_bn) => {
                val vi = fn.get_var(v)
                val vt: DataType = call.var_types(v)
                val e = gen_ast(e_bn)
                val fs = gen_ast_list(fs_bt)

                if (fs.length == 0) {
                    val et: DataType = call.node_types.get(e_bn)
                    println(vt.str + " += " + et.str)

                    op match {
                        case "="  => () => { val d = e(); Env.set_var(vi, d); d }
                        case "-=" => () => { val d = Env.get_var(vi) - e(); Env.set_var(vi, d); d }
                        case "*=" => () => { val d = Env.get_var(vi) * e(); Env.set_var(vi, d); d }
                        case "/=" => () => { val d = Env.get_var(vi) / e(); Env.set_var(vi, d); d }
                        case "+=" => Ops.plus_eq(vt, et, vi, e)
                    }
                }
                else {
                    val (fs_pre, fs_fin_ls) = fs.splitAt(fs.length - 1)
                    val fs_fin = fs_fin_ls(0)

                    def extr(): (Data, Data) = {
                        var v = Env.get_var(vi)
                        fs_pre.foreach(f => v = v -> f())
                        (v, fs_fin())
                    }

                    op match {
                        case "=" => () => {
                            val (v, f) = extr(); val d = e(); v.set(f, d); d
                        }
                        case "-=" => () => {
                            val (v, f) = extr(); val d = (v -> f) - e(); v.set(f, d); d
                        }
                        case "*=" => () => {
                            val (v, f) = extr(); val d = (v -> f) * e(); v.set(f, d); d
                        }
                        case "/=" => () => {
                            val (v, f) = extr(); val d = (v -> f) / e(); v.set(f, d); d
                        }
                        case "+=" => () => {
                            val (v, f) = extr(); val d = (v -> f) + e(); v.set(f, d); d

                            // val (v, f) = extr();
                            // val ev = e()
                            // val vf = v -> f

                            // (ev, vf) match {
                            //     case (Data.Number(n1), Data.Number(n2)) => {
                            //         val d = Data.Number(n1 + n2); v.set(f, d); d
                            //     }
                            //     case default => {
                            //         val d = vf + ev; v.set(f, d); d
                            //     }
                            // }
                        }
                    }
                }
            }
            case Node.BinOp(e1_bn, op, e2_bn) => {
                val e1 = gen_ast(e1_bn)
                val e2 = gen_ast(e2_bn)
                val e1t = call.node_types.get(e1_bn)
                val e2t = call.node_types.get(e2_bn)
                Ops.binop(op, e1t, e2t, e1, e2)
            }
            case Node.If(c_bn, bt) => {
                val c = gen_ast(c_bn)
                val b = gen_ast_list(bt)
                () => {
                    if (c()._is_truthy) b.foreach(a => a())
                    Data.Number(0)
                }
            }
            case Node.Match(s_bn, ls) => {
                val s = gen_ast(s_bn)
                val c_ls = ls.map(_._1).map(gen_ast)
                val b_ls = ls.map(_._2).map(gen_ast_list)
                val cb_ls = c_ls.zip(b_ls)
                () => {
                    val v: Data = s()
                    val res = cb_ls.find((c, b) => v == c())
                    res match {
                        case Some(mc: Ast, mb: AstList) => {
                            val (pre, fins) = mb.splitAt(mb.length - 1)
                            val fin = fins(0)
                            pre.foreach(s => s())
                            fin()
                        }
                        case None => {
                            Data.Number(0)
                        }
                    }
                }
            }
            case Node.While(c_bn, bt) => {
                val c = gen_ast(c_bn)
                val b = gen_ast_list(bt)
                () => {
                    while (c()._is_truthy) b.foreach(a => a())
                    Data.Number(0)
                }
            }
            case Node.ForIn(v, e_bn, bt) => {
                val vi = fn.get_var(v)
                val e = gen_ast(e_bn)
                val b = gen_ast_list(bt)
                () => {
                    val varr: Data = e()
                    varr match {
                        case Data.Array(arr) => {
                            for (x <- arr) {
                                Env.set_var(vi, x)
                                b.foreach(a => a())
                            }
                        }
                        case default => println("wtf")
                    }
                    varr
                }
            }
            case Node.ForTo(v, e1_bn, e2_bn, bt) => {
                val vi = fn.get_var(v)
                val e1 = gen_ast(e1_bn)
                val e2 = gen_ast(e2_bn)
                val b = gen_ast_list(bt)
                () => {
                    val Data.Number(ve1) = e1(): @unchecked
                    val Data.Number(ve2) = e2(): @unchecked
                    for (x <- ve1.toInt to ve2.toInt) {
                        Env.set_var(vi, Data.Number(x))
                        b.foreach(a => a())
                    }
                    Data.Number(0)
                }
            }
            case Node.Fn(_, _, _) => () => Data.Number(0) // never executes, but makes scala happy.
        }
    }
}