package pipeline.compiler

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import java.util.IdentityHashMap

import types.util.*
import types.data.*

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

    def gen_caller(args: AstList = ArrayBuffer()): Ast = {
        lazy val cb = gen_function_cached
        val stack_sz = fn.vars.size

        () => {
            val stack = ArrayBuffer.fill[Data](stack_sz)(Data.Number(0))
            for (i <- 0 to args.length - 1) {
                stack(i) = args(i)()
            }
            cb(stack)
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

        (stack) => {
            val old_stack = Env.vars
            Env.vars = stack
            ls.foreach(ast => ast())
            val retv = ret()
            Env.vars = old_stack
            retv
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
                Compiler(bn).gen_caller(args)
            }
            case Node.Const(n) => {
                () => n
            }
            case Node.Var(v) => {
                val vi = fn.get_var(v)
                () => Env.vars(vi)
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
                    var v = Env.vars(vi)
                    fs.foreach(f => v = v -> f())
                    v
                }
            }
            case Node.Set(v, fs_bt, op, e_bn) => {
                val vi = fn.get_var(v)
                val e = gen_ast(e_bn)
                val fs = gen_ast_list(fs_bt)

                if (fs.length == 0) {
                    op match {
                        case "="  => () => { Env.vars(vi) = e(); Data.Number(0) }
                        case "-=" => () => { Env.vars(vi) = Env.vars(vi) - e(); Data.Number(0) }
                        case "*=" => () => { Env.vars(vi) = Env.vars(vi) * e(); Data.Number(0) }
                        case "/=" => () => { Env.vars(vi) = Env.vars(vi) / e(); Data.Number(0) }
                        case "+=" => () => {
                            val ev = e()
                            val vv = Env.vars(vi)
                            (ev, vv) match {
                                case (Data.Number(n1), Data.Number(n2)) => {
                                    Env.vars(vi) = Env.vars(vi) + ev
                                }
                                case default => {
                                    Env.vars(vi) += ev
                                }
                            }
                            Data.Number(0)
                        }
                    }
                }
                else {
                    val (fs_pre, fs_fin_ls) = fs.splitAt(fs.length - 1)
                    val fs_fin = fs_fin_ls(0)

                    def extr(): (Data, Data) = {
                        var v = Env.vars(vi)
                        fs_pre.foreach(f => v = v -> f())
                        (v, fs_fin())
                    }

                    op match {
                        case "=" => () => {
                            val (v, f) = extr(); v.set(f, e()); Data.Number(0)
                        }
                        case "-=" => () => {
                            val (v, f) = extr(); v.set(f, (v -> f) - e()); Data.Number(0)
                        }
                        case "*=" => () => {
                            val (v, f) = extr(); v.set(f, (v -> f) * e()); Data.Number(0)
                        }
                        case "/=" => () => {
                            val (v, f) = extr(); v.set(f, (v -> f) / e()); Data.Number(0)
                        }
                        case "+=" => () => {
                            val (v, f) = extr();
                            val ev = e()
                            val vf = v -> f
                            (ev, vf) match {
                                case (Data.Number(n1), Data.Number(n2)) => v.set(f, vf - ev)
                                case default => v -> f += ev
                            }
                            Data.Number(0)
                        }
                    }
                }
            }
            case Node.BinOp(e1_bn, op, e2_bn) => {
                val e1 = gen_ast(e1_bn)
                val e2 = gen_ast(e2_bn)
                op match {
                    case "+"  => () => e1() +  e2()
                    case "-"  => () => e1() -  e2()
                    case "*"  => () => e1() *  e2()
                    case "/"  => () => e1() /  e2()
                    case "==" => () => e1() == e2()
                    case "!=" => () => e1() != e2()
                    case ">"  => () => e1() >  e2()
                    case "<"  => () => e1() <  e2()
                    case ">=" => () => e1() >= e2()
                    case "<=" => () => e1() <= e2()
                }
            }
            case Node.If(c_bn, bt) => {
                val c = gen_ast(c_bn)
                val b = gen_ast_list(bt)
                () => {
                    if (c()._is_truthy) b.foreach(a => a())
                    Data.Number(0)
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
                                Env.vars(vi) = x
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
                        Env.vars(vi) = Data.Number(x)
                        b.foreach(a => a())
                    }
                    Data.Number(0)
                }
            }
            case Node.Fn(_, _, _) => () => Data.Number(0) // never executes, but makes scala happy.
        }
    }
}