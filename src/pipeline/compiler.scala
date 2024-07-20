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

    def gen_ast_list(bt: Nodes): AstList = {
        bt.filter{
            case _: Node.Fn => false
            case _ => true
        }.map(gen_ast)
    }

    def gen_ast(bn: Node): Ast = {
        bn match {
            case Node.Call(_, arg_nodes) => {
                val arg_asts = gen_ast_list(arg_nodes)
                val stack_shift = fn.vars.size
                Compiler(bn).gen_caller(arg_asts, stack_shift)
            }
            case Node.Const(value) => {
                () => value
            }
            case Node.Print(val_node) => {
                val val_ast = gen_ast(val_node)
                () => {
                    val v = val_ast()
                    println(v)
                    v
                }
            }
            case Node.Get(var_name, field_nodes) if field_nodes.length == 0 => {
                val var_offset = fn.get_var(var_name)
                () => Env.get_var(var_offset)
            }
            case Node.Get(var_name, field_nodes) => {
                val var_offset = fn.get_var(var_name)
                val field_asts = gen_ast_list(field_nodes)
                () => {
                    var v = Env.get_var(var_offset)
                    field_asts.foldLeft(v)((acc, field_ast) => acc -> field_ast())
                }
            }
            case Node.Set(getter, op, expr_node) => {
                val expr_ast = gen_ast(expr_node)
                val var_offset = fn.get_var(getter.v)
                val field_asts = gen_ast_list(getter.fs)

                val new_val_fn = op match {
                    case "="  => (lhs: Data, rhs: Data) => rhs
                    case "+=" => (lhs: Data, rhs: Data) => lhs + rhs
                    case "-=" => (lhs: Data, rhs: Data) => lhs - rhs
                    case "*=" => (lhs: Data, rhs: Data) => lhs * rhs
                    case "/=" => (lhs: Data, rhs: Data) => lhs / rhs
                }

                field_asts.length match {
                    case 0 => {
                        op match {
                            case "+=" => {
                                val expr_type = call.node_types.get(expr_node)
                                val var_type = call.var_types(getter.v)
                                Ops.plus_eq(var_type, expr_type, var_offset, expr_ast)
                            }
                            case _ => {
                                () => {
                                    val lhs_val = Env.get_var(var_offset)
                                    val rhs_val = expr_ast()
                                    val new_val = new_val_fn(lhs_val, rhs_val)
                                    Env.set_var(var_offset, new_val)
                                }
                            }
                        }
                    }
                    case _ => {
                        val (field_asts_pre, field_asts_fin_ls) = field_asts.splitAt(field_asts.length - 1)
                        val field_asts_fin = field_asts_fin_ls(0)

                        () => {
                            val fielded_val = Env.get_var(var_offset)
                            val modified_val = field_asts_pre.foldLeft(fielded_val)((v, field_ast) => v -> field_ast())
                            val last_field = field_asts_fin()

                            val lhs_val = modified_val -> last_field
                            val rhs_val = expr_ast()
                            val new_val = new_val_fn(lhs_val, rhs_val)
                            modified_val.set(last_field, new_val)
                            new_val
                        }
                    }
                }
            }
            case Node.BinOp(e1_bn, op, e2_bn) => {
                val e1: Ast = gen_ast(e1_bn)
                val e2: Ast = gen_ast(e2_bn)
                val e1t: DataType = call.node_types.get(e1_bn)
                val e2t: DataType = call.node_types.get(e2_bn)
                Ops.binop(op, e1t, e2t, e1, e2)
            }
            case Node.If(c_bn, bt) => {
                val condition: Ast = gen_ast(c_bn)
                val block: AstList = gen_ast_list(bt)
                () => {
                    if (condition()._is_truthy) block.map(a => a()).last
                    else Data.Number(0)
                }
            }
            case Node.Match(s_bn, ls) => {
                val s: Ast = gen_ast(s_bn)
                val c_ls: ArrayBuffer[Ast] = ls.map(_._1).map(gen_ast)
                val b_ls: ArrayBuffer[AstList] = ls.map(_._2).map(gen_ast_list)
                val cb_ls: ArrayBuffer[(Ast, AstList)] = c_ls.zip(b_ls)
                () => {
                    val v: Data = s()
                    val res = cb_ls.find((c, b) => v == c())
                    res match {
                        case Some(_: Ast, block: AstList) => {
                            block.map(s => s()).last
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
            case Node.Fn(_, _, _) => {
                () => Data.Number(0) // never executes, but makes scala happy.
            }
        }
    }
}