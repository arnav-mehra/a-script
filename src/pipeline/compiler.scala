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
    val cache = IdentityHashMap[Node, Ast]()

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

    def gen_function_cached: Ast = {
        if (!Compiler.cache.containsKey(caller)) {
            val p: Ast = Compiler(caller).gen_function
            Compiler.cache.put(caller, p)
        }
        Compiler.cache.get(caller)
    }

    def gen_function: Ast = {
        gen_ast(fn.bt)
    }

    def gen_ast_list(bt: Nodes): AstList = {
        bt.filter{
            case _: Node.Fn => false
            case _ => true
        }.map(gen_ast)
    }

    def gen_ast(bn: Node): Ast = {
        bn match {
            case Node.Call("type", arg_nodes) => {
                val arg_asts = gen_ast_list(arg_nodes)
                () => Data.Type(arg_asts(0)().get_type())
            }
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
            case Node.Set(getter, "=", expr_node) => {
                val expr_ast = gen_ast(expr_node)
                val var_offset = fn.get_var(getter.v)
                val field_asts = gen_ast_list(getter.fs)

                field_asts.length match {
                    case 0 => {
                        () => {
                            val lhs_val = Env.get_var(var_offset)
                            val rhs_val = expr_ast()
                            Env.set_var(var_offset, rhs_val)
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
                            modified_val.set(last_field, rhs_val)
                            rhs_val
                        }
                    }
                }
            }
            case Node.Set(e1_bn, op, e2_bn) => {
                val e1: Ast = gen_ast(e1_bn)
                val e2: Ast = gen_ast(e2_bn)
                val e1t: DataType = call.node_types.get(e1_bn)
                val e2t: DataType = call.node_types.get(e2_bn)
                Ops.bin_op(op, e1t, e2t, e1, e2)
            }
            case Node.BinOp(e1_bn, op, e2_bn) => {
                val e1: Ast = gen_ast(e1_bn)
                val e2: Ast = gen_ast(e2_bn)
                val e1t: DataType = call.node_types.get(e1_bn)
                val e2t: DataType = call.node_types.get(e2_bn)
                Ops.bin_op(op, e1t, e2t, e1, e2)
            }
            case Node.Block(nodes) => {
                val block_asts = gen_ast_list(nodes)
                () => block_asts.map(ast => ast()).last
            }
            case node: Node.Match => {
                val val_ast: Ast = gen_ast(node.v)
                val case_block_asts: ArrayBuffer[(Ast, Ast)] = node.c_ls.map(p => (gen_ast(p._1), gen_ast(p._2)))
                val default_val: Data = call.node_types.get(node) match {
                    case DataType.Number => Data.Number(0)
                    case DataType.String => Data.String("")
                    case DataType.Array  => Data.Array(ArrayBuffer())
                    case DataType.Object => Data.Object(HashMap())
                    case other_type      => Data.Type(other_type)
                }
                () => {
                    val v: Data = val_ast()
                    val res = case_block_asts.find((c, b) => v.matches(c()))
                    res match {
                        case Some(_: Ast, ast: Ast) => ast()
                        case None => default_val
                    }
                }
            }
            case Node.While(c_bn, bt) => {
                val c = gen_ast(c_bn)
                val b = gen_ast(bt)
                () => {
                    while (c()._is_truthy) b()
                    Data.Number(0)
                }
            }
            case Node.ForIn(v, e_bn, bt) => {
                val vi = fn.get_var(v)
                val e = gen_ast(e_bn)
                val b = gen_ast(bt)
                () => {
                    val varr: Data = e()
                    varr match {
                        case Data.Array(arr) => {
                            arr.foreach(x => {
                                Env.set_var(vi, x)
                                b()
                            })
                        }
                        case Data.Object(obj) => {
                            obj.foreachEntry((k, v) => {
                                Env.set_var(vi, Data.Array(ArrayBuffer(k, v)))
                                b()
                            })
                        }
                        case _ => {
                            throw Exception("Type Error: Cannot iterate over non-array values.")
                        }
                    }
                    varr
                }
            }
            case Node.ForTo(v, e1_bn, e2_bn, bt) => {
                val vi = fn.get_var(v)
                val e1 = gen_ast(e1_bn)
                val e2 = gen_ast(e2_bn)
                val b = gen_ast(bt)
                () => {
                    val Data.Number(ve1) = e1(): @unchecked
                    val Data.Number(ve2) = e2(): @unchecked
                    for (x <- ve1.toInt to ve2.toInt) {
                        Env.set_var(vi, Data.Number(x))
                        b()
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