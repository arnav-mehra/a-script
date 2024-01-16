package pipeline.typer

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

import types.util.*
import types.data.*
import parsers.block.*
import parsers.line.*

object Typer {
    def digest(fn_name: String, call_bn: Node, pt: ArrayBuffer[DataType]) = {
        if (!Calls.data.contains(call_bn)) {
            Calls.data(call_bn) = Call(fn_name, pt)
            Typer(fn_name, call_bn).gen_ret_type
        }
    }
}

class Typer(fn_name: String, call_bn: Node) {
    def call: Call = Calls.data(call_bn)
    def fn: Function = Functions.data(fn_name)

    def gen_ret_type: DataType = {
        val (bt_ls, bt_ret_ls) = fn.bt.splitAt(fn.bt.length - 1)
        val bt_ret = bt_ret_ls(0)

        bt_ls.foreach(a => gen_node_type(a))
        gen_node_type(bt_ret)
    }

    def iter_nodes(bt: Tree): Unit = {
        bt.foreach(gen_node_type)
    }

    def gen_node_type(bn: Node): DataType = {
        val tp: DataType = bn match {
            case Node.Fn(fn, ps, bt) => {
                DataType.Any
            }
            case Node.Call(fn, bt) => {
                val pt: ArrayBuffer[DataType] = bt.map(gen_node_type)
                Typer.digest(fn, bn, pt)
                DataType.Any
            }
            case Node.Print(e) => {
                gen_node_type(e)
                DataType.Void
            }
            case Node.Var(v)   => {
                call.get_var_type(v)
            }
            case Node.Const(n) => n.get_type()
            case Node.Get(v, fs_bt) => {
                if (fs_bt.length == 0) {
                    call.get_var_type(v)
                } else {
                    DataType.Any
                }
            }
            case Node.Set(v, fs_bt, op, e_bn) => {
                val et = gen_node_type(e_bn)
                if (fs_bt.length == 0) call.add_var_type(v, et)
                DataType.Void
            }
            case Node.BinOp(e1_bn, op, e2_bn) => {
                val e1t: DataType = gen_node_type(e1_bn)
                val e2t: DataType = gen_node_type(e2_bn)
                e1t.binOp(op, e2t)
            }
            case Node.If(c_bn, bt) => {
                val c = gen_node_type(c_bn)
                iter_nodes(bt)
                DataType.Void
            }
            case Node.While(c_bn, bt) => {
                val c = gen_node_type(c_bn)
                iter_nodes(bt)
                DataType.Void
            }
            case Node.ForIn(v, e_bn, bt) => {
                call.add_var_type(v, DataType.Any)
                val e = gen_node_type(e_bn)
                iter_nodes(bt)
                DataType.Void
            }
            case Node.ForTo(v, e1_bn, e2_bn, bt) => {
                call.add_var_type(v, DataType.Number)
                val e1 = gen_node_type(e1_bn)
                val e2 = gen_node_type(e2_bn)
                iter_nodes(bt)
                DataType.Void
            }
            case default => DataType.Void
        }
        call.set_node_type(bn, tp)
        tp
    }   
}