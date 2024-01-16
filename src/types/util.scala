package types.util

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import types.data.*

type Program = (ArrayBuffer[Data] => Data)
type Ast = (() => Data)
type AstList = ArrayBuffer[Ast]

type Tree = ArrayBuffer[Node]
enum Node {
    // blocks
    case If   (c: Node, bt: Tree)
    case While(c: Node, bt: Tree)
    case ForTo(v: String, s: Node, e: Node, bt: Tree)
    case ForIn(v: String, arr: Node, bt: Tree)
    case Fn   (v: String, ps: ArrayBuffer[String], bt: Tree)

    // ops
    case Print(e: Node)
    case Call (f: String, bt: Tree)
    case Set  (v: String, fs: Tree, op: String, e: Node)
    case Get  (v: String, fs: Tree)
    case BinOp(e1: Node, op: String, e2: Node)
    case Var  (v: String)
    case Const(e: Data)
}

class Function(
    val n: String,
    val ps: ArrayBuffer[String],
    val bt: Tree
) {
    val vars: HashMap[String, Int] = HashMap().addAll(ps.zipWithIndex)

    def add_var(s: String) = {
        if (!vars.contains(s)) {
            vars(s) = vars.size
        }
    }
}

class Call(
    val fn_name: String,
    val pt: ArrayBuffer[DataType] = ArrayBuffer()
) {
    def fn: Function = Functions.data(fn_name)

    val var_types: HashMap[String, DataType] = HashMap().addAll(
        fn.ps.zipWithIndex.map(v => (v._1, pt(v._2)))
    )
    val node_types: HashMap[Node, DataType] = HashMap()
    val ret_type: DataType = DataType.Number
    val program: Program = (arr => Data.Number(0))

    def set_node_type(n: Node, dt: DataType) = {
        node_types(n) = dt
    }

    def add_var_type(s: String, dt: DataType) = {
        if (var_types.contains(s) && var_types(s) != dt) {
            println("Cannot reassign variable type")
        }
        var_types(s) = dt
    }

    def get_var_type(s: String) = {
        if (!var_types.contains(s)) {
            var_types(s) = DataType.Any
        }
        var_types(s)
    }
}

object Functions {
    val data: HashMap[String, Function] = HashMap()

    def add(n: String, ps: ArrayBuffer[String], bt: Tree) = {
        if (!data.contains(n)) {
            data(n) = Function(n, ps, bt)
        }
    }
}

object Calls {
    val data: HashMap[Node, Call] = HashMap()

    // def add(n: String, ps: ArrayBuffer[String], bt: Tree) = {
    //     if (!data.contains(n)) {
    //         data(n) = Function(n, ps, bt)
    //     }
    // }
}