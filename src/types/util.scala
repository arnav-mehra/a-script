package types.util

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import types.data.*
import java.util.IdentityHashMap

type Program = (() => Data)
type Ast = (() => Data)
type AstList = ArrayBuffer[Ast]

type Nodes = ArrayBuffer[Node]
enum Node {
    // blocks
    case If   (c: Node, bt: Nodes)
    case Match(v: Node, c_ls: ArrayBuffer[(Node, Nodes)])
    case While(c: Node, bt: Nodes)
    case ForTo(v: String, s: Node, e: Node, bt: Nodes)
    case ForIn(v: String, arr: Node, bt: Nodes)
    case Fn   (v: String, ps: ArrayBuffer[String], bt: Nodes)

    // ops
    case Print(e: Node)
    case Call (f: String, bt: Nodes)
    case Set  (v: Node.Get, op: String, e: Node)
    case Get  (v: String, fs: Nodes)
    case BinOp(e1: Node, op: String, e2: Node)
    case Const(e: Data)
}

class Function(
    val n: String,
    val ps: ArrayBuffer[String],
    val bt: Nodes
) {
    val vars = HashMap().addAll(ps.zipWithIndex)

    def add_var(s: String) = {
        if (!vars.contains(s)) {
            vars(s) = vars.size
        }
    }

    def get_var(s: String) = vars(s)
}

class Call(
    val fn_name: String,
    val pt: ArrayBuffer[DataType] = ArrayBuffer()
) {
    def fn: Function = Functions.data(fn_name)

    val var_types = HashMap().addAll(fn.ps.zipWithIndex.map(v => (v._1, pt(v._2))))
    val node_types = IdentityHashMap[Node, DataType]()
    var ret_type = DataType.Any

    def set_node_type(n: Node, dt: DataType) = {
        node_types.put(n, dt)
    }

    def add_var_type(s: String, dt: DataType) = {
        if (var_types.contains(s) && var_types(s).assignable(dt)) {
            println("Cannot reassign " + s + " to type " + dt.str);
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

object Env {
    val stack = ArrayBuffer.fill(1000)(Data.Number(0))
    var stack_ptr = 0

    inline def get_var(offset: Int): Data = {
        stack(stack_ptr + offset)
    }

    inline def set_var(offset: Int, new_val: Data): Data = {
        stack(stack_ptr + offset) = new_val
        new_val
    }
}

object Functions {
    val data: HashMap[String, Function] = HashMap()

    def add(n: String, ps: ArrayBuffer[String], bt: Nodes) = {
        if (!data.contains(n)) {
            data(n) = Function(n, ps, bt)
        }
    }
}

object Calls {
    val data: IdentityHashMap[Node.Call, Call] = IdentityHashMap()

    def has = data.containsKey
    def add = data.put
    def get = data.get
}