package types.util

import scala.collection.mutable.ArrayBuffer

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