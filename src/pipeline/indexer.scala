// Indexes functions. Perhaps also classes/structs/etc in the future.
package pipeline.indexer

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import types.util.*
import types.data.*

object Indexer {
    def digest(n: Node.Fn) = {
        val Node.Fn(fn, ps, bt) = n
        Functions.add(fn, ps, bt)
        Indexer(fn).iter_tree(bt)
    }
}

class Indexer(val fn_name: String) {
    def fn: Function = Functions.data(fn_name)

    def iter_tree(bt: Nodes): Unit = {
        bt.foreach(iter_node)
    }

    def iter_node(bn: Node): Unit = {
        bn match {
            case bn_fn: Node.Fn          => Indexer.digest(bn_fn)
            case Node.Set(v, fs, _, _)   => if (fs.length == 0) fn.add_var(v)
            case Node.If(_, bt)          => iter_tree(bt)
            case Node.While(_, bt)       => iter_tree(bt)
            case Node.ForIn(v, _, bt)    => fn.add_var(v); iter_tree(bt)
            case Node.ForTo(v, _, _, bt) => fn.add_var(v); iter_tree(bt)
            case Node.Match(_, ls)       => ls.map(_._2).foreach(iter_tree)
            case default => {}
        }
    }
}