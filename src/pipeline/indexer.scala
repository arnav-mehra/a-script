// Indexes functions. Perhaps also classes/structs/etc in the future.
package pipeline.indexer

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import types.util.*
import types.data.*

object Indexer {
    def digest(n: Node) = {
        val Node.Fn(fn, ps, bt) = n: @unchecked
        Functions.add(fn, ps, bt)
        Indexer(fn).iter_tree(bt)
    }
}

class Indexer(val fn_name: String) {
    def fn: Function = Functions.data(fn_name)

    def iter_tree(bt: Tree): Unit = {
        bt.foreach(a => iter_node(a))
    }

    def iter_node(bn: Node): Unit = {
        bn match {
            case Node.Fn(v, ps, bt)      => Indexer.digest(bn)
            case Node.Set(v, fs, _, _)   => if (fs.length == 0) fn.add_var(v)
            case Node.If(_, bt)          => iter_tree(bt)
            case Node.Match(_, ls)       => ls.map(_._2).foreach(iter_tree)
            case Node.While(_, bt)       => iter_tree(bt)
            case Node.ForIn(v, _, bt)    => fn.add_var(v); iter_tree(bt)
            case Node.ForTo(v, _, _, bt) => fn.add_var(v); iter_tree(bt)
            case default => {}
        }
    }
}