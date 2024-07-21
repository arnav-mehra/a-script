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
        Indexer(fn).iter_node(bt)
    }
}

class Indexer(val fn_name: String) {
    def fn: Function = Functions.data(fn_name)

    def iter_node(bn: Node): Unit = {
        bn match {
            case bn_fn: Node.Fn             => Indexer.digest(bn_fn)
            case Node.Set(getter, _, _) if getter.fs.length == 0 => fn.add_var(getter.v)
            case Node.While(_, block)       => iter_node(block)
            case Node.ForIn(v, _, block)    => fn.add_var(v); iter_node(block)
            case Node.ForTo(v, _, _, block) => fn.add_var(v); iter_node(block)
            case Node.Match(_, ls)          => ls.map(_._2).foreach(iter_node)
            case Node.Block(lines)          => lines.foreach(iter_node)
            case _ => {}
        }
    }
}