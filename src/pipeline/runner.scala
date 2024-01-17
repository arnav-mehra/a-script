package pipeline.runner

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

import types.util.*
import types.data.*
import parsers.block.*
import parsers.line.*

import pipeline.minimizer.* 
import pipeline.indexer.*
import pipeline.typer.*
import pipeline.compiler.*

object Runner {
    def run(code: String): Unit = {
        val mcode: String = Minimizer.digest(code)

        val (root_fn: Node, root_caller: Node) = ProgramParser.parse(mcode)

        Indexer.digest(root_fn)
        Typer.digest(root_caller)
        Compiler.digest(root_caller)()

        println(mcode); println()
        println(root_fn); println()
        Functions.data.foreach((n, f) => {
            print(n + ": "); println(f.vars)
        })
        println()
        Calls.data.forEach((n, c) => {
            val Node.Call(f, _) = n: @unchecked
            println(f + " -> " + c.ret_type)
            println(c.var_types)
        })
    }
}