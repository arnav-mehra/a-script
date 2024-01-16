package pipeline.runner

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

import types.util.*
import types.data.*
import parsers.block.*
import parsers.line.*
import pipeline.minimizer.* 
// import pipeline.compiler.*
import pipeline.typer.*
import pipeline.indexer.*

object Runner {
    def run(code: String): Unit = {
        val mcode: String = Minimizer.digest(code)
        println(mcode); println()

        val (root_fn: Node, root_caller: Node) = ProgramParser.parse(mcode)
        println(root_fn); println()

        Indexer.digest(root_fn)
        Typer.digest(root_caller)

        Functions.data.foreach((n, f) => {
            print(n + ": "); println(f.vars)
        })
        println()

        Calls.data.forEach((n, c) => {
            val Node.Call(f, _) = n: @unchecked
            println(f + " -> " + c.ret_type)
            println(c.var_types)
        })

        // val typer = Typer("_")
        // val ret_type = typer.get_ret_type(res)
        // typer.node_types.foreach(a => {println(a); println()})

        // val cctx = CompilerCtx()
        // val p: Program = cctx.genProgram(res)
        // val args: ArrayBuffer[Data] = ArrayBuffer.fill(ctx.var_to_idx.size)(Data.Number(0))

        // val ret = p(args)
        // print("ret: "); println(ret)
    }
}