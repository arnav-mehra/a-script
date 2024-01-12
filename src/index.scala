package index

import types.util.*
import parsers.line.*
import parsers.block.*
import runner.*

object Index {
    def main(args: Array[String]) = {
        if (args.length == 0) {
            println("Please add a file to run")
            System.exit(1)
        }
        val fname: String = args(0) + ".asc"
        println(fname)
        val code: String = scala.io.Source.fromFile(fname).mkString
        Runner.run(code)
    }
}