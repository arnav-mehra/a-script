package index

import pipeline.runner.*

object Index {
    def EXT = "asc"

    def main(args: Array[String]) = {
        if (args.length == 0) {
            throw Error("File Error: No file specified.")
        }

        val fname: String = if (args(0).endsWith(EXT)) args(0)
                            else args(0) + EXT
        println("Running: " + fname)

        val code: String = scala.io.Source.fromFile(fname).mkString
        Runner.run(code)
    }
}