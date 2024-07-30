package index

import pipeline.runner.*

object Index {
    def EXT = ".asc"

    var folder = "."

    def read_file(raw_rel_path: String) = {
        val rel_path = if (raw_rel_path.endsWith(EXT)) raw_rel_path
                       else raw_rel_path + EXT
        val abs_path = folder + "\\" + rel_path
        val content = scala.io.Source.fromFile(abs_path).mkString
        // println("read file: " + abs_path + "\n" + content)
        content
    }

    def main(args: Array[String]) = {
        if (args.length == 0) {
            throw Error("File Error: No file specified.")
        }

        val file = new java.io.File(args(0))
        folder = file.getParent()
        println(folder)
        val code: String = read_file(file.getName())
        Runner.run(code)
    }
}