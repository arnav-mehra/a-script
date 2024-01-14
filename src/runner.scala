package runner

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

import types.util.*
import parsers.block.*
import parsers.line.*

object Runner {
    def keywords = Array("if", "while", "for", "to", "in", "fn")
    def kw_regexes = keywords.map(s => (
        "\\s" + s + "\\s", "~" + s + "~"
    ))

    val functions: HashMap[Int, AST] = HashMap();
    val var_to_idx: HashMap[String, Int] = HashMap();
    val vars: ArrayBuffer[Data] = ArrayBuffer();

    def min_code(code: String): String = {
        // remove comments
        var mcode: String = (
            code.split("\n")
                .filter(s => s.length() < 2 || s.subSequence(0, 2) != "//")
                .mkString("\n")
        )
        // clarify keywords
        kw_regexes.foreach(p => mcode = mcode.replaceAll(p._1, p._2))
        // remove whitespace
        mcode = mcode.replaceAll("\\s", "")
        mcode
    }

    def run(code: String): Unit = {
        val mcode = min_code(code)
        println(mcode)
        val res: AST = ProgramParser.parse(mcode)
        res()
    }

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
