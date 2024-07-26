package pipeline.minimizer

import scala.util.chaining._

object Minimizer {
    def remove_comments(code: String): String = {
        code.split("\n")
            .map(s => s.indexOf("//") match {
                case -1 => s
                case idx => s.subSequence(0, idx)
            })
            .mkString("\n")
    }

    def digest(code: String): String = {
        code.pipe(remove_comments)
    }
}