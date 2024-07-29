package pipeline.minimizer

import scala.util.chaining._

object Minimizer {
    def whitespace_pat_map = Array(
        ("\\s", " "),
        ("\\}", "}"), ("\\{", "{"),
        ("\\]", "]"), ("\\[", "["),
        ("\\(", "("), ("\\)", ")"),
        ("\\.", "."), ("@", "@"),
        (",", ","), (";", ";"),
        ("\\+", "+"), ("-", "-"), ("\\*", "*"), ("/", "/"),
        ("<", "<"), (">", ">"), ("=", "="), ("!", "!")
    )

    def remove_whitespace(code: String): String = {
        val new_code = whitespace_pat_map.foldLeft(code)((code, p) => {
            val from = p._1
            val to = p._2
            code.replaceAll(s"(\\s$from|$from\\s)", to)
        })
        new_code.length() == code.length() match {
            case true => new_code
            case false => remove_whitespace(new_code)
        }
    }

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
            // .pipe(remove_whitespace)
    }
}