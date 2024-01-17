package pipeline.minimizer

import scala.util.chaining._

object Minimizer {
    def top_keywords = Array("if", "while", "for", "fn")
    def mid_keywords = Array("to", "in")
    def kw_regexes = top_keywords.map(s => (s + "\\s", s + "~"))
                  ++ mid_keywords.map(s => ("\\s" + s + "\\s", "~" + s + "~"))

    def rm_comments(code: String): String = {
        code.split("\n")
            .filter(s => s.length() < 2 || s.subSequence(0, 2) != "//")
            .mkString("\n")
    }

    def sep_keywords(code: String): String = {
        kw_regexes.foldLeft(code)((acc, p) => {
            acc.replaceAll(p._1, p._2)
        })
    }

    def rm_whitesp(code: String): String = {
        code.replaceAll("\\s", "")
    }

    def digest(code: String): String = {
        code.pipe(rm_comments)
            .pipe(sep_keywords)
            .pipe(rm_whitesp)
    }
}