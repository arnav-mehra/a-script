package pipeline.minimizer

import scala.util.chaining._

object Minimizer {
    def top_keywords = Array("if", "while", "for", "fn", "match")
    def mid_keywords = Array("to", "in")
    def keyword_regexes =
        top_keywords.map(s => (s + "\\s", s + "~"))
        ++ mid_keywords.map(s => ("\\s" + s + "\\s", "~" + s + "~"))

    def remove_comments(code: String): String = {
        code.split("\n")
            .map(s => if (s.contains("//")) s.subSequence(0, s.indexOf("//")) else s)
            .mkString("\n")
    }

    def separate_keywords(code: String): String = {
        keyword_regexes.foldLeft(code)((acc, p) => {
            acc.replaceAll(p._1, p._2)
        })
    }

    def remove_whitespace(code: String): String = {
        code.replaceAll("\\s", "")
    }

    def digest(code: String): String = {
        code.pipe(remove_comments)
            .pipe(separate_keywords)
            .pipe(remove_whitespace)
    }
}