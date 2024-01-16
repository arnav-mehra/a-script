package pipeline.minimizer

def top_keywords = Array("if", "while", "for", "fn")
def mid_keywords = Array("to", "in")
def kw_regexes = top_keywords.map(s => (s + "\\s", s + "~"))
              ++ mid_keywords.map(s => ("\\s" + s + "\\s", "~" + s + "~"))

object Minimizer {
    def digest(code: String): String = {
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
}