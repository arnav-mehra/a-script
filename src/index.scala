package index

import types.util.*
import parsers.line.*
import parsers.block.*
import runner.*

object Index {
    def main(args: Array[String]) = {
        // def main(args: Array[String]) = {
        // val code : String = """
        //     x = 10;
        //     while x {
        //         x -= 1;
        //         if x == 5 {
        //             x -= 1;
        //         }
        //     }
        // """

        val code : String = """
            x = [];
            x += 5;
        """
        Runner.run(code)
    }
}