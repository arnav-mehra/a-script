// package line

// import scala.util.parsing.combinator.JavaTokenParsers
// import scala.collection.mutable.HashMap

// import util.{DataType, Data, BlockType}
// import index.ProgramParser

// object LineParser extends JavaTokenParsers {
//     val variables : HashMap[String, Data] = HashMap();

//     def line:Parser[Data] = setter | form

//     def setter:Parser[Data] = variable ~ ("="|"+="|"-="|"*="|"/=") ~ form ^^ {
//         case x~s~f => {
//             s match {
//                 case "="  => x = f
//                 case "+=" => x += f
//                 case "-=" => x -= f
//                 case "*=" => x *= f
//                 case "/=" => x /= f
//             }
//             x
//         }
//     }

//     def form:Parser[Data] = term ~ rep(("+" | "-") ~ term) ^^ {
//         case t1~lst => {
//             lst.foldLeft(t1)(
//                 (acc, t) => if (t._1 == "+") acc + t._2 else acc - t._2
//             )
//         }
//     }
//     def term:Parser[Data] = factor ~ rep(("*" | "/") ~ factor) ^^ {
//         case f1~lst => {
//             lst.foldLeft(f1)(
//                 (acc, f) => if (f._1 == "*") acc * f._2 else acc / f._2
//             )
//         }
//     }
//     def factor:Parser[Data] =
//         "(" ~ form ~ ")" ^^ (x => x._1._2)
//         | number
//         | variable
//         | caller

//     // def array: Parser[List[Double]] = "[" ~ number ~ rep("," ~ number) ~ "]" ^^ {
//     //     case "["~n~lst~"]" => {
//     //         List(n) ::: lst.map(p => p._2)
//     //     }
//     // }

//     def caller: Parser[Data] = "[a-z]+".r ~ "()" ^^ {
//         case v~"()" => {
//             ProgramParser.run_function(v)
//         }
//     }
//     def variable: Parser[Data] = "[a-z]+".r ^^ {
//         s => {
//             if (!variables.contains(s)) {
//                 variables(s) = new Data(0.0)   
//             }
//             variables(s)
//         }
//     }
//     def number: Parser[Data] = """(0|[1-9]\d*)""".r ^^ {
//         s => new Data(DataType.Number, s.toDouble)
//     }

//     def run(code: String) = {
//         code.replaceAll("\\s", "")
//             .split(";")
//             .filter(x => x.length() > 0)
//             .foreach(l => {
//                 println(l)
//                 val res = parseAll(line, l)
//                 println(res.get.v)
//             })
//     }

//     def main(args: Array[String]) = {
//         val code : String = """
//             x = 1;
//             y = 1;
//             x *= 2;
//             x *= 2;
//             x;
//         """

//         // run(code)
        
//         // 2+3*5 
//         // => term(2) [+ term(3*5)]
//         // => (factor(2), []) [+ (factor(3), [* factor(5)])]
//         // => (float(2), []) [+ (float(3), [* float(5)])]
//     }
// }