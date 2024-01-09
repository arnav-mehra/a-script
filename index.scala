// package index

// import scala.util.parsing.combinator.JavaTokenParsers
// import scala.collection.mutable.HashMap

// import util.{Wrapper, BlockType, BlockNode, BlockTree}
// import line.LineParser

// object ProgramParser extends JavaTokenParsers {
//     val functions: HashMap[String, BlockTree] = HashMap();

//     def program:Parser[BlockTree] = rep(line) ^^ (lst => lst)

//     def function_block:Parser[BlockNode] = "fn " ~ variable ~ block ^^ {
//         case "fn "~v~b => {
//             functions(v) = b
//             BlockNode(BlockType.Fn)
//         }
//         case default => {
//             println("wtf")
//             BlockNode()
//         }
//     }

//     def while_block:Parser[BlockNode] = "while " ~ statement ~ block ^^ {
//         case "while "~s~b => {
//             BlockNode(BlockType.While, s, b)
//         }
//         case default => {
//             println("wtf")
//             BlockNode()
//         }
//     }

//     def if_block:Parser[BlockNode] = "if " ~ statement ~ block ^^ {
//         case "if "~s~b => {
//             BlockNode(BlockType.If, s, b)
//         }
//         case default => {
//             println("wtf");
//             BlockNode()
//         }
//     }

//     def block:Parser[BlockTree] = "{" ~ rep(line) ~ "}" ^^ {
//         case "{"~lst~"}" => lst
//         case default => List()
//     }

//     def line:Parser[BlockNode | String] =
//         statement ~ ";" ^^ {
//             case s~";" => s
//         }
//         | if_block
//         | while_block
//         | function_block

//     def statement:Parser[String] = "[^;{}]*".r ^^ (s => s)

//     def variable:Parser[String] = "[a-z]+".r ^^ (s => s)

//     def run_iter(tree: BlockTree): Unit = {
//         for node <- tree do {
//             node match {
//                 case node: BlockNode => {
//                     node.bt match {
//                         case BlockType.If => {
//                             if (LineParser.parseAll(LineParser.line, node.cond).get.v != 0) {
//                                 run_iter(node.ls)
//                             }
//                         }
//                         case BlockType.While => {
//                             while (LineParser.parseAll(LineParser.line, node.cond).get.v != 0) {
//                                 run_iter(node.ls)
//                             }
//                         }
//                         case BlockType.Fn => {
//                             functions(node.cond) = node.ls
//                         }
//                         case util.BlockType.Null => {
//                             println("wtf")
//                         }
//                     }
//                 }
//                 case ln: String => {
//                     LineParser.parseAll(LineParser.line, ln).get
//                 }
//             }
//         }
//     }

//     def run_function(fn_name: String): Wrapper[Any] = {
//         val tree: BlockTree = functions(fn_name)
//         val (exec_tree, last_ln) = tree.splitAt(tree.length - 1)
//         run_iter(exec_tree)

//         last_ln(0) match {
//             case ln: String => LineParser.parseAll(LineParser.line, ln).get
//             case default => println("wtf"); Wrapper(0.0)
//         }
//     }

//     def run(code: String) = {
//         val min_code = code.replaceAll("\\s", "")
//                            .replaceAll("if", "if ")
//                            .replaceAll("while", "while ")
//                            .replaceAll("fn", "fn ")
//         val res = parseAll(program, min_code)
//         run_iter(res.get)
//     }

//     def main(args: Array[String]) = {
//         val code : String = """
//             x = 10;
//             while x {
//                 x -= 1;
//             }
//         """
//         run(code)
//     }
// }