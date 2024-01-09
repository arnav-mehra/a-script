file:///C:/Users/Arnav/Documents/GitHub/apl/apl/index.scala
### dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition BlockTree is defined in
  C:/Users/Arnav/Documents/GitHub/apl/apl/util.scala
and also in
  C:/Users/Arnav/Documents/GitHub/apl/apl/util.scala
One of these files should be removed from the classpath.

occurred in the presentation compiler.

action parameters:
uri: file:///C:/Users/Arnav/Documents/GitHub/apl/apl/index.scala
text:
```scala
package index

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.HashMap

import util.Wrapper
import util.BlockType
import util.BlockNode
import util
import line.LineParser

object ProgramParser extends JavaTokenParsers {
    val functions: HashMap[String, List[Any]] = HashMap();

    def program:Parser[List[String | BlockNode]] = rep(line) ^^ (lst => lst)

    def function_block:Parser[Any] = "fn " ~ variable ~ block ^^ {
        case "fn "~v~b => functions(v) = b
        case default => println("wtf"); 0
    }

    def while_block:Parser[BlockNode] = "while " ~ statement ~ block ^^ {
        case "while "~s~b => BlockNode(BlockType.While, s, b)
        case default => println("wtf"); BlockNode()
    }

    def if_block:Parser[BlockNode] = "if " ~ statement ~ block ^^ {
        case "if "~s~b => BlockNode(BlockType.If, s, b)
        case default => println("wtf"); BlockNode()
    }

    def block:Parser[List[String | BlockNode]] = "{" ~ rep(line) ~ "}" ^^ {
        case "{"~lst~"}" => lst
        case default => List()
    }

    def line:Parser[Any] =
        statement ~ ";" ^^ {
            case s~";" => s
        }
 if_block
 while_block
 function_block

    def statement:Parser[String] = "[^;{}]*".r ^^ (s => s)

    def variable:Parser[String] = "[a-z]+".r ^^ (s => s)

    def run_iter(tree: List[String | BlockNode]): Unit = {
        for node <- tree do {
            node match {
                case node: BlockNode => {
                    node.bt match {
                        case BlockType.If => {
                            if (LineParser.parseAll(LineParser.line, node.cond).get.v != 0) {
                                run_iter(node.ls)
                            }
                        }
                        case BlockType.While => {
                            while (LineParser.parseAll(LineParser.line, node.cond).get.v != 0) {
                                run_iter(node.ls)
                            }
                        }
                    }
                }
                case ln: String => {
                    LineParser.parseAll(LineParser.line, ln).get
                }
                case default => println("wtf")
            }
        }
    }

    def run_function(fn_name: String): Wrapper[Any] = {
        val tree: List[String | BlockNode] = functions(fn_name)
        val (exec_tree, last_ln) = tree.splitAt(tree.length - 1)
        run_iter(exec_tree)

        last_ln(0) match {
            case ln: String => LineParser.parseAll(LineParser.line, ln).get
            case default => println("wtf"); Wrapper(0.0)
        }
    }

    def run(code: String) = {
        val min_code = code.replaceAll("\\s", "")
                           .replaceAll("if", "if ")
                           .replaceAll("while", "while ")
                           .replaceAll("fn", "fn ")
        val res = parseAll(program, min_code)
        run_iter(res.get)
    }

    def main(args: Array[String]) = {
        val code : String = """
            x = 10;
            while x {
                x -= 1;
            }
        """
        run(code)
    }
}
```



#### Error stacktrace:

```

```
#### Short summary: 

dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition BlockTree is defined in
  C:/Users/Arnav/Documents/GitHub/apl/apl/util.scala
and also in
  C:/Users/Arnav/Documents/GitHub/apl/apl/util.scala
One of these files should be removed from the classpath.