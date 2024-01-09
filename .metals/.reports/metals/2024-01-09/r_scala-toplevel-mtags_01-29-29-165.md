error id: file:///C:/Users/Arnav/Documents/GitHub/apl/apl/util.scala:[203..206) in Input.VirtualFile("file:///C:/Users/Arnav/Documents/GitHub/apl/apl/util.scala", "package util

enum DataType {
    case Number,
         String,
         Object,
         Array
}

class Wrapper(
    private var _t: DataType,
    private var _v: Any
) {
    def type
    def value = _v
    def value_=(v: Any) = {
        _v = v
    }
}

enum BlockType {
    case While,
         If,
         Fn,
         Null
}

type BlockTree = List[BlockNode | String]

class BlockNode(
    private var _bt: BlockType = BlockType.Null,
    private var _cond: String = "",
    private var _ls: BlockTree = List()
) {
    def bt = _bt
    def cond = _cond
    def ls = _ls
}")
file:///C:/Users/Arnav/Documents/GitHub/apl/apl/util.scala
file:///C:/Users/Arnav/Documents/GitHub/apl/apl/util.scala:15: error: expected identifier; obtained def
    def value = _v
    ^
#### Short summary: 

expected identifier; obtained def