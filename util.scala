package util

import java.lang.Double as JavaDouble

enum DataType {
    case Number,
         String,
         Object,
         Array
}

class Data(
    private var _t: DataType = DataType.Number,
    private var _v: AnyRef = new Double(0.0).asInstanceOf[AnyRef]
) {
    def this(x: Any) = {
        val ref = x.asInstanceOf(AnyRef)
        x match {
            case Double => {
                this(DataType.Number, ref)
            }
            case String => {
                this(DataType.String, ref)
            }
            case List[Data] => {
                this(DataType.Array, ref)
            }
            case HashMap[Data, Data] => {
                this(DataType.Object, ref)
            }
            case default => {
                println("wtf, u done did it now")
                this()
            }
        }
    }

    def t = _t
    def t_=(t: DataType) = {
        _t = t
    }

    def v = _v
    def v_=(v: AnyRef) = {
        _v = v
    }

    def get_tv(): Unit = {
        t match {
            case DataType.Number => {
                v.asInstanceOf[Double]
            }
            case DataType.String => {
                v.asInstanceOf[String]
            }
            case DataType.Array => {
                v.asInstanceOf[List[Data]]
            }
            case DataType.Object => {
                v.asInstanceOf[Object[Data, Data]]
            }
        }
    }

    def +=(op: Data) = {
        val v1 = get_tv()
        val v2 = op2.get_tv()

        (t, op2.t) match {
            case (DataType.Object, DataType.Array) => {
                v1 += (v2(0), v2(1))
            }
            case (DataType.String, DataType.String)
               | (DataType.Number, DataType.Number)
               | (DataType.Object, DataType.Object)
               | (DataType.Array, DataType.Array) => {
                v1 += v2
            }
        }
        val n = this + op
        v = n.v
        t = n.t
    }

    def +(op2: Data): Data = {
        val v1 = get_tv()
        val v2 = op2.get_tv()

        (t, op2.t) match {
            case (DataType.Number, DataType.Number)
               | (DataType.String, DataType.String) => {
                new Data(v1 + v2)
            }
            case (DataType.Array, DataType.Array) 
               | (DataType.Object, DataType.Object) => {
                new Data(v1 ++ v2)
            }
            case (DataType.Array, _) => {
                val v3 = v1.map(x => {
                    if (x.t == op2.t) {
                        x + op2
                    } else {
                        x
                    }
                })
                new Data(v3)
            }
            case (DataType.String, _) => {
                new Data(v1 + v2.toString())
            }
            case (_, DataType.String) => {
                new Data(v1.toString() + v2)
            }
            case default => {
                println("this aint a + op")
                new Data()
            }
        }
    }

    def -(op2: Data): Data = {
        val v1 = get_tv()
        val v2 = op2.get_tv()

        (t, op2.t) match {
            case (DataType.Number, DataType.Number)
               | (DataType.String, DataType.String)
               | (DataType.Object, _) => {
                new Data(v1 - v2)
            }
            case default => {
                println("this aint a + op")
                new Data()
            }
        }
    }

    def ++(): Data = {
        val v1 = get_tv()

        t match {
            case DataType.Number => {
                new Data(v1 + 1)
            }
            case default => {
                println("this aint a + op")
                new Data()
            }
        }
    }

    def --(): Data = {
        val v1 = get_tv()

        t match {
            case DataType.Number => {
                new Data(v1 - 1)
            }
            case default => {
                println("this aint a + op")
                new Data()
            }
        }
    }

    def *(op2: Data): Data = {
        val v1 = get_tv()
        val v2 = op2.get_tv()

        (t, op2.t) match {
            case (DataType.Number) => {
                new Data(v1 * v2)
            }
            case default => {
                println("this aint a + op")
                new Data()
            }
        }
    }

    def /(op2: Data): Data = {
        val v1 = get_tv()
        val v2 = op2.get_tv()

        (t, op2.t) match {
            case (DataType.Number) => {
                new Data(v1 / v2)
            }
            case default => {
                println("this aint a + op")
                new Data()
            }
        }
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
}