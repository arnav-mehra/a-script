package util

import scala.collection.mutable.HashMap

enum DataType {
    case Number,
         String,
         Object,
         Array
}

type NumberType = Double 
type StringType = String
type ArrayType = List[Data]
type ObjectType = HashMap[Data, Data]

class Data(
    private var _t: DataType = DataType.Number,
    private var _v: AnyRef = (0.0).asInstanceOf[AnyRef]
) {
    def this(x: NumberType) = this(DataType.Number, x.asInstanceOf[AnyRef])
    def this(x: StringType) = this(DataType.String, x.asInstanceOf[AnyRef])
    def this(x: ArrayType)  = this(DataType.Array,  x.asInstanceOf[AnyRef])
    def this(x: ObjectType) = this(DataType.Object, x.asInstanceOf[AnyRef])

    def t = _t
    def t_=(t: DataType) = {
        _t = t
    }

    def v = _v
    def v_=(v: AnyRef) = {
        _v = v
    }

    def get_tv(): NumberType | StringType | ArrayType | ObjectType = {
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
            case ObjectType => {
                v.asInstanceOf[HashMap[Data, Data]]
            }
        }
    }

    def +=(op2: Data) = {
        val v1 = get_tv()
        val v2 = op2.get_tv()

        (v1, v2) match {
            case (ObjectType, ArrayType) => {
                v1 += (v2(0), v2(1))
            }
            case (StringType, StringType)
               | (NumberType, NumberType)
               | (ObjectType, ObjectType)
               | (ArrayType, ArrayType) => {
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
            case (NumberType, NumberType)
               | (StringType, StringType) => {
                new Data(v1 + v2)
            }
            case (ArrayType, ArrayType) 
               | (ObjectType, ObjectType) => {
                new Data(v1 ++ v2)
            }
            case (ArrayType, _) => {
                val v3 = v1.map(x => {
                    if (x.t == op2.t) {
                        x + op2
                    } else {
                        x
                    }
                })
                new Data(v3)
            }
            case (StringType, _) => {
                new Data(v1 + v2.toString())
            }
            case (_, StringType) => {
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
            case (NumberType, NumberType)
               | (StringType, StringType)
               | (ObjectType, _) => {
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
            case NumberType => {
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
            case NumberType => {
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
            case (NumberType) => {
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
            case (NumberType) => {
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