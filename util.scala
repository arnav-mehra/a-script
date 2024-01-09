package util

import java.lang.Double as JavaDouble

enum DataType {
    case Number,
         String,
         Object,
         Array
}

class Data(
    private var _t: DataType = Number,
    private var _v: AnyRef = new Double(0.0)
) {
    def t = _t
    def t_=(t: DataType) = {
        _t = t
    }

    def v = _v
    def v_=(v: AnyRef) = {
        _v = v
    }

    def +=(op: Data) = {
        val n = this + 
    }

    def +(op: Data): Data = {
        (t, op.t) match {
            case (DataType.Number, DataType.Number) => {
                val curr = Double.unbox(v)
                val next = Double.unbox(op.v)
                val new_val = curr + next;

                new Data(DataType.Number, Double.box(new_val))
            }
            case (DataType.Array, _) => {
                val ls = v.asInstanceOf[List[Data]]
                val nls = ls.map(x => {
                    if (x.t == op.t) {
                        x + op
                    } else {
                        x
                    }
                })

                new Data(DataType.Array, nls.asInstanceOf[AnyRef])
            }
            case default: {

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