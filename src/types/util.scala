package types.util

import scala.collection.mutable.HashMap
import java.lang.{String => SString}
import scala.{Array => AArray}
import scala.collection.mutable.ArrayBuffer

enum Data {
    case Number(v: Double) extends Data
    case String(v: SString) extends Data
    case Array (v: ArrayBuffer[Data]) extends Data
    case Object(v: HashMap[Data, Data]) extends Data

    def +=(op2: Data): Unit = {
        (this, op2) match {
            case (String(v1), String(v2)) => v1 ++: v2
            case (Array(v1),  Array(v2) ) => v1.appendAll(v2)
            case (Object(v1), Object(v2)) => v1 ++= v2
            case (Object(v1), Array(v2) ) => v1 += v2(0) -> v2(1)
            case (Array(v1),  _)          => v1.append(op2)
            case (String(v1), Number(v2)) => v1 ++: v2.toString()
            case default => println("shit!")
        }
    }

    def +(op2: Data): Data = {
        (this, op2) match {
            case (Number(v1), Number(v2)) => Data.Number(v1 + v2)
            case (String(v1), String(v2)) => String(v1 + v2)
            case (Array(v1),  Array(v2) ) => Array(v1 ++ v2)
            case (Object(v1), Object(v2)) => Object(v1 ++ v2)
            
            case (Number(v1), String(v2)) => String(v1.toString() + v2)
            case (Object(v1), String(v2)) => String(v1.toString() + v2)
            case (Array(v1),  String(v2)) => String(v1.toString() + v2)
            case (String(v1), Number(v2)) => String(v1 + v2.toString())
            case (String(v1), Object(v2)) => String(v1 + v2.toString())
            case (String(v1), Array(v2))  => String(v1 + v2.toString())

            case default => println("shit!"); this
        }
    }

    def -(op2: Data): Data = {
        (this, op2) match {
            case (Number(v1), Number(v2)) => Number(v1 - v2)
            case default => println("shit!"); this
        }
    }

    def *(op2: Data): Data = {
        (this, op2) match {
            case (Number(v1), Number(v2)) => Number(v1 * v2)
            case default => println("shit!"); this
        }
    }

    def /(op2: Data): Data = {
        (this, op2) match {
            case (Number(v1), Number(v2)) => Number(v1 / v2)
            case default => println("shit!"); this
        }
    }

    def ==(op2: Data): Data = {
        if (this.equals(op2)) Data.Number(1) else Data.Number(0)
    }

    def !=(op2: Data): Data = {
        if (this.equals(op2)) Data.Number(0) else Data.Number(1)
    }

    def equals(op2: Data): Boolean = {
        (this, op2) match {
            case (Number(v1), Number(v2)) => v1 == v2
            case (String(v1), String(v2)) => v1 == v2
            case (Array(v1), Array(v2))   => {
                v1.zip(v2).foldLeft(false)((acc, p) => acc || (p._1 == p._2))
            }
            case (Object(v1), Object(v2)) => {
                ((v1.keySet -- v2.keySet).size == 0)
                    && ((v2.keySet -- v1.keySet).size == 0)
            }
            case default => false
        }
    }
}

enum BlockType {
    case While,
         If,
         Fn,
         Null
}

type BlockTree = ArrayBuffer[BlockNode | String]

class BlockNode(
    private var _bt: BlockType = BlockType.Null,
    private var _cond: String = "",
    private var _ls: BlockTree = ArrayBuffer()
) {
    def bt = _bt
    def cond = _cond
    def ls = _ls
}

// object Test {
//     def main(args: AArray[SString]) = {
//         val v1 = Data.Number(5)
//         val v2 = Data.Number(4)
//         print(v1 + v2)
//     }
// }