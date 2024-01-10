package util

import scala.collection.mutable.HashMap

abstract class Data {
    case class Number(v: Double) extends Data
    case class String(v: String) extends Data
    case class Array(v: List[Data]) extends Data
    case class Object(v: HashMap[Data, Data]) extends Data

    def +=(op2: Data): Unit = {
        (this, op2) match {
            case (Object(v1), Array(v2) ) => v1 += v2(0) -> v2(1)
            case (String(v1), String(v2)) => v1 += v2
            case (Number(v1), Number(v2)) => this.Number(v1 + v2)
            case (Array(v1),  Array(v2) ) => v1 :++ v2
            case (Object(v1), Object(v2)) => v1 ++= v2
        }
    }

    def +(op2: Data): Data = {
        (this, op2) match {
            case (Number(v1), Number(v2)) => new this.Number(v1 + v2)
            case (String(v1), String(v2)) => new this.String(v1 + v2)
            case (Array(v1),  Array(v2) ) => new this.Array(v1 ++ v2)
            case (Object(v1), Object(v2)) => new this.Object(v1 ++ v2)
            
            case (Number(v1), String(v2)) => new this.String(v1.toString() + v2)
            case (Object(v1), String(v2)) => new this.String(v1.toString() + v2)
            case (Array(v1), String(v2)) => new this.String(v1.toString() + v2)
            
            case (String(v1), Number(v2)) => new this.String(v1 + v2.toString())
            case (String(v1), Object(v2)) => new this.String(v1 + v2.toString())
            case (String(v1), Array(v2)) => new this.String(v1 + v2.toString())

            case default => {
                println("this aint a + op")
                new Data.Number(0.0)
            }
        }
    }

    // def -(op2: Data): Data = {
    //     val v1 = get_tv()
    //     val v2 = op2.get_tv()

    //     (t, op2.t) match {
    //         case (NumberType, NumberType)
    //            | (StringType, StringType)
    //            | (ObjectType, _) => {
    //             new Data(v1 - v2)
    //         }
    //         case default => {
    //             println("this aint a + op")
    //             new Data()
    //         }
    //     }
    // }

    // def ++(): Data = {
    //     val v1 = get_tv()

    //     t match {
    //         case NumberType => {
    //             new Data(v1 + 1)
    //         }
    //         case default => {
    //             println("this aint a + op")
    //             new Data()
    //         }
    //     }
    // }

    // def --(): Data = {
    //     val v1 = get_tv()

    //     t match {
    //         case NumberType => {
    //             new Data(v1 - 1)
    //         }
    //         case default => {
    //             println("this aint a + op")
    //             new Data()
    //         }
    //     }
    // }

    // def *(op2: Data): Data = {
    //     val v1 = get_tv()
    //     val v2 = op2.get_tv()

    //     (t, op2.t) match {
    //         case (NumberType) => {
    //             new Data(v1 * v2)
    //         }
    //         case default => {
    //             println("this aint a + op")
    //             new Data()
    //         }
    //     }
    // }

    // def /(op2: Data): Data = {
    //     val v1 = get_tv()
    //     val v2 = op2.get_tv()

    //     (t, op2.t) match {
    //         case (NumberType) => {
    //             new Data(v1 / v2)
    //         }
    //         case default => {
    //             println("this aint a + op")
    //             new Data()
    //         }
    //     }
    // }
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