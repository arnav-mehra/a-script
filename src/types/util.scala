package types.util

import scala.collection.mutable.HashMap
import java.lang.{String => SString}
import scala.{Array => AArray}
import scala.collection.mutable.ArrayBuffer

enum Data {
    case Number(v: Double)
    case String(v: SString)
    case Array (v: ArrayBuffer[Data])
    case Object(v: HashMap[Data, Data])

    // FIELD GETTERS & SETTERS

    def ->(idx: Data): Data = {
        (this, idx) match {
            case (Array(v),  Number(i)) => v(i.toInt) 
            case (String(v), Number(i)) => String(v.charAt(i.toInt).toString())
            case (Object(v), _)         => v(idx)
            case default => println("shit!"); this
        }
    }

    def set(idx: Data, x: Data) = {
        (this, idx, x) match {
            case (Array(v), Number(i), _) => v(i.toInt) = x
            // not possible until mut string type is used...
            // case (String(v), Number(i), String(xs)) => { 
            //     val sb = StringBuilder(v)
            //     sb.replace(i.toInt, i.toInt + xs.length(), xs)
            // }
            case (Object(v), _, _)         => v(idx) = x
            case default => println("shit!"); this
        }
    }

    // MATH OPS

    def +=(op2: Data): Unit = {
        (this, op2) match {
            case (String(v1), String(v2)) => v1 ++: v2
            case (Array(v1),  Array(v2) ) => v1.appendAll(v2)
            case (Object(v1), Object(v2)) => v1 ++= v2
            case (Object(v1), Array(v2) ) => v1(v2(0)) = v2(1)
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

    // COMPARISON

    def ==(op2: Data): Data = _from_antibool(_equals(op2))
    def !=(op2: Data): Data = _from_antibool(_equals(op2))
    def  <(op2: Data): Data = _from_bool(_less_than(op2))
    def >=(op2: Data): Data = _from_antibool(_less_than(op2))
    def <=(op2: Data): Data = _from_bool(_less_than(op2) || _equals(op2))
    def  >(op2: Data): Data = _from_antibool(_less_than(op2) || _equals(op2))

    // HELPERS

    def _from_bool(b: Boolean): Data = {
        Data.Number(if (b) 1 else 0)
    }

    def _from_antibool(b: Boolean): Data = {
        Data.Number(if (b) 0 else 1)
    }

    def _less_than(op2: Data): Boolean = {
        (this, op2) match {
            case (Number(v1), Number(v2)) => v1 < v2
            case (String(v1), String(v2)) => v1 < v2
            case (Array(v1),  Array(v2) ) => v1.length < v2.length
            case (Object(v1), Object(v2)) => v1.size < v2.size
            case default => println("shit!"); false
        }
    }

    def _equals(op2: Data): Boolean = {
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
            case default => println("shit!"); false
        }
    }

    def _is_truthy = {
        this match {
            case Number(v) => v != 0
            case Array(v)  => v.length != 0
            case Object(v) => v.size != 0
            case String(v) => v.length() != 0
        }
    }
}


type AST = (() => Data)
type BlockTree = ArrayBuffer[BlockNode | AST]

enum BlockNode {
    case While(c: AST, bt: BlockTree)
    case If   (c: AST, bt: BlockTree)
    case ForTo(v: String, s: AST, e: AST, bt: BlockTree)
    case ForIn(v: String, arr: AST, bt: BlockTree)
    case Fn   (v: String, bt: BlockTree)
    case Null ()
}