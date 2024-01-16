package types.data

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import java.lang.{String => SString}

enum DataType {
    case Number, String, Array, Object, Any, Void

    def binOp(op: String, v2: DataType): DataType = {
        op match {
            case "+" => {
                (this, v2) match {
                    case (Number, Number) => Number
                    case (String, String) => String
                    case (Array,  Array)  => Array
                    case (Object, Object) => Object
                    
                    case (Object, Array)  => Object
                    case (Array, Object)  => Array
        
                    case (Array,  String) => Array
                    case (Number, String) => String
                    case (Object, String) => String
                    case (String, Number) => String
                    case (String, Object) => String
                    case (String, Array)  => String

                    case (Any, _) => Any
                    case (_, Any) => Any
                    case (Void, _) => println("Cannot perform operation with void type."); Any
                    case (_, Void) => println("Cannot perform operation with void type."); Any

                    case default => Number
                }
            }
            case default => Number
        }
    }
}

enum Data {
    case Number(v: Double)
    case String(v: SString)
    case Array (v: ArrayBuffer[Data])
    case Object(v: HashMap[Data, Data])

    def get_type(): DataType = {
        this match {
            case Data.Number(x) => DataType.Number
            case Data.String(x) => DataType.String
            case Data.Array(x)  => DataType.Array
            case Data.Object(x) => DataType.Object
        }
    }

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
        (this, idx) match {
            case (Array(v),  Number(i)) => v(i.toInt) = x
            case (Object(v), _)         => v(idx) = x
            case default => println("shit!"); this
        }
    }

    // MATH OPS

    def +=(op2: Data): Unit = {
        (this, op2) match {
            case (String(v1), String(v2)) => v1 ++: v2
            case (String(v1), Number(v2)) => v1 ++: v2.toString()
            case (Array(v1),  Array(v2) ) => v1.appendAll(v2)
            case (Array(v1),  _)          => v1.append(op2)
            case (Object(v1), Object(v2)) => v1 ++= v2
            case (Object(v1), Array(v2) ) => v1(v2(0)) = v2(1)
            case default => println("shit!")
        }
    }

    def +(op2: Data): Data = {
        (this, op2) match {
            case (Number(v1), Number(v2)) => Number(v1 + v2)
            case (String(v1), String(v2)) => String(v1 + v2)
            case (Array(v1),  Array(v2) ) => Array(v1 ++ v2)
            case (Object(v1), Object(v2)) => Object(v1 ++ v2)
            
            case (Object(v1), Array(v2))  => {
                val res = v1.clone()
                res(v2(0)) = v2(1)
                Object(res)
            }
            case (Array(v1), Object(v2)) => Array(v1.appended(op2))
            case (Array(v1), String(v2)) => Array(v1.appended(op2))

            case (Number(v1), String(v2)) => String(v1.toString() + v2)
            case (Object(v1), String(v2)) => String(v1.toString() + v2)
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
            case (Array(v1),  Array(v2) ) => {
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