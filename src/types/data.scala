package types.data

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import java.lang.{String => SString}

enum DataType {
    case Number, String, Array, Object, Type, Any, Void

    def binOp(op: String, v2: DataType): DataType = {
        op match {
            case "+" => this + v2
            case _ => Number
        }
    }

    def +(op2: DataType): DataType = {
        (this, op2) match {
            case (Number, Number) => Number
            case (String, String) => String
            case (Array,  Array ) => Array
            case (Object, Object) => Object
            case (Type,   Type  ) => Type

            case (Object, Array ) => Object
            case (Array,  Object) => Array
            case (Array,  String) => Array

            case (Number, String) => String
            case (Object, String) => String
            case (String, Number) => String
            case (String, Object) => String
            case (String, Array ) => String

            case (Void,   _     ) => throw Exception("Type Error: Invalid + operation.")
            case (_,      Void  ) => throw Exception("Type Error: Invalid + operation.")
            case (Any,    _     ) => Any
            case (_,      Any   ) => Any

            case _ => throw Exception("Type Error: Invalid + operation.")
        }
    }

    def -(op2: DataType): DataType = {
        (this, op2) match {
            case (Number, Number) => Number
            case _ => throw Exception("Type Error: Invalid - operation.")
        }
    }

    def *(op2: DataType): DataType = {
        (this, op2) match {
            case (Number, Number) => Number
            case _ => throw Exception("Type Error: Invalid * operation.")
        }
    }

    def /(op2: DataType): DataType = {
        (this, op2) match {
            case (Number, Number) => Number
            case _ => throw Exception("Type Error: Invalid / operation.")
        }
    }

    def default_val: Data = this match {
        case DataType.Number => Data.Number(0)
        case DataType.String => Data.String("")
        case DataType.Array  => Data.Array(ArrayBuffer())
        case DataType.Object => Data.Object(HashMap())
        case other_type      => Data.Type(other_type)
    }

    def is_iterable: Boolean = {
        this match {
            case Any | Array | Object => true
            case default => false
        }
    }

    def is_void: Boolean = this == Void

    def str: SString = this.toString()
}

enum Data {
    case Number(var v: Double)
    case String(var v: SString)
    case Array (var v: ArrayBuffer[Data])
    case Object(var v: HashMap[Data, Data])
    case Type  (var v: DataType)

    def get_type(): DataType = {
        this match {
            case Data.Number(x) => DataType.Number
            case Data.String(x) => DataType.String
            case Data.Array(x)  => DataType.Array
            case Data.Object(x) => DataType.Object
            case Data.Type(x)   => DataType.Type
        }
    }

    def matches(that: Data): Boolean = {
        (this, that) match {
            case (_, Data.Type(DataType.Any))     => true
            case (Data.Type(a), Data.Type(b))     => a == b
            case (_, Data.Type(b))                => this.get_type() == b
            case (Data.Array(a), Data.Array(b))   => a.size == b.size && a.zip(b).forall(p => p._1.matches(p._2))
            case (Data.Object(a), Data.Object(b)) => a.size == b.size && a.forall(ent => b.contains(ent._1) && ent._2.matches(b(ent._1)))
            case _                                => this == that
        }
    }

    // FIELD GETTERS & SETTERS

    def ->(idx: Data): Data = {
        (this, idx) match {
            case (Array(v),  Number(i)) => v(i.toInt) 
            case (String(v), Number(i)) => String(v.charAt(i.toInt).toString())
            case (Object(v), _)         => v(idx)
            case _ => throw Exception("Run-time Error: Invalid indexing operation.")
        }
    }

    def set(idx: Data, x: Data) = {
        (this, idx) match {
            case (Array(v),  Number(i)) => v(i.toInt) = x
            case (Object(v), _)         => v(idx) = x
            case _ => throw Exception("Run-time Error: Invalid = operation.")
        }
    }

    // MATH OPS

    def +=(op2: Data): Unit = {
        (this, op2) match {
            case (v1: Number, v2: Number) => v1.v += v2.v
            case (String(v1), String(v2)) => v1 ++: v2
            case (String(v1), Number(v2)) => v1 ++: v2.toString()
            case (Array(v1),  Array(v2) ) => v1.appendAll(v2)
            case (Array(v1),  _)          => v1.append(op2)
            case (Object(v1), Object(v2)) => v1 ++= v2
            case (Object(v1), Array(v2) ) => v1(v2(0)) = v2(1)
            case _ => throw Exception("Operation Error: Invalid += operation.")
        }
    }

    def -=(op2: Data): Unit = {
        (this, op2) match {
            case (v1: Number, v2: Number) => v1.v -= v2.v
            case _ => throw Exception("Operation Error: Invalid -= operation.")
        }
    }

    def *=(op2: Data): Unit = {
        (this, op2) match {
            case (v1: Number, v2: Number) => v1.v *= v2.v
            case _ => throw Exception("Operation Error: Invalid *= operation.")
        }
    }

    def /=(op2: Data): Unit = {
        (this, op2) match {
            case (v1: Number, v2: Number) => v1.v /= v2.v
            case _ => throw Exception("Operation Error: Invalid /= operation.")
        }
    }

    def +(op2: Data): Data = {
        (this, op2) match {
            case (Number(v1), Number(v2)) => Number(v1 + v2)
            case (String(v1), String(v2)) => String(v1 + v2)
            case (Array(v1),  Array(v2) ) => Array(v1 ++ v2)
            case (Object(v1), Object(v2)) => Object(v1 ++ v2)
            case (Type(v1),   Type(v2)  ) => Type(v1 + v2)

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

            case _ => throw Exception("Run-time Error: Invalid + operation.")
        }
    }

    def -(op2: Data): Data = {
        (this, op2) match {
            case (Number(v1), Number(v2)) => Number(v1 - v2)
            case _ => throw Exception("Run-time Error: Invalid - operation.")
        }
    }

    def *(op2: Data): Data = {
        (this, op2) match {
            case (Number(v1), Number(v2)) => Number(v1 * v2)
            case _ => throw Exception("Run-time Error: Invalid * operation.")
        }
    }

    def /(op2: Data): Data = {
        (this, op2) match {
            case (Number(v1), Number(v2)) => Number(v1 / v2)
            case _ => throw Exception("Run-time Error: Invalid / operation.")
        }
    }

    // COMPARISON

    def ==(op2: Data): Data.Number = _from_bool(_equals(op2))
    def !=(op2: Data): Data.Number = _from_antibool(_equals(op2))
    def  <(op2: Data): Data.Number = _from_bool(_less_than(op2))
    def >=(op2: Data): Data.Number = _from_antibool(_less_than(op2))
    def <=(op2: Data): Data.Number = _from_bool(_less_than(op2) || _equals(op2))
    def  >(op2: Data): Data.Number = _from_antibool(_less_than(op2) || _equals(op2))
    def ~~(op2: Data): Data.Number = _from_bool(matches(op2))
    def !~(op2: Data): Data.Number = _from_antibool(matches(op2))

    // HELPERS

    def _from_bool(b: Boolean): Data.Number = {
        Data.Number(if (b) 1 else 0)
    }

    def _from_antibool(b: Boolean): Data.Number = {
        Data.Number(if (b) 0 else 1)
    }

    def _less_than(op2: Data): Boolean = {
        (this, op2) match {
            case (Number(v1), Number(v2)) => v1 < v2
            case (String(v1), String(v2)) => v1 < v2
            case _ => throw Exception("Run-time Error: Invalid inequality comparison operation.")
        }
    }

    def _equals(op2: Data): Boolean = {
        (this, op2) match {
            case (Number(v1), Number(v2)) => v1 == v2
            case (String(v1), String(v2)) => v1 == v2
            case (Array(v1),  Array(v2) ) => v1.sameElements(v2)
            case (Object(v1), Object(v2)) => v1.equals(v2)
            case (Type(v1),   Type(v2)  ) => v1 == v2
            case _ => false
        }
    }

    def _is_truthy: Boolean = {
        this match {
            case Number(v) => v != 0
            case Array(v)  => v.length != 0
            case Object(v) => v.size != 0
            case String(v) => v.length() != 0
            case Type(v)   => v != DataType.Void
        }
    }
}