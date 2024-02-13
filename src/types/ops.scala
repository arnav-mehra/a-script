package types.ops

import types.util.*
import types.data.*

object Ops {
    def plus_eq(vt: DataType, et: DataType, vi: Int, e: Ast) = {
        (vt, et) match {
            case (DataType.Number, DataType.Number) => (() => {
                val Data.Number(n1) = Env.get_var(vi): @unchecked
                val Data.Number(n2) = e(): @unchecked
                val d = Data.Number(n1 + n2)
                Env.set_var(vi, d)
                d
            })
            case (DataType.String, DataType.Number) => (() => {
                val Data.String(n1) = Env.get_var(vi): @unchecked
                val Data.Number(n2) = e(): @unchecked
                val d = Data.String(n1 ++: n2.toString())
                Env.set_var(vi, d)
                d
            })
            case default => (() => {
                val v = e()
                Env.get_var(vi) += v
                Env.get_var(vi)
            })
        }
    }
    
    inline def binop(inline op: String, e1t: DataType, e2t: DataType, e1: Ast, e2: Ast) = {
        println("binop: " + op + " " + e1t.str + " " + e2t.str)

        (op, e1t, e2t) match {
            case ("<"|">"|">="|"<="|"=="|"!=", _, _)  => {
                cmpop(op, e1t, e2t, e1, e2)
            }
            case (_, DataType.Number, DataType.Number) => {
                numop(op, e1, e2)
            }
            case ("+", DataType.Array, DataType.Array) => {
                () => {
                    val Data.Array(a1) = e1(): @unchecked
                    val Data.Array(a2) = e2(): @unchecked
                    Data.Array(a1 ++ a2)
                }
            }
            case ("+", DataType.Array, _) => {
                () => {
                    val Data.Array(a1) = e1(): @unchecked
                    Data.Array(a1.appended(e2()))
                }
            }
            case ("+", _, _) => {
                () => e1() + e2()
            }
            case default => {
                println("un impl op")
                () => Data.Number(0)
            }
        }
    }

    inline def cmpop(inline op: String, e1t: DataType, e2t: DataType, e1: Ast, e2: Ast) = {
        val cmp_fn = op match {
            case "<"  => (a: Double, b: Double) => a < b
            case ">"  => (a: Double, b: Double) => a > b
            case ">=" => (a: Double, b: Double) => a >= b
            case "<=" => (a: Double, b: Double) => a <= b
            case "==" => (a: Double, b: Double) => a == b
            case "!=" => (a: Double, b: Double) => a != b
        }

        (e1t, e2t) match {
            case (DataType.Number, DataType.Number) => (() => {
                val Data.Number(n1) = e1(): @unchecked
                val Data.Number(n2) = e2(): @unchecked
                Data.Number(if (cmp_fn(n1, n2)) 1 else 0)
            })
            case (DataType.String, DataType.String) => (() => {
                val Data.String(s1) = e1(): @unchecked
                val Data.String(s2) = e2(): @unchecked
                Data.Number(if (cmp_fn(s1.length, s2.length)) 1 else 0)
            })
            case (DataType.Array, DataType.Array) => (() => {
                val Data.Array(a1) = e1(): @unchecked
                val Data.Array(a2) = e2(): @unchecked
                Data.Number(if (cmp_fn(a1.length, a2.length)) 1 else 0)
            })
            case (DataType.Object, DataType.Object) => (() => {
                val Data.Object(o1) = e1(): @unchecked
                val Data.Object(o2) = e2(): @unchecked
                Data.Number(if (cmp_fn(o1.size, o2.size)) 1 else 0)
            })
            
            case default => (() => Data.Number(0))
        }
    }

    inline def numop(inline op: String, e1: Ast, e2: Ast) = {
        val op_fn = op match {
            case "-" => (a: Double, b: Double) => a - b
            case "+" => (a: Double, b: Double) => a + b
            case "*" => (a: Double, b: Double) => a * b
            case "/" => (a: Double, b: Double) => a / b
        }

        () => {
            val Data.Number(n1) = e1(): @unchecked
            val Data.Number(n2) = e2(): @unchecked
            Data.Number(op_fn(n1, n2))
        }
    }
}