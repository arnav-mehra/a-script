package types.ops

import types.util.*
import types.data.*

object Ops {
    def bin_fn[A <: Data, B <: Data](op: String) = op match {
        case "+"  => (a: A, b: B) => a + b
        case "-"  => (a: A, b: B) => a - b
        case "*"  => (a: A, b: B) => a * b
        case "/"  => (a: A, b: B) => a / b

        case "<"  => (a: A, b: B) => a < b
        case ">"  => (a: A, b: B) => a > b
        case ">=" => (a: A, b: B) => a >= b
        case "<=" => (a: A, b: B) => a <= b
        case "==" => (a: A, b: B) => a == b
        case "!=" => (a: A, b: B) => a != b
        case "~~" => (a: A, b: B) => a ~~ b
        case "!~" => (a: A, b: B) => a !~ b

        case "+=" => (a: A, b: B) => a += b; a
        case "-=" => (a: A, b: B) => a -= b; a
        case "*=" => (a: A, b: B) => a *= b; a
        case "/=" => (a: A, b: B) => a /= b; a
    }

    def bin_apply[A <: Data, B <: Data](op: String, e1: Ast, e2: Ast) = {
        val fn = bin_fn[A, B](op)
        () => {
            val a1 = e1().asInstanceOf[A]
            val a2 = e2().asInstanceOf[B]
            fn(a1, a2)
        }
    }

    def bin_op(op: String, e1t: DataType, e2t: DataType, e1: Ast, e2: Ast) = {
        (e1t, e2t) match {
            case (DataType.Number, DataType.Number) => bin_apply[Data.Number, Data.Number](op, e1, e2)
            case (DataType.Number, DataType.String) => bin_apply[Data.Number, Data.String](op, e1, e2)
            case (DataType.Number, DataType.Array ) => bin_apply[Data.Number, Data.Array ](op, e1, e2)
            case (DataType.Number, DataType.Object) => bin_apply[Data.Number, Data.Object](op, e1, e2)
            case (DataType.Number, DataType.Type  ) => bin_apply[Data.Number, Data.Type  ](op, e1, e2)
            case (DataType.Number, _              ) => bin_apply[Data.Number, Data](op, e1, e2)

            case (DataType.String, DataType.Number) => bin_apply[Data.String, Data.Number](op, e1, e2)
            case (DataType.String, DataType.String) => bin_apply[Data.String, Data.String](op, e1, e2)
            case (DataType.String, DataType.Array ) => bin_apply[Data.String, Data.Array ](op, e1, e2)
            case (DataType.String, DataType.Object) => bin_apply[Data.String, Data.Object](op, e1, e2)
            case (DataType.String, DataType.Type  ) => bin_apply[Data.String, Data.Type  ](op, e1, e2)
            case (DataType.String, _              ) => bin_apply[Data.String, Data](op, e1, e2)

            case (DataType.Array, DataType.Number) => bin_apply[Data.Array, Data.Number](op, e1, e2)
            case (DataType.Array, DataType.String) => bin_apply[Data.Array, Data.String](op, e1, e2)
            case (DataType.Array, DataType.Array ) => bin_apply[Data.Array, Data.Array ](op, e1, e2)
            case (DataType.Array, DataType.Object) => bin_apply[Data.Array, Data.Object](op, e1, e2)
            case (DataType.Array, DataType.Type  ) => bin_apply[Data.Array, Data.Type  ](op, e1, e2)
            case (DataType.Array, _              ) => bin_apply[Data.Array, Data](op, e1, e2)

            case (DataType.Object, DataType.Number) => bin_apply[Data.Object, Data.Number](op, e1, e2)
            case (DataType.Object, DataType.String) => bin_apply[Data.Object, Data.String](op, e1, e2)
            case (DataType.Object, DataType.Array ) => bin_apply[Data.Object, Data.Array ](op, e1, e2)
            case (DataType.Object, DataType.Object) => bin_apply[Data.Object, Data.Object](op, e1, e2)
            case (DataType.Object, DataType.Type  ) => bin_apply[Data.Object, Data.Type  ](op, e1, e2)
            case (DataType.Object, _              ) => bin_apply[Data.Object, Data](op, e1, e2)

            case (DataType.Type, DataType.Number) => bin_apply[Data.Type, Data.Number](op, e1, e2)
            case (DataType.Type, DataType.String) => bin_apply[Data.Type, Data.String](op, e1, e2)
            case (DataType.Type, DataType.Array ) => bin_apply[Data.Type, Data.Array ](op, e1, e2)
            case (DataType.Type, DataType.Object) => bin_apply[Data.Type, Data.Object](op, e1, e2)
            case (DataType.Type, DataType.Type  ) => bin_apply[Data.Type, Data.Type  ](op, e1, e2)
            case (DataType.Type, _              ) => bin_apply[Data.Type, Data](op, e1, e2)

            case (_, DataType.Number) => bin_apply[Data, Data.Number](op, e1, e2)
            case (_, DataType.String) => bin_apply[Data, Data.String](op, e1, e2)
            case (_, DataType.Array ) => bin_apply[Data, Data.Array ](op, e1, e2)
            case (_, DataType.Object) => bin_apply[Data, Data.Object](op, e1, e2)
            case (_, DataType.Type  ) => bin_apply[Data, Data.Type  ](op, e1, e2)
            case (_, _              ) => bin_apply[Data, Data](op, e1, e2)
        }
    }
}