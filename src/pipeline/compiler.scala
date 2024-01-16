// package pipeline.compiler

// import scala.collection.mutable.HashMap
// import scala.collection.mutable.ArrayBuffer
// import scala.collection.mutable.HashSet

// import types.util.*
// import types.data.*

// // Note: Not actual compiling.
// // Follows "compiling" to closures strategy.

// object Functions {
//     class Pair(var p: Program, var c: CompilerCtx)
//     val data: HashMap[String, Pair] = HashMap()

//     def get(n: String): Pair = {
//         if (!data.contains(n)) {
//             data(n) = Pair(a => Data.Number(0), CompilerCtx())
//         }
//         data(n)
//     }

//     def add(n: String, p: Program, c: CompilerCtx) = {
//         if (!data.contains(n)) {
//             data(n) = Pair(p, c)
//         }
//         data(n).p = p
//         data(n).c = c
//     }
// }

// object Env {
//     var vars: ArrayBuffer[Data] = ArrayBuffer()
// }

// class CompilerCtx {
//     val var_to_idx: HashMap[String, Int] = HashMap()

//     def get_var(s: String): Int = {
//         if (!var_to_idx.contains(s)) {
//             val i: Int = var_to_idx.size
//             var_to_idx(s) = i
//         }
//         var_to_idx(s)
//     }

//     def genProgram(bt: Tree): Program = {
//         val ast_ls: AstList = genAstList(bt)
//         val (ls: AstList, ret_ls: AstList) = ast_ls.splitAt(ast_ls.length - 1)
//         val ret = ret_ls(0)

//         (args => {
//             val old_vars = Env.vars
//             Env.vars = args
//             ls.foreach(ast => ast())
//             val retv = ret()
//             Env.vars = old_vars
//             retv
//         })
//     }

//     def genAstList(bt: Tree): AstList = {
//         bt.map(a => genAst(a)).collect { case Some(v) => v }
//     }

//     def genAst(bn: Node): Option[Ast] = {
//         bn match {
//             case Node.Fn(v, ps, bt) => {
//                 val ctx = CompilerCtx()
//                 ps.foreach(p => ctx.get_var(p))
//                 val p: Program = ctx.genProgram(bt)
//                 Functions.add(v, p, ctx)
//                 None
//             }
//             case Node.Call(n, bt) => {
//                 val args = genAstList(bt)
//                 val pair = Functions.get(n)
//                 val v_hm = pair.c.var_to_idx
//                 Some(() => {
//                     val stack: ArrayBuffer[Data] = ArrayBuffer.fill(v_hm.size)(Data.Number(0))
//                     for (i <- 0 to args.length - 1) {
//                         stack(i) = args(i)()
//                     }
//                     pair.p(stack)
//                 })
//             }
//             case Node.Const(n) => {
//                 Some(() => n)
//             }
//             case Node.Var(v) => {
//                 val vi = get_var(v)
//                 Some(() => Env.vars(vi))
//             }
//             case Node.Print(v_bn) => {
//                 val Some(v) = genAst(v_bn): @unchecked
//                 Some(() => {
//                     println(v())
//                     Data.Number(0)
//                 })
//             }
//             case Node.Get(v, fs_bt) => {
//                 val vi = get_var(v)
//                 val fs = genAstList(fs_bt)
//                 Some(() => {
//                     var v = Env.vars(vi)
//                     fs.foreach(f => v = v -> f())
//                     v
//                 })
//             }
//             case Node.Set(v, fs_bt, op, e_bn) => {
//                 val vi = get_var(v)
//                 val Some(e) = genAst(e_bn): @unchecked
//                 val fs = genAstList(fs_bt)

//                 if (fs.length == 0) {
//                     op match {
//                         case "="  => Some(() => { Env.vars(vi) = e(); Data.Number(0) })
//                         case "-=" => Some(() => { Env.vars(vi) = Env.vars(vi) - e(); Data.Number(0) })
//                         case "*=" => Some(() => { Env.vars(vi) = Env.vars(vi) * e(); Data.Number(0) })
//                         case "/=" => Some(() => { Env.vars(vi) = Env.vars(vi) / e(); Data.Number(0) })
//                         case "+=" => Some(() => {
//                             val ev = e()
//                             val vv = Env.vars(vi)
//                             (ev, vv) match {
//                                 case (Data.Number(n1), Data.Number(n2)) => {
//                                     Env.vars(vi) = Env.vars(vi) + ev
//                                 }
//                                 case default => {
//                                     Env.vars(vi) += ev
//                                 }
//                             }
//                             Data.Number(0)
//                         })
//                     }
//                 }
//                 else {
//                     val (fs_pre, fs_fin_ls) = fs.splitAt(fs.length - 1)
//                     val fs_fin = fs_fin_ls(0)

//                     def extr(): (Data, Data) = {
//                         var v = Env.vars(vi)
//                         fs_pre.foreach(f => v = v -> f())
//                         (v, fs_fin())
//                     }

//                     op match {
//                         case "=" => Some(() => {
//                             val (v, f) = extr(); v.set(f, e()); Data.Number(0)
//                         })
//                         case "-=" => Some(() => {
//                             val (v, f) = extr(); v.set(f, (v -> f) - e()); Data.Number(0)
//                         })
//                         case "*=" => Some(() => {
//                             val (v, f) = extr(); v.set(f, (v -> f) * e()); Data.Number(0)
//                         })
//                         case "/=" => Some(() => {
//                             val (v, f) = extr(); v.set(f, (v -> f) / e()); Data.Number(0)
//                         })
//                         case "+=" => Some(() => {
//                             val (v, f) = extr();
//                             val ev = e()
//                             val vf = v -> f
//                             (ev, vf) match {
//                                 case (Data.Number(n1), Data.Number(n2)) => v.set(f, vf - ev)
//                                 case default => v -> f += ev
//                             }
//                             Data.Number(0)
//                         })
//                     }
//                 }
//             }
//             case Node.BinOp(e1_bn, op, e2_bn) => {
//                 val Some(e1) = genAst(e1_bn): @unchecked
//                 val Some(e2) = genAst(e2_bn): @unchecked
//                 op match {
//                     case "+"  => Some(() => e1() +  e2())
//                     case "-"  => Some(() => e1() -  e2())
//                     case "*"  => Some(() => e1() *  e2())
//                     case "/"  => Some(() => e1() /  e2())
//                     case "==" => Some(() => e1() == e2())
//                     case "!=" => Some(() => e1() != e2())
//                     case ">"  => Some(() => e1() >  e2())
//                     case "<"  => Some(() => e1() <  e2())
//                     case ">=" => Some(() => e1() >= e2())
//                     case "<=" => Some(() => e1() <= e2())
//                 }
//             }
//             case Node.If(c_bn, bt) => {
//                 val Some(c) = genAst(c_bn): @unchecked
//                 val b = genAstList(bt)
//                 Some(() => {
//                     if (c()._is_truthy) b.foreach(a => a())
//                     Data.Number(0)
//                 })
//             }
//             case Node.While(c_bn, bt) => {
//                 val Some(c) = genAst(c_bn): @unchecked
//                 val b = genAstList(bt)
//                 Some(() => {
//                     while (c()._is_truthy) b.foreach(a => a())
//                     Data.Number(0)
//                 })
//             }
//             case Node.ForIn(v, e_bn, bt) => {
//                 val vi = get_var(v)
//                 val Some(e) = genAst(e_bn): @unchecked
//                 val b = genAstList(bt)
//                 Some(() => {
//                     val varr: Data = e()
//                     varr match {
//                         case Data.Array(arr) => {
//                             for (x <- arr) {
//                                 Env.vars(vi) = x
//                                 b.foreach(a => a())
//                             }
//                         }
//                         case default => println("wtf")
//                     }
//                     varr
//                 })
//             }
//             case Node.ForTo(v, e1_bn, e2_bn, bt) => {
//                 val vi = get_var(v)
//                 val Some(e1) = genAst(e1_bn): @unchecked
//                 val Some(e2) = genAst(e2_bn): @unchecked
//                 val b = genAstList(bt)
//                 Some(() => {
//                     val Data.Number(ve1) = e1(): @unchecked
//                     val Data.Number(ve2) = e2(): @unchecked
//                     for (x <- ve1.toInt to ve2.toInt) {
//                         Env.vars(vi) = Data.Number(x)
//                         b.foreach(a => a())
//                     }
//                     Data.Number(0)
//                 })
//             }
//         }
//     }
// }