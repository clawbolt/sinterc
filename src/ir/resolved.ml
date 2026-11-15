(* modules.ml 的结果：合成好的程序，和读入时登记的、按名字查的表。类型检查从这里取，不再读 Ast 里的全局表。 *)
open Ast

(* 内建方法（intrinsic fn）：编译成的函数名（例如 Str.len）登记一条。不生成函数，方法调用直接换成内建函数 builtin。 *)
type prim = {
  builtin : string;
  params : (mode * string * ty) list;
  ret : ty;
  tparams : (string * bool) list;
  pos : pos;
}

(* 类型自己的方法（Lean `Traits.Inherent`）：方法名登记一条（同名的可以有几条，头部不同）。
   head 是 impl 的类型，fn_name 是编译成的函数名（Vec<I64>.sum）。 *)
type inherent = { head : ty; tparams : (string * bool) list; fn_name : string; pos : pos }

(* 类型函数（impl 类型 { … } 里第一个参数不是接收者的函数，例如 Vec.new()）：“类型的名字.函数名”登记一条。
   target 是调用的函数：生成的函数或者内建函数。改名时调用 T.f(…) 已经换成了 target。 *)
type static = { target : string; pos : pos }

type t = {
  items : program;
  type_file : (string, int) Hashtbl.t;  (* 结构体、枚举（加上模块前缀的名字）定义在第几个文件里：consume 只能在那个文件里用 *)
  inherent : (string, inherent) Hashtbl.t;
  prims : (string, prim) Hashtbl.t;
  statics : (string, static) Hashtbl.t;
}

(* 读入之前的空表。大小和以前的全局表一样，遍历的顺序（报错的先后）才不变。 *)
let empty () =
  {
    items = [];
    type_file = Hashtbl.create 16;
    inherent = Hashtbl.create 16;
    prims = Hashtbl.create 64;
    statics = Hashtbl.create 16;
  }
