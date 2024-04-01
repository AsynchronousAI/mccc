
val longPtrType : Cil.typ
val ulongPtrType : Cil.typ

val shortType : Cil.typ
val ushortType : Cil.typ

val currentFunction : Cil.fundec ref
val currentFile : Cil.file ref

(** Make a function type (non-vararg with no attributes) *)
val funType : Cil.typ -> (string * Cil.typ) list -> Cil.typ

(** Make a Set instruction *)
val mkSet : Cil.lval -> Cil.exp -> Cil.stmt

(** Make a Call instruction *)
val mkCall : 
  Cil.lval option -> Cil.exp -> Cil.exp list -> Cil.stmt

(** Make a Asm statement *)
val mkAsm : 
  Cil.attributes -> string list -> (string * Cil.lval) list -> 
  (string * Cil.exp) list -> string list -> Cil.stmt

val dropCasts : Cil.exp -> Cil.exp

val castVoidStar : Cil.exp -> Cil.exp
val voidStarZero : Cil.exp

val containsArray : Cil.typ -> bool
