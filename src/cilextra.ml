open Cil

let longPtrType : typ = TPtr(longType, [])
let ulongPtrType : typ = TPtr(ulongType, [])

let shortType : typ = TInt(IShort, [])
let ushortType : typ = TInt(IUShort, [])

let currentFunction : fundec ref = ref dummyFunDec
let currentFile : file ref = ref dummyFile

(* make a function type (non-vararg with no attributes) *)
let funType (ret_t: typ)
            (args: (string * typ) list) =
  TFun(ret_t,
       Some (List.map (fun (n,t) -> (n, t, [])) args),
       false, [])

let mkSet lv e : stmt =
  mkStmtOneInstr (Set(lv, e, !currentLoc))
let mkCall lvo f args : stmt = 
  mkStmtOneInstr (Call(lvo, f, args, !currentLoc))
let mkAsm attrs tmpls outputs inputs clobs : stmt = 
  mkStmtOneInstr (Asm(attrs, tmpls, outputs, inputs, clobs, !currentLoc))

let rec dropCasts (e: exp) : exp = 
  match e with
      CastE (_, e) -> dropCasts e
    | _ -> e

let castVoidStar e = mkCast e voidPtrType
let voidStarZero = castVoidStar zero

let containsArray t = 
  existsType 
    (function 
	 TArray _ -> ExistsTrue
       | TPtr _ -> ExistsFalse
       | TFun _ -> ExistsFalse
       | _ -> ExistsMaybe) t

