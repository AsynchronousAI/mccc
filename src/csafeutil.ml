open Cil

let compactBlocks = true

let isSome = function Some _ -> true | _ -> false

let typeSigSafe t = typeSigWithAttrs (fun a -> []) t
let typeSigNoAttrs t = typeSigWithAttrs (fun a -> []) t

let doHeader = true     (* Separate Header/Info? *)
let doRTTI = true       (* Enable RTTI support? *)
let doTemporal = true   (* Check temporal errors? *)

(* Use one bit for each capability and never reuse it? *)
let doSingleUseCapability = false  

let optHeader = true    (* Generate Header variables only when necessary? *)

let defaultNeedTsStack = false (* Call push/pop_ts_stack only when necessary?*)
let currentNeedTsStack = ref false

let showGlobals = false
let debugGlobals = false
let debugInstr = false

let showWarnings = ref false

let numPtrAssign = ref 0
let numPtrDeref = ref 0
let numPtrCasts = ref 0
let numBadPtrCasts = ref 0


let strValueField = "_value"
let strInfoField = "_info"

(**** Make new string names *)
let stringId = ref 0 
let newStringName () = 
  incr stringId;
  "__string" ^ (string_of_int !stringId)


(* let csafePrefix = "__csafe_" *)
let csafePrefix = ""
let headerSuffix = "_header"
let infoSuffix = "_info"
let ptrInfoSuffix = "_p" ^ infoSuffix
let structInfoSuffix = "_s" ^ infoSuffix
let unionInfoSuffix = "_u" ^ infoSuffix
let retInfoSuffix = "_ret" 

let headerName name = (csafePrefix ^ name ^ headerSuffix)
let infoName name = (csafePrefix ^ name ^ infoSuffix)
let fieldInfoName fname = (fname ^ infoSuffix)

