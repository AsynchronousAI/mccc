
open Cil
open Pretty

open Cilextra

module H = Hashtbl
module E = Errormsg

let astrHeaderComp = "headercomp"
let astrInfoComp = "infocomp"
let astrInfoShadowComp = "infoshadowcomp"
let astrInfoReturn = "inforeturn"
let astrInfoFun = "infofun"

let astrCompat = "compat"
let astrNoMemSafe = "nomemsafe"
let astrCsafeAlloc = "csafealloc"
let astrCsafeDealloc = "csafedealloc"

(* Should match the macro values defined in cmemsafe.h *)
let invalidTypeId = 0
let nonCastTypeId = 1
let primitiveTypeId = 2
let validTypeId = 3
let firstCastTypeId = 4
let nextCastTypeId = ref 4

(* External Macros about TS *)

let csafeCapValForever = makeGlobalVar "CAPVAL_FOREVER" ulongType
let csafeCapForever = makeGlobalVar "CSAFE_CAP_FOREVER" ulongPtrType
let csafeCapValFrame = makeGlobalVar "CAPVAL_FRAME" ulongType
let csafeCapFrame = makeGlobalVar "CSAFE_CAP_FRAME" ulongPtrType
let csafeCapNever = makeGlobalVar "CSAFE_CAP_NEVER" ulongPtrType

(*** the CHECKERS ***)

let csafeAssert (what: exp) = 
  let fdec = emptyFunction "CSAFE_ASSERT" in
    fdec.svar.vtype <- funType voidType [ ("p",    voidPtrType);
					  ("file", charPtrType);
					  ("line", intType) ];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) [ mkCast what voidPtrType;
				        Const (CStr !currentLoc.file);
				        (integer !currentLoc.line) 
				      ]

let checkNull (what: exp) = 
  let fdec = emptyFunction "CHECK_NULL" in
    fdec.svar.vtype <- funType voidType [ ("p",    voidPtrType);
					  ("file", charPtrType);
					  ("line", intType) ];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) [ castVoidStar what;
				        Const (CStr !currentLoc.file);
				        (integer !currentLoc.line) 
				      ]

let checkUninitialized (what: exp) = 
  let fdec = emptyFunction "CHECK_UNINITIALIZED" in
    fdec.svar.vtype <- funType voidType [ ("blkhdr",    voidPtrType);
					  ("file", charPtrType);
					  ("line", intType) ];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) [ castVoidStar what;
				        Const (CStr !currentLoc.file);
				        (integer !currentLoc.line) 
				      ]


let checkRTTI (p: exp) (p_stype: exp) (p_rtype: exp) : stmt = 
  let fdec = emptyFunction "CHECK_RTTI" in
    fdec.svar.vtype <- funType voidType [ ("p",       voidPtrType);
                                          ("p_stype", longType);
					  ("p_rtype", longType);
					  ("file",    charPtrType);
					  ("line",    intType) ];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) [ p;
                                        mkCast p_stype longType;
					mkCast p_rtype longType;
					Const (CStr !currentLoc.file);
					(integer !currentLoc.line)
				      ]

let checkRTTIAddrOf (p: exp) (p_stype: exp) (p_rtype: exp) (tmptid: exp)
  : stmt = 
  let fdec = emptyFunction "CHECK_RTTI_ADDROF" in
    fdec.svar.vtype <- funType voidType [ ("p",       voidPtrType);
                                          ("p_stype", longType);
					  ("p_rtype", longType);
					  ("tmptid", longType);
					  ("file",    charPtrType);
					  ("line",    intType) ];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) [ p;
                                        mkCast p_stype longType;
					mkCast p_rtype longType;
					mkCast tmptid longType;
					Const (CStr !currentLoc.file);
					(integer !currentLoc.line)
				      ]

let checkPtrArith (p: exp) (e: exp) (link_e: exp)
  (p_stype: exp) (p_ssize: exp) (p_rtype: exp) (p_rblktype: exp) : stmt = 
  let fdec = emptyFunction "CHECK_PTR_ARITH" in
    fdec.svar.vtype <- funType voidType [ ("p",       voidPtrType);
                                          ("e",       longType);
                                          ("link_e",  longType);
                                          ("p_stype", longType);
                                          ("p_ssize", longType);
					  ("p_rtype", longType);
					  ("p_rblktype", longType);
					  ("file",    charPtrType);
					  ("line",    intType) ];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) [ p;
                                        mkCast e longType;
                                        mkCast link_e longType;
                                        mkCast p_stype longType;
                                        mkCast p_ssize longType;
					mkCast p_rtype longType;
					mkCast p_rblktype longType;
					Const (CStr !currentLoc.file);
					(integer !currentLoc.line)
				      ]
let checkFree (p: exp) (base: exp) (cap_ptr: exp) (cap_index: exp) : stmt = 
  let fdec = emptyFunction "CHECK_FREE" in
    fdec.svar.vtype <- funType voidType [ ("p",       voidPtrType);
					  ("base",    voidPtrType);
					  ("cap_ptr", ulongPtrType);
					  ("cap_index", ulongType);
					  ("file",    charPtrType);
					  ("line",    intType) ];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) [ p;
					mkCast base    voidPtrType;
					mkCast cap_ptr ulongPtrType;
					mkCast cap_index ulongType;
					Const (CStr !currentLoc.file);
					(integer !currentLoc.line)
				      ]


let checkTemporal (p: exp) (cap_ptr: exp) (cap_idx: exp) : stmt = 
  let fdec = emptyFunction "CHECK_TEMPORAL" in
    fdec.svar.vtype <- funType voidType [ ("p",       voidPtrType);
					  ("cap_ptr", ulongPtrType);
					  ("cap_idx", ulongType);
					  ("file",    charPtrType);
					  ("line",    intType) ];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) [ p;
					mkCast cap_ptr ulongPtrType;
					mkCast cap_idx ulongType;
					Const (CStr !currentLoc.file);
					(integer !currentLoc.line)
				      ]

let checkSpatial (p: exp) (sz: exp) (base: exp) (size: exp) : stmt = 
  let fdec = emptyFunction "CHECK_SPATIAL" in
    fdec.svar.vtype <- funType voidType [ ("p",    voidPtrType);
					  ("sz",   longType);
					  ("base", voidPtrType);
					  ("size", longType);
					  ("file", charPtrType);
					  ("line", intType) ];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) [ p;
					mkCast sz longType;
					mkCast base voidPtrType;
					mkCast size longType;
					Const (CStr !currentLoc.file);
					(integer !currentLoc.line)
				      ]

let checkSpatialUB (p: exp) (sz: exp) (base: exp) (size: exp) : stmt = 
  let fdec = emptyFunction "CHECK_SPATIAL_UB" in
    fdec.svar.vtype <- funType voidType [ ("p",    voidPtrType);
					  ("sz",   longType);
					  ("base", voidPtrType);
					  ("size", longType);
					  ("file", charPtrType);
					  ("line", intType) ];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) [ mkCast p voidPtrType;
					mkCast sz longType;
					mkCast base voidPtrType;
					mkCast size longType;
					Const (CStr !currentLoc.file);
					(integer !currentLoc.line)
				      ]

let checkSpatialLB (p: exp) (sz: exp) (base: exp) (size: exp) : stmt = 
  let fdec = emptyFunction "CHECK_SPATIAL_LB" in
    fdec.svar.vtype <- funType voidType [ ("p",    voidPtrType);
					  ("sz",   longType);
					  ("base", voidPtrType);
					  ("size", longType);
					  ("file", charPtrType);
					  ("line", intType) ];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) [ mkCast p voidPtrType;
					mkCast sz longType;
					mkCast base voidPtrType;
					mkCast size longType;
					Const (CStr !currentLoc.file);
					(integer !currentLoc.line)
				      ]

let csafeInit () = 
  let fdec = emptyFunction "csafe_init" in
    fdec.svar.vtype <- funType voidType [];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) []

let csafeCleanup () = 
  let fdec = emptyFunction "csafe_cleanup" in
    fdec.svar.vtype <- funType voidType [];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) []
			
let pushTSStack () = 
  let fdec = emptyFunction "csafe_SCS_push" in
    fdec.svar.vtype <- funType voidType [];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) []

let popTSStack () = 
  let fdec = emptyFunction "csafe_SCS_pop" in
    fdec.svar.vtype <- funType voidType [];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) []

let releaseTSHeap (which: exp) = 
  let fdec = emptyFunction "csafe_HCS_release" in
    fdec.svar.vtype <- funType voidType [ ("cap", ulongPtrType) ];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) [ mkCast which ulongPtrType ]

let nextTSHeap = 
  let fdec = emptyFunction "csafe_HCS_allocate" in
    fdec.svar.vtype <- funType ulongPtrType [];
    fdec.svar.vstorage <- Static;
    fdec
		  
(* External functions *)

let callMemset (s: exp) (c: exp) (n: exp) : stmt = 
  let fdec = emptyFunction "memset" in
    fdec.svar.vtype <- funType voidType [ ("s", voidPtrType);
					  ("c", intType);
					  ("n", intType) ];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) [ mkCast s voidPtrType;
					mkCast c intType;
					mkCast n intType
				      ]

let callCalloc (dest: lval) (nmemb: exp) (size: exp) : stmt = 
  let fdec = emptyFunction "calloc" in
    fdec.svar.vtype <- funType intType [ ("nmemb", intType);
					     ("size", intType) ];
    fdec.svar.vstorage <- Static;
    mkCall (Some dest) (Lval(var fdec.svar)) [ mkCast nmemb intType;
					       mkCast size intType
					     ]

let callFree (ptr: exp) : stmt = 
  let fdec = emptyFunction "free" in
    fdec.svar.vtype <- funType voidType [ ("ptr", voidPtrType) ];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) [ mkCast ptr voidPtrType ]


let csafeIncPtrAssign () = 
  let fdec = emptyFunction "CSAFE_INC_PTR_ASSIGN" in
    fdec.svar.vtype <- funType voidType [];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) []

let csafeIncPtrDeref () = 
  let fdec = emptyFunction "CSAFE_INC_PTR_DEREF" in
    fdec.svar.vtype <- funType voidType [];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) []

let csafeIncCalls () = 
  let fdec = emptyFunction "CSAFE_INC_CALLS" in
    fdec.svar.vtype <- funType voidType [];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) []

let csafeIncAllocs () = 
  let fdec = emptyFunction "CSAFE_INC_ALLOCS" in
    fdec.svar.vtype <- funType voidType [];
    fdec.svar.vstorage <- Static;
    mkCall None (Lval(var fdec.svar)) []

