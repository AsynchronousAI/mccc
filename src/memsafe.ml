

(* Cil Analysis Module for MEmory-SAfe transformation *)

open Pretty
open Cil

open Clist

open Cilextra

open Metadata
open Allocinfo
open Updncasts
open Csafeutil

module E = Errormsg
module H = Hashtbl

module CC = Csafechecks
module MU = Markutil


(* Global Definitions *)


(**** Make new types ****)

(* For each new type name, keep track of various versions, usually due
 * to varying attributes *)
let typeNames : (string, alphaTableData ref) H.t = H.create 17

let newTypeName (prefix: string) = fst (newAlphaName typeNames None prefix)

let rec newTypeNameFromType suffix t = 
  let name = baseTypeName t in
  let n = name ^ suffix in
    newTypeName n

(* Make a type name, for use in type defs *)
and baseTypeName = function
  | TNamed (t, _) -> t.tname
  | TBuiltin_va_list _ -> "__builtin_va_list"
  | TVoid(_) -> "void"
  | TInt(IInt,_) -> "int"
  | TInt(IUInt,_) -> "uint"
  | TInt(IShort,_) -> "short"
  | TInt(IUShort,_) -> "ushort"
  | TInt(IChar,_) -> "char"
  | TInt(IUChar,_) -> "uchar"
  | TInt(ISChar,_) -> "schar"
  | TInt(ILong,_) -> "long"
  | TInt(IULong,_) -> "ulong"
  | TInt(ILongLong,_) -> "llong"
  | TInt(IULongLong,_) -> "ullong"
  | TFloat(FFloat,_) -> "float"
  | TFloat(FDouble,_) -> "double"
  | TFloat(FLongDouble,_) -> "ldouble"
  | TEnum (enum, _) -> "enum_" ^ enum.ename
  | TComp (comp, _) ->
      let su = if comp.cstruct then "s_" else "u_" in
      let name = comp.cname in
	su ^ name
  | TFun _ -> "fun"
  | TPtr(t, _) ->
      let name = baseTypeName t in
	"p_" ^ name
  | TArray(t, _, _) ->
      let name = baseTypeName t in
	"a_" ^ name

(* Tables *)

(* We collect here the new file *)
let theFile : global list ref = ref []
let consGlobal (x : global) l = x :: l
				  
let extraGlobInit : stmt clist ref = ref empty

(* Map a string literal to a global array *)
let stringGlobalVarTable : (string, varinfo) H.t = H.create 511

(* Map a variable to its HEADER variable *)
let headerGlobalVarTable : (string, varinfo) H.t = H.create 511
let headerLocalVarTable : (string, varinfo) H.t = H.create 113

(* Map a variable to its INFO variable *)
let infoGlobalVarTable : (string, varinfo) H.t = H.create 511
let infoLocalVarTable : (string, varinfo) H.t = H.create 113

let infoTypeTable : (typsig, typ option) H.t = H.create 113
let infoReturnTypeTable : (typsig, typ option) H.t = H.create 113
let infoFunTypeTable : (typsig, typ) H.t = H.create 113

let metadataComps : (int, compinfo) H.t = H.create 113
let checkMetadataComps : (int, unit) H.t = H.create 113

let functionDescriptors : (int, varinfo) H.t = H.create 113
let definedFunctionIds : (int, bool) H.t = H.create 113
let functionDescriptorInit : global list ref = ref []

(* We need to remember the original function types (pre-fixupType) in
 * order to properly set the type of a compatible function in
 * varStartInput. *)
let originalFunctionTypes : (typsig, typ) H.t = H.create 113

let recordOriginalFunctionType (oldt: typ) (newt: typ) : unit =
  let tsig = typeSigSafe newt in
    if not (H.mem originalFunctionTypes tsig) then
      H.add originalFunctionTypes tsig oldt

let getOriginalFunctionType (t: typ) : typ =
  try
    H.find originalFunctionTypes (typeSigSafe t)
  with Not_found -> t (*E.s (bug "Original function type not stored!")*)


(* This test determines whether a function should be called as a compat
   function -- that is, when calling this function, we will call it directly 
   without adding info variables for arguments or wrapping the return value. 
*) 

let isCompatFunctionType (t: typ) : bool =
  match unrollType t with
    | TFun (rt, args, isva, a) -> begin
        hasAttribute CC.astrCompat a
      end
    | _ -> false

let isCompatFunction (e: exp) : bool = 
  match e with
      Lval (Var v, NoOffset) -> 
	let isGlobinitFunc = 
	  match !(currentFile).globinit with
	      Some gi -> begin
		if v = gi.svar then true else false
	      end
	    | None -> false
        in
	let is_extern = 
          not ((H.mem definedFunctionIds v.vid) || isGlobinitFunc) in
	let is_compat =
          (* hasAttribute CC.astrCompat v.vattr *) 
          isCompatFunctionType v.vtype
        in
        let is_alloc = isAllocFunction v.vname in
        let is_dealloc = isDeallocFunction v.vname in
	  is_extern || is_compat || is_alloc || is_dealloc

    | _ -> false


(* Given a type, return the corresponding type for its info data, if any *)
let rec infoDataType (t: typ) : typ option =
  let ts = typeSigSafe t in
    try 
      H.find infoTypeTable ts
    with Not_found -> 
      let info = 
	match t with
	    TVoid _ -> None (* Some (voidType) *)

	  | TFun (rt, args, _, _) -> 
	      if !E.verboseFlag then
		ignore (E.log "\tinfoDataType for function type: %a\n" 
			  d_type t);
	      None
		(*
		  | TFun (rt, args, _, _)
		  when (infoDataType rt <> None) ||
                  (List.exists (fun (_, t, _) -> infoDataType t <> None)
                  (argsToList args)) ->
		  Some (fixupFunType t)
		*)
	  | TPtr (t', a) -> begin
	      match infoDataType t' with
		  None -> Some infoType
		| Some t'' -> begin
		    let tname = newTypeNameFromType ptrInfoSuffix t' in
		    let infoFields = infoFieldsOfPtrType t in
		    let mcomp =
		      mkCompInfo true tname
			(fun _ ->
			   List.map
			   (fun mk ->
			      let fn, ft = fieldOfMK mk in
				(fn, ft, None, [], !currentLoc))
			   infoFields)
			[Attr(CC.astrInfoComp, [])]
		    in
		      if List.mem MKLink infoFields then
			(* Now we must patch the type of the _link field *)
			let fn, _ = fieldOfMK MKLink in
			  List.iter
			    (fun f ->
			       if f.fname = fn then
				 f.ftype <- TPtr(t'', [])) mcomp.cfields

		      else ();
		        theFile := consGlobal (GCompTag (
					         mcomp, !currentLoc)) !theFile;
		      Some (TComp (mcomp, []))
		  end
	    end

	  | TArray (t', eo, _) -> begin
	      match infoDataType t' with
		  Some t'' -> Some (TArray (t'', eo, []))
		| None -> None
	    end

	  | TComp (ci, _) -> begin
	      if not ci.cdefined then None
	      else begin
		let tname = 
		  if ci.cstruct then newTypeNameFromType structInfoSuffix t 
		  else newTypeNameFromType unionInfoSuffix t
		in
		  (* Only create a stub here to avoid infinite loops for
		     recursive structures. Will be finished in
		     finalizeInfoShadowComp *)
		let newci = 
		  mkCompInfo ci.cstruct tname 
		    (fun _ -> []) 
		    [Attr(CC.astrInfoShadowComp, [])]
		in
		  Some (TComp (newci,[]))
	      end
	    end
	      
	  | TNamed (ti, _) ->
	      infoDataType ti.ttype

	  | _-> 
	      None
      in
	H.add infoTypeTable ts info;
	info

and infoFieldsOfPtrType (t: typ) : mkind list =
  match t with
      TPtr (t', _) ->
	if isSome (infoDataType t') then
	  infoFieldsOrder
	else
	  infoFieldsOrder 
	    (* List.filter (fun x -> x != MKLink) mkFieldsOrder *)

    | _ ->
	E.s (bug "%a is non-pointer type\n" d_type t)


and fixupType (t: typ) : typ =
  if !E.verboseFlag then ignore (E.log "fixupType (%a)\n" d_type t);
  let newt = 
    match t with
	TFun (rt, args, isva, a) -> fixupFunType t
      | TPtr (t', a) -> begin
	  let newt' = fixupType t' in
	    TPtr (newt', a)
	end
      | TNamed (ti, _) ->
          fixupType ti.ttype
      | _ -> t
  in
    newt

and fixupFunType (t: typ) : typ = 
  if !E.verboseFlag then ignore (E.log "fixupFunType (%a)\n" d_type t);
  if isInfoFunType t then t   (* avoid to repeatedly fixup function type *)
  else begin
    let ts = typeSigSafe t in
      try 
	H.find infoFunTypeTable ts
      with Not_found ->
	let newt = 
	  match unrollType t with 
	    | TFun (rt, args, isva, a) -> begin
		let args' = 
		  match args with
		      None -> None
		    | Some argl ->
			let mkInfoArg (vtype,a) =
			  match infoDataType vtype with
			      Some mt ->
				Some (mt,[])
			    | None -> None
			in
			let rec loopFormals (forms: (string * typ * attributes) list) = 
			  match forms with
			      [] -> []
			    | (fname,ftype,fattrs) :: restf -> begin
				let r1 = loopFormals restf in
                                let fname' = 
                                  if fname = "" then "" else infoName fname
                                in
				  match mkInfoArg (ftype,fattrs) with
				      Some (ftype', fattrs') ->
					(fname, fixupType ftype, fattrs) 
					:: ((fname', ftype', fattrs') :: r1)
				    | None -> 
					(fname, fixupType ftype, fattrs) 
					:: r1
			      end
			in
			  Some (loopFormals argl)
		in
		let newa = addAttribute (Attr(CC.astrInfoFun,[])) a in
		let res = 
		  match infoReturnType rt with
		      None -> TFun(rt,args',isva,newa)
		    | Some rt' -> TFun(rt',args',isva,newa)
		in
		  res
	      end
		
	    | _ ->
		E.s (bug "%a is not a function type" d_type t)
	in
	  H.add infoFunTypeTable ts newt;
	  newt
  end

and infoReturnType (t: typ) : typ option = 
  if !E.verboseFlag then ignore (E.log "infoReturnType (%a)\n" d_type t);
  let ts = typeSigSafe t in
    try 
      H.find infoReturnTypeTable ts
    with Not_found -> 
      let info = 
	match unrollType t with
	    TVoid _ -> None 
	  | _ -> begin
	      match infoDataType t with
		  None -> None
		| Some t' -> begin 
		    let tname = newTypeNameFromType retInfoSuffix t in
		    let mcomp = 
		      mkCompInfo true tname
			(fun _ -> 
			   [ (strValueField, t, None, [], !currentLoc);
			     (strInfoField, t', None, [], !currentLoc); ]
			)
			[ Attr(CC.astrInfoReturn, []) ]
		    in
		      theFile := consGlobal (GCompTag 
					       (mcomp, !currentLoc)) !theFile;
		      Some (TComp (mcomp, []))
		  end 
	    end
      in
	H.add infoReturnTypeTable ts info;
	info


let finalizeInfoShadowComp (ci: compinfo) : unit = 
  match infoDataType (TComp (ci, [])) with
      Some (TComp (mci, _)) when mci.cfields = [] ->
	let foldField fld acc = 
	  (* Could be missingFieldName. Need to be fixed later *)
	  match infoDataType fld.ftype with
	      Some t' ->
		(fld.fname ^ infoSuffix, t', None, [], !currentLoc) :: acc
	    | None -> acc
	in
	let fieldSpec = List.fold_right foldField ci.cfields [] in
	let fields = List.map 
		       (fun (fn,ft,fb,fa,fl) ->
			  { fcomp=mci; ftype=ft; fname=fn;
			    fbitfield=fb; fattr=fa; floc=fl})
		       fieldSpec
	in
	  mci.cfields <- fields;

    | _ -> E.s (bug "finalizeInfoShadowComp: comp metadata is not a stub comp\n")

let globalTempVarCounter = ref 0
let getGlobalTempVarName () = 
  globalTempVarCounter := !globalTempVarCounter + 1;
  "__cil_global_tmp" ^ (string_of_int !globalTempVarCounter)
    

let getMetadataOfVar (vi: varinfo) : metadata = 
  let lv = (Var vi, NoOffset) in
  let lvt = typeOfLval lv in
  let blkhdr = 
    if not doHeader then None
    else begin
      try
        Some (mkCast 
                (mkAddrOf (var (H.find headerLocalVarTable vi.vname))) 
                headerPtrType)
      with Not_found -> begin
        try 
          Some (mkCast
                  (mkAddrOf (var (H.find headerGlobalVarTable vi.vname)))
                  headerPtrType)
        with Not_found ->
          (* Missing HEADER variable, maybe due to optimization *)
          None
      end 
    end
  in
  let blktid = 
    if doRTTI then Some (getTypeId (typeOfLval (var vi)))
    else None
  in
  let tid = blktid in
  let base = Some (mkAddrOf (var vi)) in
  let size = Some (SizeOfE (Lval (var vi))) in
  let cap_ptr, cap_index = 
    if doTemporal then begin
      if vi.vglob then 
        Some (Lval (var CC.csafeCapForever)), 
        Some (Lval (var CC.csafeCapValForever))
      else 
        let cap_ptr' = (Lval (var CC.csafeCapFrame)) in
        let cap_index' = (Lval (var CC.csafeCapValFrame)) in
          Some cap_ptr', Some cap_index'
    end
    else
      None, None
  in
  let getAddrOfInfo vi_info = mkAddrOf (var vi_info) in
    (*     match unrollType vi_info.vtype with *)
    (*         TArray _ -> mkAddrOf (Var vi_info, Index(zero, NoOffset)) *)
    (*       | _ -> mkAddrOf (var vi_info) *)
  let link =
    try
      let vi_info = H.find infoLocalVarTable vi.vname in
	Some (getAddrOfInfo vi_info)
    with Not_found -> begin
      try 
	let vi_info = H.find infoGlobalVarTable vi.vname in
	  Some (getAddrOfInfo vi_info)
      with Not_found -> 
	match infoDataType (typeOfLval (var vi)) with
	    Some mt -> 
	      if (!showWarnings) then
	        ignore (E.warn "missing info variable for (%a)\n" 
                        d_lval (var vi));
              Some (voidStarZero)
	  | None -> (* None *) Some (voidStarZero)
    end
  in
  let lvm = mkMetadata ~blktid:blktid ~tid:tid ~base:base ~size:size 
              ~cap_ptr:cap_ptr ~cap_index:cap_index ~link:link blkhdr 
  in
    lvm


let setInfoPointer (dest: lval) (md: metadata) : stmt clist = 
  let t = typeOfLval dest in 
  let infoFields = getFieldsOfInfo t in
  let instrs = 
    List.fold_left
      (fun (acc: instr list) (mk, o, ft) ->
	 Set((addOffsetLval o dest), 
	     (mkCast (mdGet mk "setInfoPointer" md) ft), 
             !currentLoc) 
         :: acc)
      [] 
      infoFields
  in
    single (mkStmt (Instr (List.rev instrs)))

let setHeader (dest: lval) (md: metadata) : stmt clist = 
  let t = typeOfLval dest in
  let headerFields = getFieldsOfHeader t in
  let instrs =
    List.fold_left
      (fun (acc: instr list) (mk, o, ft) ->
         Set((addOffsetLval o dest),
             (mdGet mk "setHeader" md), !currentLoc) :: acc)
      []
      headerFields
  in
    single (mkStmt (Instr (List.rev instrs)))

let setHeaderInit (dest: lval) (md: metadata) : init = 
  let destt = typeOfLval dest in
  let headerFields = getFieldsOfHeader destt in
  let initl = 
    List.fold_left
      (fun (acc: (offset * init) list) (mk, o, ft) ->
         (o, SingleInit (mdGet mk "setHeaderInit" md)) :: acc)
      []
      headerFields
  in
    CompoundInit (destt, List.rev initl)

let rec setInfoAlwaysValid 
  (ve: exp) 
  (mlv:lval) 
  (linksize: exp option) 
  (glob: bool) : stmt clist = 
  
  (* Initialize the info variable with always valid values *)
  match unrollType (typeOf ve) with
      TPtr (t', _) -> begin
	let blkhdr = 
          if doHeader then Some (mkAddrOf (var csafeForeverBlkhdr))
          else None in
	let base = Some (voidStarZero) in
	let size = Some (kinteger IULong (Int32.to_int Int32.max_int)) in 
	let cap_ptr, cap_index = 
          if doTemporal then 
            Some (Lval (var CC.csafeCapForever)),
            Some (Lval (var CC.csafeCapValForever))
          else None, None in
        let blktid = 
          if doRTTI then Some (integer CC.validTypeId) 
          else None in
	let tid = 
          if doRTTI then Some (integer CC.validTypeId) 
          else None in
	let link, dolink =
	  let sz = 
	    match linksize with
		Some sz' -> sz'
	      | None -> one
	  in
	    match infoDataType t' with
		Some t'' -> begin
                  let linkt = TPtr (t'', []) in
		  let tmp, k = 
		    if glob then
		      makeGlobalVar (getGlobalTempVarName ()) linkt,
                      makeGlobalVar (getGlobalTempVarName ()) intType 
		    else 
		      makeTempVar !currentFunction linkt,
                      makeTempVar !currentFunction intType
                  in
                  let alloclink = 
		    single (CC.callCalloc (var tmp) sz (sizeOf t'')) in
                  let initlink = 
                    let init_k = single (mkSet (var k) zero) in
                    let term_cond = BinOp (Eq, Lval (var k), sz, intType) in
                    let term_loop = 
                      mkStmt (
                        If(term_cond, 
                           mkBlock [mkStmt (Break !currentLoc)], 
                           mkBlock [], 
                           !currentLoc)) in
                    let off = Lval (var k) in
                    let set_link = 
		      (setInfoAlwaysValid 
			 (Lval 
                            (Mem (BinOp 
                                    (PlusPI, ve, off, typeOf ve)), 
                             NoOffset)) 
			 (Mem (BinOp 
                                 (PlusPI, Lval (var tmp), off, linkt)), 
                          NoOffset) None glob) in
                    let inc_k = 
                      single (mkSet 
                                (var k)
                                (BinOp (PlusA, 
                                        Lval (var k), 
                                        one, intType))) in
                    let loop_body = 
                      CConsL (term_loop, append set_link inc_k) in
                    let loop_k = 
                      single (mkStmt
                                (Loop (mkBlock (toList loop_body),
                                      !currentLoc, 
                                      None, Some term_loop))) in
                      append init_k loop_k
                  in
		  let dolink = append alloclink initlink in 
		    if glob then begin
		      let lg = GVar (tmp, {init=None}, !currentLoc) in
		      let lgk = GVar (k, {init=None}, !currentLoc) in
			theFile := consGlobal lg !theFile;
			theFile := consGlobal lgk !theFile;
		    end;
		    Some (Lval (var tmp)), dolink
		end
	      | None ->
		  Some voidStarZero, empty
	in
	let lvm = mkMetadata ~blktid:blktid ~tid:tid 
                    ~base:base ~size:size ~cap_ptr:cap_ptr 
                    ~cap_index:cap_index ~link:link blkhdr in
	let initinfo = setInfoPointer mlv lvm in
	  append dolink initinfo
      end
    | _ -> begin
	if (!showWarnings) then
	   ignore (E.warn "Unimplemented: initialization of info variable for non-pointer type (%a)" 
		  d_type (typeOf ve));
	empty
      end
	

(*
 * Generate info structures/variables
 **)
and requireBlockHeader (vi: varinfo) : bool = 
  true

and makeHeaderVarInternal (glob: bool) (vi: varinfo) : 
  varinfo option * metadata =
  if not doHeader then None, mdNone
  else if not (requireBlockHeader vi) then None, mdNone
  else begin
    let found =
      if glob then
        H.mem headerGlobalVarTable vi.vname
      else
        H.mem headerLocalVarTable vi.vname 
    in
      if found then (* Header already exists *) None, mdNone
      else begin
        let vi' = 
          if glob then begin
	    let res = makeGlobalVar (headerName vi.vname) headerType in
	      res
          end
          else begin
	    let f = !currentFunction in
	    let res = makeLocalVar f (headerName vi.vname) headerType in
	  res
          end
        in
        let md = getMetadataOfVar vi in
          if glob then 
	    H.add headerGlobalVarTable vi.vname vi'
          else 
	    H.add headerLocalVarTable vi.vname vi';
          Some vi', md
      end
  end

and makeGlobalHeaderVar (vi: varinfo) : varinfo option * init option = 
  let hdr_vio, md = makeHeaderVarInternal true vi in
    match hdr_vio with
        Some hdr_vi -> 
          Some hdr_vi, Some (setHeaderInit (var hdr_vi) md)
      | None -> None, None

and makeLocalHeaderVar (vi: varinfo) : varinfo option * stmt clist = 
  let hdr_vio, md = makeHeaderVarInternal false vi in
    match hdr_vio with
        Some hdr_vi ->
          Some hdr_vi, setHeader (var hdr_vi) md
      | None ->
          None, empty

and makeInfoVarInternal (glob: bool) (keepLocal: bool) (vi: varinfo) 
  : varinfo option =
  
  if (!E.verboseFlag) then
        ignore (E.log "makeInfoVar (%a)\n" d_lval (var vi));
  match infoDataType vi.vtype with
      Some infot ->
        if (!E.verboseFlag) then 
            ignore (E.log "infoType (%a)\n" d_type infot);
	let vi' = 
	  if glob then begin
	    let res = makeGlobalVar (infoName vi.vname) infot in
	      res
	  end
	  else begin
	    let f = !currentFunction in
	    let res = makeLocalVar f (infoName vi.vname) infot in
	      if not keepLocal then 
		f.slocals <- List.filter (fun x -> 
					    x.vname <> res.vname) f.slocals;
	      res
	  end
	in
	  if glob then
	    H.add infoGlobalVarTable vi.vname vi'
	  else 
	    H.add infoLocalVarTable vi.vname vi';
	  Some vi'
	    
    | None ->
        if !E.verboseFlag then 
            ignore (E.log "infoType (None)\n");
	None

and makeGlobalInfoVar (vi: varinfo) : varinfo option =
  makeInfoVarInternal true true vi

and makeLocalInfoVar (vi: varinfo) : varinfo option =
  makeInfoVarInternal false true vi

and makeFormalInfoVar (vi: varinfo) : varinfo option =
  makeInfoVarInternal false false vi



let rec initInfoLval (lv: lval) : stmt clist = 
  match unrollType (typeOfLval lv) with
      TComp(ci, _) when isInfoComp ci -> 
	let initflds fi = 
	  let lv' = addOffsetLval (Field (fi, NoOffset)) lv in
	    match fi.ftype with
		TComp(fci, _) -> initInfoLval lv'
	      | _ -> begin
                  (* Only initialize cap_ptr field *)
                  match mkOfField fi.fname with
                      MKCapPtr -> 
                        single (mkSet lv' (mkCast zero fi.ftype))
                    | _ -> empty
                end
	in
	  List.fold_left (
	    fun acc fi -> append acc (initflds fi)
	  ) empty ci.cfields
    | _ -> empty

and initInfoVar (vi: varinfo) : stmt clist = initInfoLval (var vi)

let unregisterStmts () : stmt clist = 
  let ismain = (!currentFunction.svar.vname = !Globinit.mainname) in
  let fun_cleanup : stmt clist = 
    if (doTemporal && !currentNeedTsStack) then
      single (CC.popTSStack ())
    else 
      empty
  in
    if ismain then
      CConsR (fun_cleanup, CC.csafeCleanup ())
    else 
      fun_cleanup

(* Various reasons why we might want to check an LV *)
type checkLvWhy = 
    ToWrite of exp
  | ToRead

(* Return just the code to check the memory operation *)
let checkAccess (inlv: safelval) : stmt clist =
  if !E.verboseFlag then
    ignore (E.log "checkAccess (%a)\n" d_safelval inlv);
  let isGlobinitFunc = 
    match !(currentFile).globinit with
	Some gi -> 
          if !currentFunction.svar = gi.svar then true else false
      | None -> false
  in
    (* We do not check memory accesses in the GlobInit function because
       its code is automatically generated to only initialize global
       variables and thus should be free of memory errors.  *)
    if isGlobinitFunc then empty
    else begin
      match inlv.lv with
        | Var vi, off when containsArray vi.vtype ->
	    let rec existIndex off =
	      match off with
	          NoOffset -> false
	        | Field(fi, off1) -> existIndex off1
	        | Index(e, off1) -> true
	    in
	      if not (existIndex off) then empty
	      else begin
	        let p    = mkAddrOf inlv.lv in
	        let sz   = sizeOf inlv.lvt in
	        let base = mdGet MKBase "checkAccess" inlv.lvm in
	        let size = mdGet MKSize "checkAccess" inlv.lvm in
	          numPtrDeref := !numPtrDeref + 1;
	          single (CC.checkSpatial p sz base size)
	      end

        | Mem e, _ ->
            let p, sz =
              (* Can't apply AddrOf on bit fields *)
              let rec containsBitField off =
                match off with
                    NoOffset -> false
                  | Field(fi, off1) -> 
                      if isSome fi.fbitfield then true 
                      else containsBitField off1
                  | Index(e, off1) -> containsBitField off1
              in
              let b,off = inlv.lv in
              if containsBitField off then
                e, sizeOf (typeOfLval (Mem e, NoOffset))
              else
                mkAddrOf inlv.lv, sizeOf inlv.lvt
            in
	    let base = mdGet MKBase "checkAccess" inlv.lvm in
	    let size = mdGet MKSize "checkAccess" inlv.lvm in

            let checkRtti = 
              if not doRTTI then empty
              else begin
                let ptr_t = typeOfLval (Mem e, NoOffset) in
	        let p_stype = getTypeId ptr_t in
	        let p_rtype = mdGet MKPtrRtti "checkAccess" inlv.lvm in
                  match infoDataType inlv.lvt with
                      None -> 
                        (* Won't access the link field, 
                           so no RTTI check needed. *)
                        empty
                    | Some t' -> begin
                        match isInteger p_stype, isInteger p_rtype with
                            Some (i64_stid), Some (i64_rtid) -> begin
                              if Int64.compare i64_stid i64_rtid = 0 then
                                empty
                              else
                                single (CC.checkRTTI p p_stype p_rtype)
                            end
                
                          | _, _ ->
                              single (CC.checkRTTI p p_stype p_rtype) 
                      end
              end
            in

	  (*	  CConsL (CC.checkUninitialized blkhdr,
		  CConsL (CC.checkTemporal p gtsptr cap_index,
		  single (CC.checkSpatial p sz base size))); *)
	      numPtrDeref := !numPtrDeref + 1;
	      if (doTemporal) then begin
	        let gtsptr = mdGet MKCapPtr "checkAccess" inlv.lvm in
	        let cap_index = mdGet MKCapIndex "checkAccess" inlv.lvm in
	          if doHeader then begin
		    (* CConsL (CC.checkUninitialized blkhdr, *)
		    (* 	CConsL (CC.checkTemporal p gtsptr cap_index, *)
		    (* 		single (CC.checkSpatial p sz base size))) *)
		    append checkRtti
                      (CConsL (CC.checkTemporal p gtsptr cap_index,
			       single (CC.checkSpatial p sz base size)))
	          end
	          else begin
                    append checkRtti 
		      (CConsL (CC.checkTemporal p gtsptr cap_index,
			       single (CC.checkSpatial p sz base size)))
	          end
	      end
	      else begin
	        if doHeader then begin
	          (* CConsL (CC.checkUninitialized blkhdr, *)
	          (*       single (CC.checkSpatial p sz base size)); *)
	          append checkRtti 
                    (single (CC.checkSpatial p sz base size));
	        end
	        else begin
                  append checkRtti
	            (single (CC.checkSpatial p sz base size));
	        end
	      end

        | _, _ ->
	    empty
    end

let checkAddrOf (slv : safelval) : metadata * stmt clist =
  if not doRTTI then slv.lvm, empty
  else begin
    let tmptid = makeTempVar !currentFunction intType in
    let tmptid_e = Lval (var tmptid) in
    let fldtid = mdGet MKFldRtti "transExp" slv.lvm in
    let doinit = single (mkSet (var tmptid) fldtid) in
    let checkRtti =
      (* Note: 

         If the check succeeds, the run-time type of (&lv) is set to 
         FldRtti of (slv.lvm). 
         
         If the check fails, we should NOT fail the program at
         this point.  Instead, we just set the run-time type of
         (&lv) to the PRIMITIVE type.

         The above can be done by introducing a temporal
         variable, which is initialized to FldRtti of slv.lvm, and
         can be altered by the check in the case of failure.  Then
         the value of the temporal variable is used as the run-time
         type of (&lv).  

         To make it more efficient, we should introduce an INFO
         variable for each & operation, and use this "tid" attribute
         of that INFO variable where the temporal variable is used.

      *)
      match slv.lv with
        | Var vi, _ -> empty
        | Mem e, NoOffset -> empty
        | Mem e, off -> begin
	    let p_stype =
              getTypeId (typeOfLval (Mem e, NoOffset)) in
	    let p_rtype = mdGet MKPtrRtti "checkAccess" slv.lvm in
              match isInteger p_stype, isInteger p_rtype with
                  Some (i64_stid), Some (i64_rtid) -> begin
                    if Int64.compare i64_stid i64_rtid = 0 then
                      empty
                    else
                      single (CC.checkRTTIAddrOf e p_stype p_rtype tmptid_e)
                  end
                | _, _ ->
                    single (CC.checkRTTIAddrOf e p_stype p_rtype tmptid_e)
          end
    in
    let lvm = mdSet MKPtrRtti slv.lvm tmptid_e in
      lvm, append doinit checkRtti
  end

let checkPointerArithmetic (bop: binop) (se1:safeexp) (se2:safeexp) 
  : metadata * stmt clist = 

    (* se2.ev must be an integer *)
  let lvm, dolvm = 
    let link = 
      if mdHas MKLink se1.em then mdGet MKLink "transExp" se1.em
      else voidStarZero
    in
      if link = voidStarZero then se1.em, empty
      else begin
        let deref_t = typeOfLval (Mem se1.ev, NoOffset) in
        let p_ssize = sizeOf deref_t in
        let check, link_e = 
          if not doRTTI then begin
            match infoDataType deref_t with
                Some t' -> empty, BinOp (Mult, se2.ev, sizeOf t', intType)
              | None -> empty, zero
          end
          else begin 
            let p_stype = getTypeId deref_t in
            let p_rtype = mdGet MKPtrRtti "transExp" se1.em in
            let p_rblktype = mdGet MKBlkRtti "transExp" se1.em in
            let obviousSameType =
              match isInteger p_stype, isInteger p_rtype with
                  Some (i64_stid), Some (i64_rtid) -> begin
                    if Int64.compare i64_stid i64_rtid = 0 then
                      true
                    else begin
                      (* We need to statically check this pointer arithmetic.

                         But it seems like that if we can determine the actual
                         value of the run-time type now, the pointer
                         arithmetic is always valid.  So for the time being,
                         the static checks are omitted.
                      *)
                      true
                    end;
                  end
                | _ -> false
            in
              if obviousSameType then
                empty, BinOp (Mult, se2.ev, p_ssize, intType)
              else begin
                let tmp_v = makeTempVar !currentFunction intType in
                let tmp_e = Lval (var tmp_v) in
                  (single 
                     (CC.checkPtrArith se1.ev se2.ev tmp_e 
                        p_stype p_ssize p_rtype p_rblktype)),
                  tmp_e
              end
          end
        in
        let link_n = 
          mkCast 
            (BinOp (bop, (mkCast link voidPtrType), link_e, voidPtrType)) 
            (typeOf link) 
        in
	  mdSet MKLink se1.em link_n, check
      end
  in
    lvm, dolvm


(* Transform each component of a program *)
let rec transBlock (b: block) : block = 
  if hasAttribute CC.astrNoMemSafe b.battrs then b
  else 
    begin
      let res = 
	toList 
	  (List.fold_left
	     (fun acc s -> append acc (transStmt s)) empty b.bstmts)
      in
	{ bstmts = if compactBlocks then compactStmts res else res;
	  battrs = b.battrs
	}
    end

and transStmt (s: Cil.stmt) : stmt clist = 
  (* Keep the original statement, but maybe modify its kind. This way we
     maintain the labels and we have no need to change the Gotos and the
     cases in the Switch *)
  try
    match s.skind with
      | Break _ | Continue _ | Goto _ -> single s
      | Return (None, l) ->
	  currentLoc := l;
	  CSeq(unregisterStmts (), single s)

      | Return (Some e, l) ->
	  currentLoc := l;
	  let (se, doe) = transExp e in
          let rt = 
            match (!currentFunction).svar.vtype with
                TFun(rt, args, isva, a) -> rt
              | _ -> 
                  E.s (bug "transStmt: (%a) calls a non-funtion\n" 
                         d_stmt s)
          in
	  let newret, donewret = transReturnValue rt se in
	    s.skind <- Instr [];
	    CConsL(s, 
                   CSeq (doe, 
                         CSeq(unregisterStmts (), 
			      CConsR (donewret, 
				      mkStmt (Return(Some newret, l))))))

      | Loop (b, l, lb1, lb2) ->
	  currentLoc := l;
	  s.skind <- Loop (transBlock b, l, lb1, lb2);
	  single s

      | Block b ->
	  s.skind <- Block (transBlock b);
	  single s

      | If(be, t, f, l) ->
	  currentLoc := l;
	  let (se, doe) = transExp (CastE(intType, be)) in
	    s.skind <- Instr [];
	    CConsL(s, 
		   CConsR (doe, 
                           (mkStmt (If(se.ev, 
				       transBlock t, transBlock f, l)))))
      | Instr il ->
	  let b = 
	    List.fold_left (fun acc i -> 
			      append acc (transInstr i)) empty il in
	    s.skind <- Instr [];
	    CConsL (s, b)

      | Switch (e, b, cases, l) ->
	  currentLoc := l;
	  let (se, doe) = transExp (CastE(intType, e)) in
	    s.skind <- Instr [];
	    CConsL(s, 
		   CConsR (doe, 
			   (mkStmt (Switch (se.ev, 
                                            transBlock b, cases, l)))))

      | TryFinally (b, h, l) ->
          currentLoc := l;
          s.skind <- TryFinally (transBlock b, transBlock h, l);
          single s

      | TryExcept (b, (il, e), h, l) ->
          currentLoc := l;
          let b' = transBlock b in
          let il' =
            List.fold_left 
              (fun acc i -> append acc (transInstr i)) 
              empty il in
          let (e', doe) = transExp (CastE(intType, e)) in
            (* Now produce a list of instructions *)
          let il' =
            match compactStmts (toList il' @ toList doe) with
		[] -> []
              | [s] -> begin
		  match s.skind with
                      Instr il' -> il'
		    | _ -> 
                        E.s (unimp "Wrong kind of statement after curing the __except. Try to simplify the __except expression.")
		end
              | _ -> E.s (unimp "too many statements after curing the __except. Try to simplify the __except expression.")
          in
          let h' = transBlock h in
            s.skind <- TryExcept (b', (il', e'.ev), h', l);
            single s


  with e -> begin
    ignore (E.log "transStmt (%s) in %s\n"
	      (Printexc.to_string e) !currentFunction.svar.vname);
    single (mkStmtOneInstr 
	      (dInstr 
		 (dprintf "csafeerror_statement(%a)" d_stmt s) !currentLoc))
  end

and transReturnValue (rt: typ) (se: safeexp) : exp * stmt clist = 
  match rt with
      TComp(ci, _) when isInfoReturnComp ci -> begin
	let do_prof = 
	  numPtrAssign := !numPtrAssign + 1;
	  single (CC.csafeIncPtrAssign ())
	in
	let tmp = makeTempVar !currentFunction rt in
        let vfld = getCompField ci strValueField in
        let ifld = getCompField ci strInfoField in
	let voff = Field(vfld, NoOffset) in
	let ioff = Field(ifld, NoOffset) in
        let tmp_vlv = Var tmp, voff in
        let tmp_ilv = Var tmp, ioff in
	let setval = single (mkSet tmp_vlv se.ev) in
	let setinfo = 
	  let mdest = tmp_ilv in
	    match se.emlv with
		Some mlv -> single (mkSet mdest (Lval mlv))
	      | None -> begin
		  if (isInfoType ifld.ftype) then 
		    setInfoPointer mdest se.em 
		  else begin
		    let link = 
		      mdGet MKLink "transStmt" se.em in
		      if (link <> voidStarZero) then
			single (mkSet mdest link)
		      else
			E.s (bug "Expecting link field\n")
                  end
		end
	in
	  (Lval (var tmp), append do_prof (CSeq (setinfo, setval)))
      end
    | _ -> (se.ev, empty)

and doAssign (slv: safelval) (se: safeexp) : stmt clist = 
  let do_prof = 
    if isPointerType slv.lvt then begin
      numPtrAssign := !numPtrAssign + 1;
      single (CC.csafeIncPtrAssign ())
    end
    else empty in
  let setval = single (mkSet slv.lv (mkCast se.ev slv.lvt)) in
  let setinfo =
    let link = 
      if mdHas MKLink slv.lvm then mdGet MKLink "doAssign" slv.lvm
      else voidStarZero 
    in
      if link <> voidStarZero then begin
	match slv.lvmlv, se.emlv with
	    Some lvmlv, Some emlv 
	      when (typeSigSafe (typeOfLval lvmlv) =
		      typeSigSafe (typeOfLval emlv)) -> 
		single (mkSet lvmlv (Lval emlv))
	  | _, _ ->
	      let mdest = mkMem link NoOffset in
		if isPointerType slv.lvt then 
		  setInfoPointer mdest se.em
		else begin
		  let elink = mdGet MKLink "doAssign" se.em in
		    if elink <> voidStarZero then
		      single (mkSet mdest 
                                (mkCast elink (typeOfLval mdest)))
		    else 
		      E.s (bug "Expecting link field\n");
		end
      end
      else empty
  in
    append do_prof (append setinfo setval)

and transInstr (ins: instr) : stmt clist = 
  if debugInstr then ignore (E.log "Checking %a\n" d_instr ins);
  try
    match ins with 
      | Set (lv, e, l) ->
	  currentLoc := l;
	  let slv, dolv = transLval true lv in
	  let (se, doe) = transExp e in 
	    (* checking code for updating info *)
	  let check = checkAccess slv in
          let doset = doAssign slv se in
	    append dolv
	      (append doe
		 (append check doset))

      | Call (vio, f, args, l) -> begin
	  currentLoc := l;
	  (* Check if this is a call to the globinit function.  If yes,
	     replace the call with an empty statement. The call will be
	     added later. *)
	  let isGlobinitFunc = 
	    match !(currentFile).globinit with
		Some gi -> begin
		  match f with
		      Lval lv -> begin
			match lv with
			    Var vi, NoOffset ->
			      if vi = gi.svar then true else false
			  | _ -> false
		      end
		    | _ -> false
		end
	      | None -> false
	  in
	    if isGlobinitFunc then begin
	      !(currentFile).globinitcalled <- false;
	      empty
	    end
	    else begin
	      let (ft, f', dof) =
		match f with
		    Lval lv -> begin
		      let slv, dolv = transLval true lv in
			(* We must check this call if this is a function
			   pointer, or if we are calling a function with
			   more arguments than the prototype specifies
			*)
		      let check = checkAccess slv in
		      let dof = append dolv check in
			(slv.lvt, Lval slv.lv, dof)
		    end
		  | _ -> E.s (unimp "Unexpected function expression\n")
	      in

	      let allocinfo, isdeallocate =
		match f' with
		    Lval(Var vf, NoOffset) -> 
		      getAllocInfo vf.vname, isDeallocFunction vf.vname
		  | _ -> 
		      None, false
	      in
		if isSome allocinfo then 
		  (* we are calling an allocation function *)
		  match allocinfo with
		      Some ai -> callAllocFunction ai vio f' args
		    | None -> E.s (bug "isSome disagrees with None");
		else if isdeallocate then
		  (* we are calling a deallocation function.
		     In this case vio has no meaning *)
		  append dof (callDeallocFunction f' args)
		else if isCompatFunction f' then
		  (* If we're calling a compat library function, we want to 
		     call directly *)
		  append dof (callCompatFunction vio f' args)
		else 
                  append dof (callFatFunction vio f' ft args)
	    end
	end

      | Asm(attrs, tmpls, outputs, inputs, clobs, l) as ins -> 
	  currentLoc := l;
	  ignore (warn "transformation of Asm is unimplemented\n"); 
	  single (mkAsm attrs tmpls outputs inputs clobs)
	    
  with e -> begin
    ignore 
      (E.log "transInstr (%s):%a (in %s)\n"
	 (Printexc.to_string e) d_instr ins !currentFunction.svar.vname);
    single (mkStmtOneInstr 
	      (dInstr (dprintf "csafeerror_instruction(%a) at %t" 
			 d_instr ins d_thisloc) !currentLoc))
  end

and callFatFunction (rlv: lval option) 
  (f: exp) (ft: typ) (args: exp list) : stmt clist = 
  
  let (ftret, ftargs, isva), noProto = 
    match unrollType ft with
	TFun(fret, fargs, isva, al) ->
	  (fret, fargs, isva), hasAttribute "missingproto" al
      | _ -> E.s (unimp "call of a non-function: %a @!: %a\n"
		    d_plainexp f d_plaintype ft)
  in
  let (args', doargs) = 
    let rec doArgs restargs restargst = 
      match restargs, restargst with
	  [], [] -> [], empty
(* 	| a :: resta, [] ->  begin *)
	| a :: resta, _ ->  begin
	    let (sa, doa) = transExp a in
	    let resta', doresta = doArgs resta [] in
	      if sa.em == mdNone then
		sa.ev :: resta', append doa doresta
              else begin
		match sa.emlv with
		    Some mlv ->
		      sa.ev :: (Lval mlv :: resta'),
		      append doa doresta
		  | None ->
		      let tvar = makeTempVar !currentFunction sa.et in
		      let tlv = var tvar in
		      let tmlv =
			match makeLocalInfoVar tvar with
			    Some vi' -> var vi'
			  | None ->
			      E.s (bug "(%a) no info variable\n"
				     d_lval tlv)
		      in
			(* Including checks in "transExp a" *)
		      let doset = transInstr (Set(tlv,a,!currentLoc)) in
			Lval tlv :: (Lval tmlv :: resta'),
			append doa (append doset doresta)
	      end
	  end
(* 	| a :: resta, (_, ft, _) :: restt -> *)
(* 	    let (sa', doa') = transExp a in *)
(* 	    let (sa, doa) = sa', empty in  (\* castTo sa' ft *\) *)
(* 	    let hdas, dohdas, restt' =  *)
(* 	      if sa.em == mdNone then  *)
(* 		[sa.ev], empty, restt *)
(*               else begin *)
(* 		match restt with *)
(* 		    [] ->  *)
(*                       E.s (bug "expect info type in function prototype\n"); *)
(* 		  | infot :: restt'' -> begin *)
(* 		      match sa.emlv with *)
(* 			  Some mlv -> *)
(* 			    sa.ev :: [Lval mlv],  *)
(* 			    empty, restt'' *)
(* 			| None -> *)
(* 			    let tvar = makeTempVar !currentFunction sa.et in *)
(* 			    let tlv = var tvar in *)
(* 			    let tmlv =  *)
(* 			      match makeLocalInfoVar tvar with *)
(* 				  Some vi' -> var vi' *)
(* 				| None -> *)
(* 				    E.s (bug "(%a) no info variable\n" *)
(* 					   d_lval tlv) *)
(* 			    in *)
(* 			      (\* Including checks in "transExp a" *\) *)
(* 			    let doset = transInstr (Set(tlv,a,!currentLoc)) in *)
(* 			      Lval tlv :: [Lval tmlv], doset, restt'' *)
(* 		    end *)
(* 	      end *)
(* 	    in *)
(* 	    let (resta', doresta) = doArgs resta restt' in *)
(* 	      hdas @ resta',  *)
(*               append doa'  *)
(*                 (append doa  *)
(*                    (append (dohdas doresta))) *)
                
	| _ -> 
            E.s (unimp "too few argument in call to %a" 
                   d_exp f)
    in
      doArgs args (argsToList ftargs)
  in
  let finishcall = 
    match rlv with
	None -> single (mkCall None f args')
      | Some destlv -> begin
	  let tmptype = ftret in
	    (* Always put the result of the call in a
	       temporary variables so that the actual store
	       into the destination occurs with a Set and the
	       right checks are inserted *)
	  let tmp = makeTempVar !currentFunction tmptype in
	    (* Now do the call itself *)
	  let thecall =
	    single (mkCall (Some (var tmp)) f args') in
	  let aftercall = 
	    if isInfoReturnType tmptype then begin
	      let ci = 
		match tmptype with
		    TComp(ci, _) -> ci
		  | _ -> E.s (bug "(%a) not a comp\n" d_type tmptype);
	      in
	      let voff = Field(getCompField ci strValueField, NoOffset) in
	      let ioff = Field(getCompField ci strInfoField, NoOffset) in
              let tmp_vlv = Var tmp, voff in
              let tmp_ilv = Var tmp, ioff in
	      let sdestlv, dodest = transLval true destlv in
              let se = 
                let ev = Lval tmp_vlv in
                let et = typeOf ev in
                let em = 
                  if isPointerType sdestlv.lvt then
                    readFieldsOfInfo (typeOfLval tmp_ilv) tmp_ilv
                  else 
                    mdSet MKLink mdNone (Lval tmp_ilv)
                in
                  mkSafeexp ev et em None
              in
	      let doset = doAssign sdestlv se in
		append dodest doset
	    end
	    else 
	      (* Now use transInstr to do the code after Call properly *)
	      transInstr (Set(destlv, Lval (var tmp), !currentLoc)); 
	  in
	    (* Now put them together *)
	    append thecall aftercall;
	end
  in
    append doargs finishcall

and callCompatFunction (rlv: lval option) (f: exp) (args: exp list) 
  : stmt clist = 
  (* Process each argument *)
  if (!E.verboseFlag) then
        ignore (E.log "calling external function: %a\n" d_exp f);

  let rec processArgs (args: exp list) : (safeexp list) * (stmt clist) = 
    match args with
	arg :: rest ->
	  let (sarg, doa1) = transExp arg in
	  let (srest, doa2) = processArgs rest in
	    sarg :: srest, append doa1 doa2
      | [] -> [], empty
  in
  let (sargs, doa) = processArgs args in
  let dataargs = List.map (fun sarg -> sarg.ev) sargs in
    (* Now call the function, storing the data portion of the return
       value if necessary. *)
  let docall = 
    match rlv with
	Some lv -> 
	  let ftype = typeOf f in
	  let rtype = 
	    match ftype with
		TFun (rt, _, _, _) -> rt
	      | _ -> E.s (bug "callCompatFunction: function type expected\n")
	  in
	  let tmp = makeTempVar !currentFunction rtype in
	    (*  Need wrapper functions
		let doset = 
                transInstr (Set (lv, Lval (var tmp), !currentLoc)) in
	    *)
	  let init_hdr = empty in
	    (* 	  let _, init_hdr = makeLocalHeaderVar tmp in *)
	  let initinfo = 
	    match makeLocalInfoVar tmp with
		Some tmp_info -> begin
		  setInfoAlwaysValid (Lval (var tmp)) (var tmp_info) None false
		end
	      | None -> empty
	  in

	  let doset = transInstr (Set(lv, Lval (var tmp), !currentLoc)) in 
	    append init_hdr 
	      (CConsL (mkCall (Some (var tmp)) f dataargs, 
		       (append initinfo doset)))

      | None -> 
	  single (mkCall None f dataargs)
  in
    append doa docall

(*********** Handle Allocation ****************)
and callAllocFunction (ai: allocInfo) (dest: lval option) 
  (f: exp) (args: exp list) : stmt clist = 
  (* Process each argument *)
  let rec processArgs (args: exp list) : (safeexp list) * (stmt clist) = 
    match args with
	arg :: rest ->
	  let (sarg, doa1) = transExp arg in
	  let (srest, doa2) = processArgs rest in
	    sarg :: srest, append doa1 doa2
      | [] -> [], empty
  in
  let (sargs, doa) = processArgs args in
  let dataargs = List.map (fun sarg -> sarg.ev) sargs in
  let a_size = ai.aiGetSize dataargs in
  let docall =
    match dest with 
      | None -> 
	  single (mkCall None f dataargs)

      |	Some destlv ->
	  let destt, baset, linkt_o, baselinkt_o = 
	    let t = typeOfLval destlv in
	      match unrollType t with
		  TPtr(t', _) -> begin 
		    match infoDataType t' with
			Some t'' -> t, t', Some (TPtr(t'', [])), Some t''
		      | None -> t, t', None, None
		  end
		| _ -> 
		    E.s (warn "Allocation assigned to non-pointer (%a)\n" 
			   d_type t);
		    voidPtrType, voidType, Some voidPtrType, Some voidType
	  in
          let hvar = makeTempVar !currentFunction headerPtrType in
          let hlv = var hvar in
	  let tvar = makeTempVar !currentFunction destt in
	  let tlv = var tvar in
	  let tmlv = 
	    match makeLocalInfoVar tvar with
		Some vi' -> var vi'
	      | None -> E.s (bug "(%a) no info variable\n" d_lval tlv)
	  in
	    (* 	  let thecall = single (mkCall (Some tlv) f dataargs) in *)

	  let thecall, alloclink, link = 
	    match linkt_o, baselinkt_o with
		None, _ -> 
                  let newsize = 
                    if doHeader then
                      BinOp(PlusA, a_size, SizeOf(headerType), ulongType) 
                    else a_size
                  in
                  let do_alloc = 
                    single (mkCall
                              (Some hlv) f 
                              (ai.aiNewSize newsize dataargs)) in
                  let do_setdest = 
                    if doHeader then
                      single (mkSet tlv
                                (mkCast 
                                   (BinOp(PlusPI,
                                          mkCast (Lval hlv) charPtrType,
                                          SizeOf(headerType),
                                          charPtrType)
                                   ) destt))
                    else
                      single (mkSet tlv
                                (mkCast (Lval hlv) destt))
                  in
                  let thecall' = append do_alloc do_setdest in
                    thecall', empty, Some voidStarZero

	      | Some linkt, Some baselinkt ->
		  let a_count = BinOp(Div, a_size, sizeOf baset, ulongType) in
		  let lvar = makeTempVar !currentFunction linkt in
		  let llv = var lvar in
		  let lsize = 
		    BinOp(PlusA, a_size,
			  BinOp(Mult, a_count, sizeOf baselinkt, ulongType),
			  ulongType) in
                  let newsize = 
                    if doHeader then
                      BinOp(PlusA, lsize, SizeOf(headerType), ulongType) 
                    else lsize
                  in
                  let do_alloc = 
                      single (mkCall 
                                (Some hlv) f 
                                (ai.aiNewSize newsize dataargs)) in
                  let do_setdest = 
                    if doHeader then
                       (single (mkSet tlv
                                  (mkCast 
                                     (BinOp(PlusPI,
                                            mkCast (Lval hlv) charPtrType,
                                            SizeOf(headerType),
                                            charPtrType)
                                     ) destt)))
                    else
                      single (mkSet tlv
                                (mkCast (Lval hlv) destt))
                  in
                  let thecall' = append do_alloc do_setdest in
                  let do_setlink = 
		    single (mkSet llv 
			      (mkCast (BinOp(PlusPI, 
					     mkCast (Lval tlv) charPtrType,
					     a_size, charPtrType)
				      ) linkt))
                  in
		    thecall', do_setlink, Some (Lval llv)
	      | Some _, None -> E.s (bug "missing base type for link\n")
	  in

	  (*  let a_size = ai.aiGetSize dataargs in *)
	  (*  let a_count = BinOp(Div, a_size, sizeOf baset, ulongType) in *)
	  (*  let alloclink, link =  *)
	  (*    match linkt_o, baselinkt_o with *)
	  (* 	    None, _ -> empty, Some voidStarZero *)
	  (* 	  | Some linkt, Some baselinkt -> *)
	  (* 	     let lvar = makeTempVar !currentFunction linkt in *)
	  (* 	     let llv = var lvar in *)
	  (* 	     let lsize =  *)
	  (* 	       BinOp(Mult, a_count, sizeOf baselinkt, ulongType) in  *)
	  (* 		 let doalloc = *)
	  (* 		   single (mkCall (Some llv) f  *)
	  (* 			   (ai.aiNewSize lsize dataargs)) *)
	  (* 		 in *)
	  (* 		   doalloc, Some (Lval llv) *)
	  (* 	  | Some _, None -> E.s (bug "missing base type for link\n") *)
	  (*  in *)

	  let blktid = if doRTTI then Some (getTypeId baset) else None in
          let tid = blktid in
          let blkhdr = if doHeader then Some (Lval hlv) else None in
          let base = Some (Lval tlv) in
          let size = Some a_size in
          let cap_ptr, cap_index, allocateCap = 
            if not doTemporal then None, None, empty 
            else begin
              if not doSingleUseCapability then begin
                let cap_ptr_var = 
                  makeTempVar !currentFunction ulongPtrType in
                let cap_p = Lval (var cap_ptr_var) in
                  Some cap_p, 
                  Some (Lval (mkMem cap_p NoOffset)),
                  single (mkCall (Some (var cap_ptr_var)) 
                            (Lval(var CC.nextTSHeap.svar)) [])
              end
              else begin
                let cap_var = makeTempVar !currentFunction ulongPtrType in
                let cap_index' = mkCast (Lval (var cap_var)) ulongType in
                  None, Some cap_index',
                  single (mkCall (Some (var cap_var))
                            (Lval(var CC.nextTSHeap.svar)) [])
              end
            end
          in
	  let lvm = mkMetadata ~blktid:blktid ~tid:tid 
                      ~base:base ~size:size ~cap_ptr:cap_ptr 
                      ~cap_index:cap_index ~link:link blkhdr in

	  let init_hdr = 
            if doHeader then setHeader (mkMem (Lval hlv) NoOffset) lvm
            else empty in
	  let initinfo = setInfoPointer tmlv lvm in
            
	  let doset = transInstr (Set(destlv, Lval tlv, !currentLoc)) in 
	    append thecall 
	      (append alloclink
                 (append allocateCap
                    (append init_hdr
		       (append initinfo doset))))
  in
    append doa docall

(*********** Handle Deallocation ****************)
and callDeallocFunction (f: exp) (args: exp list) : stmt clist = 
  (* Process each argument *)
  let sarg, doa = 
    match args with
      | [] ->
          E.s (bug "too few deallocation arguments\n");
      | [ arg ] -> 
          transExp arg
      | _ -> 
          E.s (bug "too many deallocation arguments\n");
  in
  let dofreeNormal = 
    let base = mdGet MKBase "callDeallocFunction" sarg.em in
    let docheck, releaseCap = 
      if not doTemporal then empty, empty
      else begin
        let cap_ptr = mdGet MKCapPtr "callDeallocFunction" sarg.em in
        let cap_index = mdGet MKCapIndex "callDeallocFunction" sarg.em in
        let docheck' = single (CC.checkFree sarg.ev base cap_ptr cap_index) in
        let dorelease' = 
          if (doSingleUseCapability) then
            single (CC.releaseTSHeap cap_index)
          else
            single (CC.releaseTSHeap cap_ptr)
        in
          docheck', dorelease'
      end
    in
    let toBeFreed = 
      if doHeader then
        (mkCast 
           (BinOp(MinusPI,
                  mkCast sarg.ev charPtrType,
                  SizeOf(headerType),
                  charPtrType))
           voidPtrType)
      else  (mkCast sarg.ev voidPtrType)
    in
    let thecall = single (mkCall None f [toBeFreed]) in
      append docheck (append releaseCap thecall)
  in
  let dofreeNull = single (mkCall None f [sarg.ev]) in
  let isNullExp = BinOp (Eq, sarg.ev, voidStarZero, intType) in
  let dofree = 
    single (mkStmt (
              If( isNullExp, 
                  mkBlock (toList dofreeNull), 
                  mkBlock (toList dofreeNormal), !currentLoc)))
  in
    append doa dofree


(* After processing an expression, we create its type, a list of
   instructions that should be executed before this exp is used, and a
   replacement exp *)
and transExp (e: exp) : safeexp * stmt clist = 
  try
    if !E.verboseFlag then ignore (E.log "transExp (%a)\n" d_exp e);
    let res_e, res_s = 
      match e with
	| Const (CStr s) ->
	    let strt = TPtr(TInt(IChar, []), []) in
	      stringLiteral s strt

	| Const c -> 
	    (mkSafeexp e (typeOf e) mdNone None), empty

	| Lval lv -> begin
	    let slv, dolv = transLval true lv in
	    let check = checkAccess slv in
	    let e' = Lval slv.lv in
	    let em = 
	      match infoDataType slv.lvt with
		  None -> mdNone
		| Some mt ->
                    let link = 
                      if mdHas MKLink slv.lvm then 
                        mdGet MKLink "transExp" slv.lvm 
                      else voidStarZero
                    in
                      if link <> voidStarZero 
                      then begin
                        let deref = mkMem link NoOffset in
                          readFieldsOfInfo mt deref
                      end
		      else begin
		        ignore (E.log "TransExp lv=%a lvm=%a" 
                                  d_lval slv.lv d_metadata slv.lvm);
		        E.s (bug "Missing 'link' metadata\n")
                      end
	    in
	      (mkSafeexp e' (typeOf e') em slv.lvmlv), append dolv check
	  end

	| AddrOf lv ->
	    let slv, dolv = transLval false lv in
	      (* Check that variables whose address is taken are flagged
		 as such, or are globals *)
	      (match slv.lv with
		   (Var vi, _) when not vi.vaddrof && not vi.vglob ->
		     E.s (bug "addrof not set for %s (addrof)\n" vi.vname)
		 | _ -> ());

              let lvm, dolvm = checkAddrOf slv in
	      let e' = mkAddrOf slv.lv in
		(mkSafeexp e' (typeOf e') lvm None), append dolv dolvm
		  
	| StartOf lv -> begin
	    let slv, dolv = transLval false lv in
	      ( match slv.lv with
		    (Var vi, _) when not vi.vaddrof && not vi.vglob ->
		      E.s (bug "addrof not set for %s (addrof)\n" vi.vname)
		  | _ -> ());
	      let slv0, dolv0 = arrayToIndexZero slv in
              let lvm, dolvm = checkAddrOf slv0 in
	      let e' = mkAddrOf slv0.lv in
		(mkSafeexp e' (typeOf e') slv0.lvm None, 
                 append dolv (append dolvm dolv0))
	  end

	| CastE (t', e') when (isPointerType t' && isZero e') -> begin
	    let ev = CastE (t', e') in
	    let em = 
	      let mt = 
		match infoDataType t' with
		    Some mt' -> mt'
		  | None -> E.s (bug "Pointer type has no info\n");
	      in
	      let infoFields = getFieldsOfInfo mt in
		List.fold_left 
		  (fun (acc: metadata) (mk, o, ft) -> mdSet mk acc zero)
		  mdNone
		  infoFields
	    in
	      mkSafeexp ev (typeOf ev) em None, empty
	  end

	| CastE (t', e') -> begin
	    let (se', doe') = transExp e' in
	    let se1, doe1 = castTo se' t' in
	      se1, append doe' doe1
	  end

	| UnOp (uop, e1, restyp) ->
	    let se1, doe1 = transExp e1 in
	      (* The result is never a pointer. *)
	    let e' = UnOp (uop, se1.ev, restyp) in
	      (mkSafeexp e' (typeOf e') mdNone None), doe1

	| BinOp (bop, e1, e2, restyp) -> begin
	    let se1, doe1 = transExp e1 in
	    let se2, doe2 = transExp e2 in
	      match bop with
		| PlusPI | MinusPI | IndexPI ->
                    let lvm, check = checkPointerArithmetic bop se1 se2 in
		      (* e2 must be an integer *)
		    let e' = BinOp (bop, se1.ev, se2.ev, restyp) in
		      (mkSafeexp e' (typeOf e') lvm None), 
		      (append doe1 (append doe2 check))
		| _ ->
		    let e' = BinOp (bop, se1.ev, se2.ev, restyp) in
		      (mkSafeexp e' (typeOf e') mdNone None), append doe1 doe2
	  end

	| SizeOf (t) ->
	    (mkSafeexp (SizeOf t) !typeOfSizeOf mdNone None), empty

	| SizeOfE (Lval lv) ->
	    let slv, _ = transLval false lv in
	      (* Drop all side-effects from this SizeOf, including the
		 checking of safety of the expression. *)
	      (mkSafeexp (SizeOfE (Lval slv.lv)) !typeOfSizeOf mdNone None), 
	      empty

	| SizeOfStr s ->
	    (mkSafeexp (SizeOfStr s) !typeOfSizeOf mdNone None, empty)

	| SizeOfE (e') ->
	    let se', doe' = transExp e' in
	      (* Drop all side-effects from this SizeOf, including the
		 checking of safety of the expression. *)
	      (mkSafeexp (SizeOfE se'.ev) !typeOfSizeOf mdNone None), empty

	| AlignOf (t) ->
	    (mkSafeexp (AlignOf t) !typeOfSizeOf mdNone None), empty

	| AlignOfE (Lval lv) ->
	    let slv, _ = transLval false lv in
	      (* Drop all side-effects from this AlignOf, including the
		 checking of safety of the expression. *)
	      (mkSafeexp (AlignOfE (Lval slv.lv)) !typeOfSizeOf mdNone None), 
	      empty

	| AlignOfE (e') ->
	    let se', doe' = transExp e' in
	      (* Drop all side-effects from this AlignOf, including the
		 checking of safety of the expression. *)
	      (mkSafeexp (AlignOfE se'.ev) !typeOfSizeOf mdNone None), empty

    in
      res_e, res_s

  with exc -> begin
    ignore (E.log "transExp (%s): %a in %s\n"
	      (Printexc.to_string exc) d_exp e 
	      !currentFunction.svar.vname);
    (mkSafeexp e (typeOf e) mdNone None, empty)
  end

and castTo (se: safeexp) (t: typ) : safeexp * stmt clist = 
  if !E.verboseFlag then
    ignore (E.log "castTo (%a, %a)\n" d_safeexp se d_type t);

  let em = 
    let mto = infoDataType t in
      match mto with
	  None -> mdNone
	| Some mt ->
	    if isInfoType mt then begin
              (* Copy all metadata from se.em, 
                 but should also fix the field types *)
              let infoFields = getFieldsOfInfo mt in
                List.fold_left
                  (fun (acc: metadata) (mk, o, ft) ->
	             mdSet mk acc (mkCast (mdGet mk "castTo" acc) ft))
                  se.em
                  infoFields
            end
	    else begin
	      if (!showWarnings) then
	          E.s (E.warn "castTo %a with meta data lost\n" d_type t);
	      mdNone
	    end
  in
  let ev = mkCast se.ev t in
    ignore (isAllowedCast (typeOf se.ev) t);
    mkSafeexp ev (typeOf ev) em None, empty

(* Check that variables whose address is taken are flagged
   as such, or are globals *)
and arrayToIndexZero (slv: safelval) : safelval * stmt clist = 
  match unrollType slv.lvt with
    | TArray (elemt, leno, _) ->
	let indexZero = Index (zero, NoOffset) in
	let lv = addOffsetLval indexZero slv.lv in
        let lvm' = 
  	  let link = 
            let link' =
	      if mdHas MKLink slv.lvm then 
	        mdGet MKLink "arrayToIndexZero" slv.lvm 
              else voidStarZero
            in
	      if link' <> voidStarZero then begin
		let deref = mkMem link' NoOffset in
		  (mkAddrOf (addOffsetLval indexZero deref))
	      end
              else voidStarZero
	  in
            mdSet MKLink slv.lvm link
        in
        let lvm = 
          if doRTTI then 
	    let fldtid = (getTypeId (typeOfLval lv)) in
              mdSet MKFldRtti lvm' fldtid 
          else lvm'
        in
	  (mkSafelval lv (typeOfLval lv) lvm None, empty)
    | _ -> 
	E.s (bug "arrayToIndexZero: expect an array type, got %a\n" 
               d_type slv.lvt)

and stringLiteral (s: string) (strt: typ) : safeexp * stmt clist =
  let stringToInit (s: string) : (offset * init) list = 
    let chars = 
      let rec allChars i acc = 
	if i < 0 then acc
	else allChars (i - 1) ((String.get s i) :: acc)
      in
	allChars (-1 + String.length s) [Char.chr 0] 
    in
    let _, initl' = 
      List.fold_left 
	(fun (idx, acc) chr -> 
	   (idx + 1, (Index(integer idx, NoOffset), 
		      SingleInit(Const(CChr chr)))
	      :: acc))
	(0, [])
	chars
    in
      List.rev initl'
  in
  let se, s1 = 
    let gvar = 
      try 
	H.find stringGlobalVarTable s
      with Not_found -> begin
	(* Make a global variable that stores this one *)
	let l = 1 + String.length s in
	let newt = TArray(charType, Some (integer l), []) in
	let gvar = makeGlobalVar (newStringName ()) newt in
	  gvar.vstorage <- Static;
	  H.add stringGlobalVarTable s gvar;
	  let init = CompoundInit (newt, stringToInit s) in
	    theFile := consGlobal (GVar (gvar, 
					 {init=Some init}, 
					 !currentLoc)) !theFile;

	    ( match makeGlobalHeaderVar gvar with
		  Some vi', init_hdr ->
		    let mg = GVar (vi', {init=init_hdr}, !currentLoc) in
                      (* extraGlobInit := append init_hdr !extraGlobInit; *)
		      theFile := consGlobal mg !theFile;
		| None, init_hdr ->
                    (* extraGlobInit := append init_hdr !extraGlobInit; *)
                    ()
            );
	    gvar
      end
    in
    let res = StartOf (Var gvar, NoOffset) in
    let sres, dores = transExp res in
      sres, dores
  in
  let se', s2 = castTo se strt in
    se', append s1 s2

(* Given an lvalue, generate all the stuff needed to construct a pointer
   to it. If foraccess is false then we will not read/write through this
   one, but will use it for & or for sizeof. *)
and transLval (foraccess:bool) (b, off) 
  : safelval * stmt clist = 
  try
    if !E.verboseFlag then ignore (E.log "transLval (%a)\n" d_lval (b,off));
    let (startinput : safelval), (startss : stmt clist) = 
      match b with
	| Var vi -> 
	    let lv = (Var vi, NoOffset) in
	    let lvt = typeOfLval lv in
            let lvm = 
              let lvm' = getMetadataOfVar vi in
                if doRTTI then
                  let fldtid = getTypeId (typeOfLval (b, off)) in
                    mdSet MKFldRtti lvm' fldtid
                else lvm'
            in
            let lvmlv = 
              if not (mdHas MKLink lvm) then None
              else begin
                let link = mdGet MKLink "transLval" lvm in
                  match link with
                      AddrOf (Var vi', off') -> Some (Var vi', off')
                    | _ -> None
              end
            in
	      (mkSafelval lv lvt lvm lvmlv, empty)

	| Mem addr ->
	    let (saddr, doaddr) = transExp addr in
	    let lv = mkMem saddr.ev NoOffset in
	    let lvt = typeOfLval lv in
	    let lvm = 
              let lvm' = 
	        match unrollType saddr.et with
		    TPtr (t, _) ->
		      let t' = infoDataType t in
		        if isSome t' then saddr.em 
		        else mdSet MKLink saddr.em voidStarZero
			  (* mdClear MKLink saddr.em *)
		  | _ ->
		      E.s (bug "Deference of a non-pointer exp(%a)\n" 
			     d_safeexp saddr);
              in
                if doRTTI then
                  let fldtid = getTypeId (typeOfLval (b, off)) in
                    mdSet MKFldRtti lvm' fldtid 
                else lvm'
            in
 	      (mkSafelval lv lvt lvm None, doaddr)
    in
    let rec transOffset (inlv: safelval) (off: offset) 
      : safelval * stmt clist =
      match off with
	  NoOffset -> inlv, empty

	| Field (f, resto) ->
	    let addf = Field (f, NoOffset) in
	      
            (* If we have info-data, we need to get the info-data field
	       corresponding to this field, if it exists. *)
	    let nextlv = addOffsetLval addf inlv.lv in
	    let linkto = 
	      match infoDataType inlv.lvt with 
		  Some t -> Some (TPtr (t, []))
		| None -> None in
	    let lvmlv, lvm, dolvm = 
	      if mdHas MKLink inlv.lvm && 
		(mdGet MKLink "transOffset" inlv.lvm <> voidStarZero) 
              then begin
		let deref, chklink = 
                  let link = mdGet MKLink "transOffset" inlv.lvm in
		    match linkto with
			Some t ->
			  mkMem (mkCast link t) NoOffset, empty
		      | None ->
			  mkMem link NoOffset, empty
			    (* single (CC.csafeAssert link) *)
                in
		  match unrollType (typeOfLval deref) with
		      TComp (comp, _) -> begin
			try
			  let foff = 
                            Field (getCompField comp 
                                     (fieldInfoName f.fname), NoOffset) in
                          let fmlv = addOffsetLval foff deref in
                          let flvmlv = 
                            match inlv.lvmlv with
                                Some _ -> Some fmlv 
                              | None -> None
                          in
			    flvmlv, 
                            mdSet MKLink inlv.lvm (mkAddrOf fmlv), 
                            chklink
			with Not_found ->
			  None, mdSet MKLink inlv.lvm voidStarZero, chklink
		      end
		    | TVoid _ ->
			None, inlv.lvm, chklink
		    | t -> 
			E.s (bug "transOffset: invalid metadata type %a\n" 
                               d_type t)
              end
	      else begin
                if isSome linkto then begin
		  ignore (E.log "lv=%a lvm=%a\n" 
			    d_lval (addOffsetLval addf inlv.lv)
			    d_metadata inlv.lvm);
		  E.s (bug "Missing 'link' metadata\n")
                end
                else
                  inlv.lvmlv, inlv.lvm, empty
              end
	    in
	      (* Prepare for the rest of the offset *)
	    let next = mkSafelval nextlv (typeOfLval nextlv) lvm lvmlv in
	    let res, dorest = transOffset next resto in
	      res, append dolvm dorest

	| Index (e, resto) ->
	    let (se, doe) = transExp e in
	    let addf = Index (se.ev, NoOffset) in
	      (* Prepare for the rest of the offset *)
	    let nextlv = addOffsetLval addf inlv.lv in

	    (* If we have info-data, we need to get the info-data offset
	       corresponding to this offset. *)
            let elemt = 
              match unrollType inlv.lvt with
                  TArray (elemt', leno, _) -> elemt'
                | _ ->
		    E.s (bug "transOffset: expect an array type, got %a\n" 
                           d_type inlv.lvt)
            in
            let linkto = 
	      match infoDataType elemt with 
		  Some t -> Some (TPtr (t, []))
		| None -> None in
(*             let rec loopOff = function *)
(*                 Index(z, NoOffset) when isZero z -> *)
(*                   Index(se.ev, NoOffset) *)
(*               | Index(i, off) -> Index(i, loopOff off) *)
(*               | Field(f, off) -> Field(f, loopOff off) *)
(*               | _ -> *)
(*                   E.s (bug "doingIndex: expected an Index(0) offset: %a" *)
(*                          d_lval inlv.lv) *)
(*             in *)
              
	    let lvmlv, lvm, dolvm =
	      if mdHas MKLink inlv.lvm && 
		(mdGet MKLink "transOffset" inlv.lvm <> voidStarZero) 
              then begin
		let deref, chklink = 
		  let link = mdGet MKLink "transOffset" inlv.lvm in
		    match linkto with
			Some t ->
                          mkMem link NoOffset, empty
		      | None ->
			  mkMem link NoOffset, empty
		in
                let fmlv = addOffsetLval addf deref in
                let flvmlv = 
                  match inlv.lvmlv with
                      Some _ -> Some fmlv
                    | None -> None
                in
                  if (!E.verboseFlag) then
                    ignore (E.log "doingIndex: deref=%a  fmlv=%a\n" 
                              d_lval deref d_lval fmlv);
(*                   let fmlv = (fst deref, loopOff (snd deref)) in *)
		  flvmlv, mdSet MKLink inlv.lvm (mkAddrOf fmlv), chklink
	      end
              else begin
		if isSome linkto then begin
		  ignore (E.log "lv=%a lvm=%a\n" 
			    d_lval (addOffsetLval addf inlv.lv)
			    d_metadata inlv.lvm);
		  E.s (bug "Expecting 'link' metadata\n")
                end
                else
		  inlv.lvmlv, inlv.lvm, empty
              end
	    in
	    let next = mkSafelval nextlv (typeOfLval nextlv) lvm lvmlv in
	    let res, dorest = transOffset next resto in
	      res, append doe (append dolvm dorest)
    in
    let slv, acc = transOffset startinput off in
      slv, append startss acc
  with exc -> begin
    ignore (E.log "transLval (%s): %a in %s\n"
	      (Printexc.to_string exc) d_lval (b,off) 
	      !currentFunction.svar.vname);
    (mkSafelval (b, off) (typeOfLval (b, off)) mdNone None, empty)
  end



let transGlobal vi isdecl init (l: location) =
  currentLoc := l;
  if showGlobals then begin
    if isdecl then
      ignore (E.log "Transforming GVarDecl(%s) at %a\n" vi.vname d_loc l)
    else
      ignore (E.log "Transforming GVar(%s) at %a\n" vi.vname d_loc l)
  end;
  if isdecl then begin (* A declaration *)
    let g = 
      if ((not (isFunctionType vi.vtype)) || 
	  (H.mem definedFunctionIds vi.vid && 
	   not ((vi.vname = !Globinit.mainname) || 
                (isCompatFunction (Lval (Var vi, NoOffset)))))) then
	(* variables or user defined functions *)
	vi.vtype <- fixupType vi.vtype;
      GVarDecl (vi, l) 
    in
      theFile := consGlobal g !theFile;
      (* create and initialize INFO variable *)
      ( match makeGlobalInfoVar vi with
	    Some vi' ->
	      let mg = GVar (vi', {init=None}, !currentLoc) in
	      let initinfo = 
		setInfoAlwaysValid (Lval (var vi)) (var vi') None true in
		extraGlobInit := append initinfo !extraGlobInit;
		theFile := consGlobal mg !theFile
	  | None -> ());

      (* create and initialize HEADER variable *)
      if vi.vaddrof || (not optHeader) then 
	( match makeGlobalHeaderVar vi with
	      Some vi', init_hdr ->
		let mg = GVar (vi', {init=init_hdr}, !currentLoc) in
(* 		  extraGlobInit := append init_hdr !extraGlobInit; *)
		  theFile := consGlobal mg !theFile;
	    | None, init_hdr ->
(* 		extraGlobInit := append init_hdr !extraGlobInit; *)
                ()
        )

  end 
  else begin (* A definition *)
    let g = 
      let oldt = vi.vtype in
	vi.vtype <- fixupType vi.vtype;
	match init with
	    None -> GVar (vi, {init=None}, l) 
	  | Some _ -> 
	      if typeSigSafe vi.vtype <> typeSigSafe oldt then begin
		if (!showWarnings) then
	            ignore (warn "type of %s changed, dropping its init\n" 
                          vi.vname);
		GVar (vi, {init=None}, l)
	      end
	      else
		GVar (vi, {init=init}, l)
    in      
      theFile := consGlobal g !theFile;
      (* create and initialize INFO variable *)
      ( match makeGlobalInfoVar vi with
	    Some vi' ->
	      let mg = GVar (vi', {init=None}, !currentLoc) in
		(* Initialize the info variable with zero *)
	      let initinfo = initInfoVar vi' in
		extraGlobInit := append initinfo !extraGlobInit;
		theFile := consGlobal mg !theFile
	  | None -> ());

      (* create and initialize HEADER variable *)
      (*       if vi.vaddrof || (not optHeader) then *)
      ( match makeGlobalHeaderVar vi with
	    Some vi', init_hdr ->
	      let mg = GVar (vi', {init=init_hdr}, !currentLoc) in
(* 		extraGlobInit := append init_hdr !extraGlobInit; *)
		theFile := consGlobal mg !theFile;
	  | None, init_hdr ->
              ()
(* 	      extraGlobInit := append init_hdr !extraGlobInit  *)
      )
      
  end 

let transFun f (l: location) = 
  currentLoc := l;
  currentFunction := f;
  currentNeedTsStack := defaultNeedTsStack;
  if showGlobals then 
    ignore (E.log "Transforming GFun(%s) at %a\n" f.svar.vname d_loc l);
  let ismain = (f.svar.vname = !Globinit.mainname) in
    (* See if it is a vararg function *)
  let isva = 
    match f.svar.vtype with
	TFun (_, _, isva, _) -> isva
      | _ -> false
  in
    (* Run the oneret first so that we have always a
       single return where to place the finalizers *)

  (*    Oneret.oneret f; *)

  let (doLocal : stmt clist) = 
    let rec loopLocals (locals: varinfo list) : stmt clist =
      match locals with
	  [] -> empty
	| lo :: restl -> begin
	    let res = loopLocals restl in
	      lo.vtype <- fixupType lo.vtype;

	      let init_info = 
		match makeLocalInfoVar lo with
		    Some lo_info ->
		      initInfoVar lo_info
		  | None -> 
		      empty
	      in
	      (* create and initialize HEADER variable *)
	      let _, init_hdr = 
                if not doHeader then begin
                  if lo.vaddrof then
                    currentNeedTsStack := true;
                  None, empty
                end
                else begin
		  if lo.vaddrof || (not optHeader) then begin
		    currentNeedTsStack := true;
		    makeLocalHeaderVar lo 
		  end
		  else None, empty 
                end
              in
	        append init_hdr (append init_info res)
                    
	  end
    in
      loopLocals f.slocals
  in

  let doFormal = 
    if ismain then begin (* special handling for argv, env *)
      match f.sformals with
	  [] -> empty
	| argc :: [] ->
	    (* create and initialize HEADER variable *)
	    currentNeedTsStack := true;
	    let _, init_hdr = makeLocalHeaderVar argc in
	      init_hdr
	| argc :: (argv :: []) -> begin
	    (* create and initialize HEADER variable *)
	    currentNeedTsStack := true;
	    let init_info = 
	      match makeLocalInfoVar argv with
		  Some argv' -> begin
		    let num = Lval (var argc) in
		      setInfoAlwaysValid 
                        (Lval (var argv)) 
                        (var argv') (Some num) false
		  end
		| None -> empty
	    in
	    let _, init_hdr1 = makeLocalHeaderVar argc in 
	    let _, init_hdr2 = makeLocalHeaderVar argv in
	    let init_hdr = append init_hdr1 init_hdr2 in
	      append init_hdr init_info
	  end
	| argc :: (argv :: (env :: [])) ->
	    (* create and initialize HEADER variable *)
	    currentNeedTsStack := true;
	    let init_info1 = 
	      match makeLocalInfoVar argv with
		  Some argv' -> begin
		    let num = Lval (var argc) in
		      setInfoAlwaysValid 
                        (Lval (var argv)) 
                        (var argv') (Some num) false
		  end
		| None -> empty
	    in
	    let init_info2 = 
	      match makeLocalInfoVar env with
		  Some env' -> begin
		    setInfoAlwaysValid 
                      (Lval (var env)) 
                      (var env') (Some (integer 100)) false
		  end
		| None -> empty
	    in
	    let init_info = append init_info1 init_info2 in
	    let _, init_hdr1 = makeLocalHeaderVar argc in
	    let _, init_hdr2 = makeLocalHeaderVar argv in
	    let _, init_hdr3 = makeLocalHeaderVar env in
	    let init_hdr = append init_hdr1 (append init_hdr2 init_hdr3) in
	      append init_hdr init_info

	| _ -> 
	    E.s (E.bug "too many arguments in main()\n");
    end
    else begin
      (*
	let oldt = f.svar.vtype in
	( match unrollType oldt with
	TFun(rt, args, isva, a) -> begin
	match infoReturnType rt with
	None -> ()
	| Some rt' -> 
	setFunctionType f (TFun(rt', args, isva, a))
	end
	| _ -> E.s (bug "transFun: not a function type\n")
	);
      *)
      let oldt = f.svar.vtype in
	f.svar.vtype <- fixupFunType oldt;

	let newformals, do_formal = 
	  let rec loopFormals (argidx: int) (forms: varinfo list) 
	    : (varinfo list * stmt clist ) =
	    match forms with
		[] -> [], empty
	      | form :: restf -> begin
		  let r1, r2 = loopFormals (argidx + 1) restf in
		    form.vtype <- fixupType form.vtype;
		    (* create INFO variable *)
                    let f_infos = 
		      match makeFormalInfoVar form with
			  Some form_info ->
			    [form; form_info] @ r1
			| None ->
			    form :: r1 in
		      (* create and initialize HEADER variable *)
		    let _, init_hdr =  
                      if not doHeader then begin
                        if form.vaddrof then
			  currentNeedTsStack := true;
                        None, empty
                      end
                      else begin
		        if form.vaddrof || (not optHeader) then begin
			  currentNeedTsStack := true;
			  makeLocalHeaderVar form 
		        end
		        else None, empty 
                      end
                    in
                      f_infos, append init_hdr r2
		end
	  in
	    loopFormals 0 f.sformals
	in
	  setFormals f newformals;
	  
	  recordOriginalFunctionType oldt f.svar.vtype;
	  
	  do_formal
	    (* If the type has changed and this is a global function
	       then we also change its name *)
            (* fixupGlobName f.svar; *)
    end
  in
  let doinit = append doFormal doLocal in

    (* Do the body now *)
    f.sbody <- transBlock f.sbody;

    let theBody = fromList f.sbody.bstmts in
    let callGlobinit = 
      if ismain then begin
	match !(currentFile).globinit with
	    Some gi when not !(currentFile).globinitcalled ->
	      !(currentFile).globinitcalled <- true;
	      single (mkStmt (Instr [Call(None, 
					  Lval(var gi.svar), 
					  [], locUnknown)]))
	  | _ -> empty
      end
      else empty
    in
      if doTemporal then begin
        if !currentNeedTsStack then
	  f.sbody.bstmts <- toList (append callGlobinit
				      (CConsL (CC.pushTSStack (),
					       (append doinit theBody))))
        else
	  f.sbody.bstmts <- toList (append callGlobinit
				      (append doinit theBody));
      end
      else
	  f.sbody.bstmts <- toList (append callGlobinit
				      (append doinit theBody));
      
      H.clear infoLocalVarTable;
      H.clear headerLocalVarTable;

      GFun(f, l)
	
(* Create the preamble *)
let preamble () : global list = 
  [(GText "#include \"cmemsafe.h\"")]

(* Create TypeInfo table *)
let genTypeInfoTable () : global list = 
  if not doRTTI then []
  else begin
    let typeinfoCI = 
      mkCompInfo true "csafe_typeinfo" 
        (fun _ -> 
           [ ("elem_sz", longType, None, [], locUnknown); 
             ("info_sz", longType, None, [], locUnknown)]
        ) 
        []
    in
    let typeinfoType = TComp (typeinfoCI, []) in
    let gTypeInfoTable = 
      let tt_type = 
        TArray(typeinfoType, Some (integer !CC.nextCastTypeId), []) in
      let tt_init = 
        let typeinfoOfType tid = 
          let init_of_size elem_sz info_sz = 
            let elem_sz_fld = getCompField typeinfoCI "elem_sz" in
            let info_sz_fld = getCompField typeinfoCI "info_sz" in
              [ (Field (elem_sz_fld, NoOffset), SingleInit elem_sz);
                (Field (info_sz_fld, NoOffset), SingleInit info_sz) ]
          in
          let elem_sz, info_sz = 
            let rec recordTypeSize (t: typ) =
              match t with
                  TArray(t', _, _) ->
                    recordTypeSize t'
                | _ -> begin
                    let sz = sizeOf t in
                      match sz with
                          SizeOf _ -> (* incomplete type *)
                            zero
                        | _ -> sz
                  end
            in
              if tid = CC.invalidTypeId then zero, zero
              else if tid = CC.nonCastTypeId then one, zero
              else if tid = CC.primitiveTypeId then one, zero
              else if tid = CC.validTypeId then one, zero
              else begin
                match getRepCastType tid with
                    Some t -> begin
                      match infoDataType t with
                          Some info_t ->
                            recordTypeSize t, recordTypeSize info_t
                        | None ->
                            recordTypeSize t, zero
                    end
                  | None -> E.s (E.bug "strange type tid (%d)\n" tid)
              end
          in
            Index (integer tid, NoOffset), 
            CompoundInit (typeinfoType, init_of_size elem_sz info_sz)
        in
        let initl = 
          let rec loop_typeid tid =
            if (tid <= 0) then 
              [typeinfoOfType tid]
            else (typeinfoOfType tid) :: (loop_typeid (tid - 1))
          in
            List.rev (loop_typeid (!CC.nextCastTypeId - 1))
        in
          CompoundInit (tt_type, initl) 
      in
      let tt_vi = makeGlobalVar typeInfoTableName tt_type in
        GVar(tt_vi, {init=Some tt_init}, locUnknown)
    in
      [gTypeInfoTable]
  end

let doFile (file: file) : file =

  if !E.verboseFlag then
    ignore (E.log "Tranforming file\n");
  E.hadErrors := false;
  H.clear definedFunctionIds;
  H.clear stringGlobalVarTable;
  H.clear headerGlobalVarTable;
  H.clear headerLocalVarTable;
  H.clear infoGlobalVarTable;
  H.clear infoLocalVarTable;
  functionDescriptorInit := [];
  H.clear allocFunctions;
  H.clear deallocFunctions;

(*   csafeallocPragma "malloc" [ACons("nozero",[]);  *)
(*                              ACons("sizein", [AInt 1])]; *)
(*   csafeallocPragma "calloc" [ACons("zero",[]);  *)
(*                              ACons("sizemul", [AInt 1; AInt 2])]; *)
(*   csafedeallocPragma "free" []; *)

  (* Mark the file *)
  MU.init ();
  MU.theFile := [];
  Wrappers.initFile ();

  let registerFunctions = function
      GPragma (a, l) ->
	currentLoc := l;
	Wrappers.processPragma a;

    | GFun(fdec, l) ->
	currentLoc := l;
	(* Build the list of functions *)
	MU.registerFunction (MU.Defined fdec);
	MU.registerGlobal fdec.svar true

    | GType (ti, l) ->
	currentLoc := l;
	(* MU.registerTypeinfo ti *) ()
	
    | GCompTag (ci, l) ->
	currentLoc := l;
	(* MU.registerCompinfo ci *) ()

    | GVarDecl (vi, l) -> begin
	currentLoc := l;
	MU.registerGlobal vi false;
	(* See if this is a function that is not yet defined *)
	match unrollType vi.vtype with
	    TFun _ when not (H.mem MU.allFunctions vi.vname) ->
	      MU.registerFunction (MU.Declared vi)
	  | _ -> ()
      end

    | GVar (vi, _, l) ->
	currentLoc := l;
	MU.registerGlobal vi true

    | _ -> ()
  in

    iterGlobals file registerFunctions;


    (* Now we can process the wrappers. The resulting file is in reverse
       order! *)
    Wrappers.replaceWrappers file;

    file.globals <- List.rev file.globals;

    (* Go over all globals once to process some pragmas and collect the
       defined functions *)
    List.iter
      (function
	   GFun (fdec, _) ->
	     H.add definedFunctionIds fdec.svar.vid true
	 | GPragma (a, _) -> begin
	     match a with
	       | Attr(astr, AStr(s) :: rest) when astr = CC.astrCsafeAlloc ->
		   if not (H.mem allocFunctions s) then begin
		     if !E.verboseFlag then
		       ignore (
                         E.log "Will treat %s as an allocation function\n" s);
		     csafeAllocPragma s rest
		   end

	       | Attr(astr, AStr(s) :: rest) when astr = CC.astrCsafeDealloc ->
		   if not (H.mem deallocFunctions s) then begin
		     if !E.verboseFlag then
		       ignore (
                         E.log "Will treat %s as a deallocation function\n" s);
		     csafeDeallocPragma s rest
		   end

	       | _ -> ()
	   end 
	 | _ -> ()
      )
      file.globals;

    currentFile := file;

    visitCilFileSameGlobals computeTypeHierarchyVisitor file;

    let rec doGlobal g = 
      try 
	( match g with
	      GPragma (a, l) -> begin
		theFile := consGlobal g !theFile
	      end
		
	    | _ -> begin
		match g with
		  | GVarDecl (vi, l) -> transGlobal vi true None l
		      
		  | GVar (vi, init, l) -> transGlobal vi false init.init l

		  | GCompTagDecl (comp, l) -> begin
		      currentLoc := l;
		      ( match infoDataType (TComp (comp, [])) with
			    Some (TComp (infocomp, _)) ->
			      theFile := (GCompTagDecl 
                                            (infocomp, l)) :: !theFile
			  | _ -> ()
		      );
		      theFile := consGlobal g !theFile;
		    end

		  | GCompTag (comp, l) ->
		      currentLoc := l;
		      if showGlobals then 
			ignore (E.log "Transforming GCompTag(%s) at %a\n"
				  (compFullName comp) d_loc l);
		      finalizeInfoShadowComp comp;
		      ( match infoDataType (TComp (comp, [])) with
			    Some (TComp (infocomp, _)) ->
			      theFile := (GCompTag (infocomp, l)) :: !theFile
			  | _ -> ()
		      );
		      theFile := consGlobal g !theFile

		  | GType (t, l) ->
		      currentLoc := l;
		      if showGlobals then 
			ignore (E.log "Transforming GType(%s) at %a\n" 
				  t.tname d_loc l);
		      theFile := consGlobal g !theFile

		  | GFun(f, l) -> begin 
		      currentLoc := l;
		      let g' = 
                        if isCompatFunction (Lval (Var f.svar, NoOffset)) 
                        then begin
		          if showGlobals then 
			    ignore (
                              E.log "Ignoring compat function GFun(%s) at %a\n"
                                      f.svar.vname d_loc l);
                          GFun(f, l)
                        end
                        else 
                          transFun f l 
                      in
			theFile := consGlobal g' !theFile;

		        (* create and initialize HEADER variable *)
		        if f.svar.vaddrof || (not optHeader) then
			  ( match makeGlobalHeaderVar f.svar with
			        Some vi', init_hdr ->
				  let mg = 
                                    GVar (vi', {init=init_hdr}, !currentLoc) in
(*                                     extraGlobInit := *)
(*                                     append init_hdr !extraGlobInit; *)
				    theFile := consGlobal mg !theFile;
			      | None, init_hdr ->
(*                                   extraGlobInit := *)
(*                                   append init_hdr !extraGlobInit; *)
                                  ()
                          );

		    end 
		      
		  | GAsm _ | GText _ | GPragma _ 
		  | GEnumTag _ | GEnumTagDecl _ ->
		      theFile := consGlobal g !theFile
	      end
	)
      with 
	| e -> begin 
	    E.hadErrors := true;
	  end
    in
      if showGlobals then ignore (E.log "Transforming file\n");

      (* Now the original file, including the global initializer *)
      iterGlobals file doGlobal;

      (* Now finish the globinit *)
      let newglobinit =
	match file.globinit with
	    None ->
	      let gi = getGlobInit file in
	      let new_initstmts = CConsL (CC.csafeInit (), !extraGlobInit) in
		gi.sbody.bstmts <- toList new_initstmts;
		
		Some gi
		  
	  | Some g -> begin
	      match !theFile with
		  GFun(gi, _) :: rest ->

		    theFile := rest; (* Take out the global intializer
				      * (last thing added *)
		    let res =
		      let bstmts = fromList gi.sbody.bstmts in
		      let new_initstmts =
			CConsL (CC.csafeInit (), append !extraGlobInit bstmts)
		      in
			toList new_initstmts

		    in
		      gi.sbody.bstmts <-
		      if compactBlocks then compactStmts res else res;
		      Some gi
		| _ -> E.s (bug "Transform: Cannot find global initializer")
	    end
      in
	

      (* Create TypeInfo and SubType Tables *)
      let typeinfoTable = 
        if doRTTI then genTypeInfoTable () else [] in

      let subtypeTable = 
        if doRTTI then genSubtypeTable () else [] in

      let res = 
        let res0 : global list ref = ref [] in
          res0 := List.rev (!theFile);
	  res0 :=
          List.append
            (List.append typeinfoTable subtypeTable)
            !res0;

          (* Create the preamble *)
          res0 := List.append (preamble ()) !res0;
          !res0
      in
	theFile := [];
	
	let res' = {file with globals = res; globinit = newglobinit } in
	  Globinit.insertGlobInit res' ;

	  let res'' = 
	    if showGlobals then 
              ignore (E.log "Spliting local INFO variables\n");
	    Stats.time "split" Csafesplit.splitLocalInfos res' in
	    if showGlobals then begin
              ignore (E.log "Finished transforming file\n");
	      ignore (E.log "\n\nNumber of Pointer Assignments: %d\n" 
                      !numPtrAssign);
	      ignore (E.log "Number of Pointer Dereferences: %d\n" 
                      !numPtrDeref);
	      ignore (E.log "Number of All Pointer Casts: %d\n" 
                      !numPtrCasts);
	      ignore (E.log "Number of Bad Pointer Casts: %d\n\n\n" 
                      !numBadPtrCasts);
	    end;
	    res''

