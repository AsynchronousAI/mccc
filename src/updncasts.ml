open Cil
open Cilextra
open Clist

open Csafeutil
open Csafechecks

module E = Errormsg
module H = Hashtbl

let subtypeTableName = "csafe_subtype_table"
let typeTableName = "csafe_type_table"
let typeInfoTableName = "csafe_typeinfo_table"

let typeInCastsTable : (typsig, (typ * int)) H.t = H.create 113
let repCastTypeTable : (int, typ) H.t = H.create 113

let getRepCastType (tid: int) : typ option = 
  try
    Some (H.find repCastTypeTable tid)
  with Not_found ->
    None

let getTypeId (t: typ) : exp = 
  try 
    integer (snd (H.find typeInCastsTable (typeSigNoAttrs t)))
  with Not_found -> 
(*     ignore (E.warn "getTypeId: type not found in cast-table: %a\n" *)
(*               d_type t); *)
    integer nonCastTypeId 

let rec isPrimitiveType (t: typ) : bool = 
  match t with
    | TVoid _ | TInt _ | TFloat _ | TEnum _ -> true
    | TPtr _ | TArray _ | TComp _ | TFun _ | TBuiltin_va_list _ -> false
    | TNamed (ti, _) -> isPrimitiveType ti.ttype

let isNonCastType (t: typ) : bool = 
  not (H.mem typeInCastsTable (typeSigNoAttrs t))

let rec recordCastType (t: typ) : unit = 
  let tsig = typeSigNoAttrs t in
    if not (H.mem typeInCastsTable tsig) then begin
      if isPrimitiveType t then begin
	H.add typeInCastsTable tsig (t, primitiveTypeId)
      end
      else begin
	let tid = !nextCastTypeId in
	  H.add typeInCastsTable tsig (t, tid);
	  H.add repCastTypeTable tid t;
	  nextCastTypeId := !nextCastTypeId + 1
      end;
      ( match t with
	  | TNamed (ti, a) 	-> recordCastType ti.ttype
	  | TVoid a        	-> ()
	  | TInt (ik, a)   	-> ()
	  | TFloat (fk, a) 	-> ()
	  | TPtr (t1, a)          -> recordCastType t1
	  | TArray (t1, eo, a)    -> recordCastType t1
	  | TFun (t1, args, isva, a) -> recordCastType t1
	  | TEnum (ei, a) 	-> ()
	  | TBuiltin_va_list a 	-> ()
	  | TComp (ci, a) -> 
	      List.iter (fun fi -> recordCastType fi.ftype) ci.cfields
      )
    end


(* Definition of subtype relationship 

   Suppose that T1 contains pointer fields at offsets p_1, ..., p_n,
   where p_i's are in sorted order. Similarly, let q_1, ..., q_m denote
   all of the offsets of pointers fields within T2.  Now T1 is a subtype
   of T2 iff m <= n and p_i = q_i for 1 <= i <= m.
              
   Note that, under this definition, the rule 
       subtype(T1, T2) && subtype(T2, T1) ==> T1 = T2
   is not true.  In fact, when T1 and T2 are subtypes of each other, 
   even their sizes need not to be same.  Here is such an example: 
       T1 = struct { int * f1; }
       T2 = struct { int * g1; int g2; }

   To make other runtime checks easier, we add one more constraint to 
   the subtype relationship:
       subtype(T1, T2)  only if T2 is a primitive type or 
                                sizeof(T2) <= sizeof(T1).
 
   Now we have
       subtype(T1, T2) && subtype(T2, T1) ==> 
           both T1 and T2 are primitive types or sizeof(T1) = sizeof(T2).

   Any types that are not involved in type casts are assumed to have 
   no subtype relationship with any other types.

*)
let typeLayoutTable : (typsig, (int * typ) list) H.t = H.create 113

let rec unrollCompoundType (startoff: int) (t: typ) : (int * typ) list = 
  let tsig = typeSigNoAttrs t in
  let layout = 
    try
      H.find typeLayoutTable tsig
    with Not_found -> begin
      match t with
        | TNamed (ti, a)           -> unrollCompoundType 0 ti.ttype
        | TVoid a            	   -> [(0, t)]
        | TInt (ik, a)   	   -> [(0, t)]
        | TFloat (fk, a) 	   -> [(0, t)]
        | TPtr (t1, a)             -> [(0, t)]
        | TFun (t1, args, isva, a) -> [(0, t)]
        | TEnum (ei, a) 	   -> [(0, t)]
        | TBuiltin_va_list a 	   -> [(0, t)]
        | TArray (t1, Some e, a)       	-> 
            let e' = constFold true e in
            let len = begin
              match isInteger e' with
                  Some (i64) ->
                    if (i64 = Int64.zero) then
		      if (!E.verboseFlag) then
                         ignore (
			    E.warn "unrollCompoundType: zero-length array %a"
                                d_type t);
                    Int64.to_int i64
                | None ->
                    E.s (E.unimp 
                           "unrollCompoundType: non-const length array %a"
                           d_type t);
            end in
            let elemsz = bitsSizeOf t1 in
            let rec loop soff i =
              if i = 0 then []
              else begin 
                let l1 = unrollCompoundType soff t1 in
                let l2 = loop (soff+elemsz) (i-1) in
                  List.concat [l1;l2]
              end
            in
              (* Dangerous hack: 

                 To avoid "Stack_Overflow" exceptions, we cut the length
                 of array layout if the array length exceeds the limit.
                 This may make a larger array be the subtype of a
                 smaller array, which is wrong and needs to be fixed
                 later.  
              *)
            let mod_len = 
              if len < 512 then len
              else begin
		if (!E.verboseFlag) then
                  ignore 
                    (E.warn 
                       "unrollCompoundType: array (%a) size bigger than 512\n"
                       d_type t);
                512
              end
              
            in
              loop 0 mod_len

        | TArray (t1, None, a) ->
            (* empty array *)
	    if (!E.verboseFlag) then
              ignore (E.warn "unrollCompoundType: empty array %a\n" d_type t);
            [(0, (TPtr(t, [])))]

        | TComp (ci, a) when ci.cstruct -> 
	    let dofield fi = 
	      let newoff, newwidth = bitsOffset t (Field(fi, NoOffset)) in
	        unrollCompoundType newoff fi.ftype
	    in
	      List.concat (List.map dofield ci.cfields)

        | TComp (ci, a) when not ci.cstruct -> begin
            let selectedField = ref None in
            let dofield fi = 
              match !selectedField with
                  Some fi' ->
                    if (bitsSizeOf fi.ftype > bitsSizeOf fi'.ftype) then
                      selectedField := Some fi
                    else ()
                | None ->
                    selectedField := Some fi
            in
              List.iter dofield ci.cfields;
              match !selectedField with
                  Some fi -> unrollCompoundType 0 fi.ftype
                | None -> (* empty union *) []
          end
            
        | TComp _ -> E.s (E.bug "unrollCompoundType: mystery comp")
    end
  in
    if not (H.mem typeLayoutTable tsig) then
      H.add typeLayoutTable tsig layout;
    List.map (fun (off, t) -> (startoff + off, t)) layout

let rec getPtrOffsets (t: typ) : int list = 
  let ptrflds = 
    List.filter 
      (fun (o, t) -> match t with TPtr _ -> true | _ -> false)
      (unrollCompoundType 0 t)
  in
    List.map (fun (o, t) -> o) ptrflds

(* Is t1 a subtype of t2? *)
let rec isSubType (t1: typ) (t2: typ) : bool = 
  if (!E.verboseFlag) then
    ignore (E.log "isSubType (%a) (%a)\n" d_type t1 d_type t2);
  if (typeSigNoAttrs t1 = typeSigNoAttrs t2) then true
  else if (isNonCastType t1 || isNonCastType t2) then false
(*   else if (not (isPrimitiveType t2) &&  *)
(*            (bitsSizeOf t1 < bitsSizeOf t2)) then false *)
  else begin
    let rec isPrefix l1 l2 = 
      match l1, l2 with
        | [], [] -> true
        | [], h2 :: t2 -> true
        | h1 :: t1, [] -> false
        | h1 :: t1, h2 :: t2 -> 
            if h1 != h2 then false
            else isPrefix t1 t2
    in
    let t1_poffs = getPtrOffsets t1 in
    let t2_poffs = getPtrOffsets t2 in
      isPrefix t2_poffs t1_poffs
  end

(* let rec isSubType (t1: typ) (t2: typ) : bool =  *)
(*   (\* Is t1 a subtype of t2? *\) *)
(*   (\*   ignore (E.log "isSubType (%a) (%a) \n" d_type t1 d_type t2); *\) *)
(*   if (typeSigNoAttrs t1 = typeSigNoAttrs t2) then true *)
(*   else if (isNonCastType t1 || isNonCastType t2) then false *)
(*   else if (isPrimitiveType t2) then true *)
(*   else begin *)
(*     match unrollType t1, unrollType t2 with *)
(* 	TPtr (t1', _), TPtr (t2', _) -> *)
(* 	  (isPrimitiveType t2') || (isSubType t1' t2') *)
(*       | _, TComp (ci, _) when (not ci.cstruct) ->  *)
(* 	  let rec loop fl =  *)
(* 	    match fl with *)
(* 		[] -> true *)
(* 	      | fh :: fl' ->  *)
(* 		  if isSubType t1 fh.ftype then loop fl' *)
(* 		  else false *)
(* 	  in *)
(* 	    loop ci.cfields *)

(*       | TComp (ci, _), _ when (not ci.cstruct) -> *)
(* 	  let rec loop fl =  *)
(* 	    match fl with *)
(* 		[] -> true *)
(* 	      | fh :: fl' ->  *)
(* 		  if isSubType t2 fh.ftype then loop fl' *)
(* 		  else false *)
(* 	  in *)
(* 	    loop ci.cfields *)

(*       | TComp _, _ | _, TComp _ ->  *)
(* 	  let rec isMemberSubType (off1,bt1) (off2,bt2) =  *)
(* 	    (\* 	      ignore (E.log "isMemberSubType (%d,%a) (%d,%a)\n" *\) *)
(* 	    (\* 			off1 d_type bt1 off2 d_type bt2); *\) *)
(* 	    if (off1 != off2) then false *)
(* 	    else begin *)
(* 	      match bt1, bt2 with *)
(* 		  TPtr (t1, _), TPtr (t2, _) -> true *)
(* 		| _ -> isSubType bt1 bt2 *)
(* 	    end *)
(* 	  in *)
(* 	  let gcl_t1 = unrollCompoundType 0 t1 in *)
(* 	  let gcl_t2 = unrollCompoundType 0 t2 in *)
(* 	  let rec loop gcl1 gcl2 =  *)
(* 	    match gcl1, gcl2 with *)
(* 		[], gc2 :: r2 -> false *)
(* 	      | _, [] -> true *)
(* 	      | gc1 :: r1, gc2 :: r2 -> *)
(* 		  if (isMemberSubType gc1 gc2) then *)
(* 		    loop r1 r2 *)
(* 		  else false *)
(* 	  in *)
(* 	    loop gcl_t1 gcl_t2 *)
(*       | _, _ -> false *)
(*   end *)


class computeTypeHierarchyVisitorClass = object
  inherit nopCilVisitor

  method vglob (g: global) = begin
    match g with
        GType (ti, l) -> begin
          match unrollType ti.ttype with
              TComp (ci, _) when not ci.cstruct ->
                recordCastType ti.ttype;
                DoChildren
            | _ -> 
                recordCastType ti.ttype;
                DoChildren
        end
      | _ -> DoChildren
  end

  method vexpr (e: exp) = begin
    match e with
	CastE(t, e') -> begin
	  let t' = typeOf e' in
	    if (isPointerType t' || isPointerType t) then begin
	      recordCastType t';
	      recordCastType t;
	    end;
	    DoChildren
        end
      | StartOf lv -> begin
          let arr_t = typeOfLval lv in
          let ptr_t =
            match unrollType arr_t with
                TArray(elemt, _, _) -> TPtr(elemt, [])
              | _ ->
	          E.s (bug "expect an array type, got %a\n" d_type arr_t)
          in
            recordCastType arr_t;
            recordCastType ptr_t;
            DoChildren
        end
      | AddrOf lv -> begin
          let lvt = typeOfLval lv in
          let ptr_t = TPtr(lvt, []) in
            recordCastType lvt;
            recordCastType ptr_t;
            DoChildren
        end
      | BinOp (bop, e1, e2, restyp) -> begin
          ( match bop with
	      | PlusPI | MinusPI | IndexPI ->
                  recordCastType (typeOf e1);
              | _ -> ();
          );
          DoChildren
        end
      | _ -> 
	  DoChildren
  end

end

let computeTypeHierarchyVisitor = new computeTypeHierarchyVisitorClass


let charY = Const(CChr('Y'))
let charN = Const(CChr('N'))

let genSubtypeTable () : global list = 
  if not doRTTI then []
  else begin 
    H.clear typeLayoutTable;
    
    (* Use a two-dimension array of unsigned short to record subtype
       relation on types, one bit for each pair of types. *)
    let maxTypeId = !nextCastTypeId - 1 in
    let numRows = !nextCastTypeId in
    let elemsOneRow = 
      if !nextCastTypeId mod 16 = 0 then !nextCastTypeId 
      else (!nextCastTypeId / 16 + 1) in
    let rowType = TArray(ushortType, Some (integer elemsOneRow), []) in
    let tableType = TArray(rowType, Some (integer numRows), []) in
    let table_vi = makeGlobalVar subtypeTableName tableType in

    let isSubtypeByTypeId ltid rtid : bool = 
      if (ltid = rtid) then true
      else if (ltid = validTypeId || rtid = validTypeId) then true
      else if (ltid = nonCastTypeId || rtid = nonCastTypeId) then
        (* NonCastType <-> NonCastType *)
        false
      else if (rtid = primitiveTypeId) then true
      else if (ltid = primitiveTypeId) then false
      else if (ltid = invalidTypeId || rtid = invalidTypeId) then 
        false
      else begin
        match getRepCastType ltid, getRepCastType rtid with
	    Some lt, Some rt -> begin
	      if (isSubType lt rt) then true
	      else false
	    end
	  | _ -> 
	      (* Error *) 
	      ignore (E.log "unmatched: cast %d to %d\n" ltid rtid);
	      false
      end
    in

    let row_init ltid : init = 
      let initl = ref [] in
      let sum = ref 0 in
        for rtid = 0 to (elemsOneRow * 16 - 1) do
          if (rtid <= maxTypeId) then begin
            if isSubtypeByTypeId ltid rtid then begin
              let incval = 1 lsl (15 - (rtid mod 16)) in
                sum := !sum + incval
            end
          end;
          if ((rtid + 1) mod 16 = 0) then begin
            initl := (Index(integer (rtid / 16), 
                            NoOffset), 
                      SingleInit (kinteger IUShort !sum)) :: !initl;
            sum := 0;
          end;
        done;
        CompoundInit (rowType, List.rev !initl)
    in
    let table_init : init = 
      let initl = ref [] in
        for ltid = 0 to (numRows - 1) do
          initl := (Index(integer ltid, 
                          NoOffset), 
                    row_init ltid) :: !initl
        done;
        CompoundInit (tableType, List.rev !initl)
    in
    let gSubtypeTable = 
      GVar(table_vi,{init=Some table_init}, locUnknown) in

    let gTypeTableComment = 
      let raw_comment =
        H.fold
          (fun tig (t, tid) acc ->
	     acc ^ (
	       Pretty.sprint 78 
		      (Pretty.dprintf "CastType (%a) = %d\n" d_type t tid)
	     ))
          typeInCastsTable
          ""
      in
      let comment = 
        let cmt = ref "" in
          String.iter 
            (fun c -> 
               if c = '/' then cmt := !cmt ^ "("
               else cmt := !cmt ^ (String.make 1 c))
            raw_comment;
          !cmt
      in
        GText ("\n/*\n" ^ comment ^ "*/\n")
    in

    let gTypeTable = 
      let tt_type = TArray(charPtrType, Some (integer !nextCastTypeId), []) in
      let nameOfType tid : string = 
        if tid = invalidTypeId then "invalid_type"
        else if tid = nonCastTypeId then "non_cast_type"
        else if tid = primitiveTypeId then "primitive_type"
        else if tid = validTypeId then "valid_type"
        else begin
          match getRepCastType tid with
              Some t -> Pretty.sprint 78 (Pretty.dprintf "%a" d_type t)
            | None -> E.s (E.bug "strange type tid (%d)\n" tid)
        end
      in
      let tt_init = 
        let initl = ref [] in
          for tid = 0 to maxTypeId do
            initl := (Index (integer tid, NoOffset),
                      SingleInit (Const (CStr (nameOfType tid)))) :: !initl
          done;
          CompoundInit (tt_type, List.rev !initl) 
      in
      let tt_vi = makeGlobalVar typeTableName tt_type in
        GVar(tt_vi, {init=Some tt_init}, locUnknown) 
    in
      [gTypeTable; gTypeTableComment; gSubtypeTable]
  end

let isAllowedCast oldt newt =
  match unrollType oldt, unrollType newt with
      TPtr (oldt',_), TPtr (newt',_) -> begin
	let typeSigWithoutAttrs = typeSigWithAttrs (fun a -> []) in
	let rec isNontrivialCast t1 t2 =
	  if (typeSigWithoutAttrs (unrollType t1) =
		typeSigWithoutAttrs (unrollType t2)) then false
	  else begin
	    match unrollType t1, unrollType t2 with
	      | TPtr (t1', _), TPtr (t2', _) ->
		  isNontrivialCast t1' t2'
	      | TArray (t1', _, _), TArray (t2', _, _) ->
		  isNontrivialCast t1' t2'
	      | TComp (_, _), _ -> true
	      | _, TComp(_, _) -> true
	      | _, _ -> false
	  end
	in
	let rec isCompatType t1 t2 = 
	  if (typeSigWithoutAttrs (unrollType t1) = 
		typeSigWithoutAttrs (unrollType t2)) then true
	  else begin
	    match unrollType t1, unrollType t2 with
		TVoid _, TVoid _ -> true
	      | TVoid _, TInt _ -> true
	      | TInt _, TVoid _ -> true
	      | TInt _, TInt _ -> true
	      | TFloat _, TFloat _ -> true
	      | TInt _, TFloat _ -> true
	      | TFloat _, TInt _ -> true
	      | TPtr (t1', _), TPtr (t2', _) -> 
		  isSubType oldt newt
		    (* 		    isCompatType t1' t2' *)
	      | TArray (t1', _, _), TArray (t2', _, _) ->
		  isSubType oldt newt
		    (* 		    isCompatType t1' t2' *)
	      | TEnum _, TEnum _ -> true
	      | _ -> 
		  isSubType oldt newt
		    (* 		    ignore (E.warn "IncompatibleCast: %a: Cast (%a) to (%a).\n" *)
		    (* 			      d_loc !currentLoc d_type (unrollType oldt) d_type (unrollType newt)); *)
		    (* 		    false *)
	  end
	in
	  numPtrCasts := !numPtrCasts + 1;

	  if (isNontrivialCast oldt newt) then begin
	    if (!E.verboseFlag) then
	      ignore (E.warn "NonTrivialCast: %a : Cast (%a) to (%a).\n"
		      d_loc !currentLoc 
		      d_type (unrollType oldt)
		      d_type (unrollType newt));
	  end;

	  if (isCompatType oldt' newt') then
	    true
	  else begin
	    numBadPtrCasts := !numBadPtrCasts + 1;
	    ignore (E.warn "IncompatibleCast: %a: Cast (%a) to (%a).\n"
		      d_loc !currentLoc d_type 
		      (unrollType oldt) d_type (unrollType newt));
	    false
	  end
	    (*
	      match infoDataType oldt', infoDataType newt' with
	      None, None -> true
	      | Some _, None -> true
	      | None, Some _ -> 
	      ignore (E.warn "%a: Cast (%a) to higher-level (%a).\n"
	      d_loc !currentLoc d_type oldt d_type newt);
	      false
	      | Some _, Some _ ->
	      if (typeSigSafe (unrollType oldt') <> 
	      typeSigSafe (unrollType newt')) then
	      ignore (E.warn "%a: Cast between multi-level (%a) and (%a).\n"
	      d_loc !currentLoc d_type oldt d_type newt);
	      true
	    *)
	    
      end
    | _, TInt _ -> true
    | TInt _, TFloat _ -> true
    | TFloat _, TFloat _ -> true
    | TInt _, TPtr _ -> 
	ignore (E.warn "%a: Cast (%a) to (%a).\n" d_loc !currentLoc d_type oldt d_type newt);
	false
    | _, _ -> 
	if (!showWarnings) then
	    ignore (E.warn "%a: Cast (%a) to (%a).\n" d_loc !currentLoc d_type oldt d_type newt);
	false
