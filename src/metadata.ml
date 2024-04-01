open Pretty
open Cil
open Clist

open Cilextra

open Csafeutil

module E = Errormsg
module H = Hashtbl

module CC = Csafechecks

(**** Pointer representation *)

(* We need the stub definitions because INFO and HEADER are mutually
   recursive. *)
let headerCIStub =
  mkCompInfo true "csafe_header" 
    (fun _ -> []) 
    [Attr(CC.astrHeaderComp, [])]

let headerTypeStub = TComp (headerCIStub, [])
let headerPtrTypeStub = TPtr (headerTypeStub, [])

let infoCIStub =
  mkCompInfo true "csafe_info"
    (fun _ -> [])
    [Attr(CC.astrInfoComp, [])]

let infoTypeStub = TComp (infoCIStub, [])
let infoPtrTypeStub = TPtr (infoTypeStub, [])

(* Kinds of meta data *)

(* An example to show the differences of the three RTTIs:

   Type 0: int
   Type 1: int *
   Type 2: struct S1 { int * f1; int * f2; };
   Type 3: struct S2 { int * g1; int * g2; int * g3; };
   Type 4: struct S3 { int * h1; struct S1 h2; };
   Type 5: struct S4 { int * e1; struct S2 e2; };

   struct S4 s;
   struct S3 * q = & s;
   struct S1 * p = & (q->h2);

   For q->h2,  BlkRtti = 5, PtrRtti = 5, FldRtti = 4
   For *p,     BlkRtti = 5, PtrRtti = 4, FldRtti = 4

   PtrRtti is used for CHECK_RTTI;
   BlkRtti is used for CHECK_PTR_ARITH;
   FldRtti is used to assign PtrRtti in the case of "v1 = &v2". 
           It is only for internal use during transformation, and 
           not maintained in the result programs.

   A caveat is FldRtti may not be the actual type, and it is implemented
   as the static type of the field preceded by a RTTI check of the
   pointer deference if any.
*)

type mkind = 
  | MKBlkRtti	(* actual type of a block *)
  | MKPtrRtti 	(* actual type of dereference of a pointer *)
  | MKFldRtti	(* actual type of a field/an offset within a block *)

  | MKBase	(* the base *)
  | MKSize	(* the size *)
  | MKCapPtr	(* pointer to the global time-stamp *)

  | MKBlkhdr 	(* pointer to the block header *)
  | MKCapIndex 	(* local copy of the time-stamp *)

  | MKInfoBase 	
  | MKLink 	(* link to info data of next level *)

  | MKElemSize		(* size of each element *)
  | MKInfoElemSize	(* size of info for each element *)

let d_mkind () = function
  | MKBlkRtti -> text "BlkRtti"
  | MKPtrRtti -> text "PtrRtti"
  | MKFldRtti -> text "FldRtti"

  | MKBase   -> text "Base"
  | MKSize   -> text "Size"
  | MKCapPtr -> text "CapPtr"

  | MKBlkhdr -> text "Blkhdr"
  | MKCapIndex  -> text "CapIndex"

  | MKInfoBase  -> text "InfoBase"
  | MKLink -> text "Link"

  | MKElemSize -> text "ElemSize"
  | MKInfoElemSize -> text "InfoElemSize"

(* For each infodata kinds we store the name of the field and its type *)
let fieldOfMK = function
  | MKBlkRtti -> "_blktid", longType
  | MKPtrRtti -> "_tid", longType
  | MKFldRtti -> 
      E.s (E.bug "FldRtti should not be used in transformed programs.\n")

  | MKBase   -> "_base", voidPtrType
  | MKSize   -> "_size", ulongType
  | MKCapPtr -> "_cap_ptr", ulongPtrType

  | MKBlkhdr -> "_blkhdr", headerPtrTypeStub
  | MKCapIndex   -> "_cap_index", ulongType

  | MKInfoBase   -> "_info_base", infoPtrTypeStub 
  | MKLink -> "_link", voidPtrType
      (* will fix this type when we make the field *)

  | MKElemSize -> "_elem_sz", shortType
  | MKInfoElemSize -> "_info_sz", shortType


let mkOfField = function
  | "_blktid" -> MKBlkRtti
  | "_tid"    -> MKPtrRtti

  | "_base"    -> MKBase
  | "_size"    -> MKSize
  | "_cap_ptr" -> MKCapPtr

  | "_blkhdr"  -> MKBlkhdr
  | "_cap_index" -> MKCapIndex

  | "_info_base"    -> MKInfoBase
  | "_link" -> MKLink

  | "_elem_sz" -> MKElemSize
  | "_info_sz" -> MKInfoElemSize

  | fn -> E.s (bug "mkOfField: not a metadata field: %s\n" fn)

let offsetOfMK (ci: compinfo) (k: mkind) : offset = 
  let fname, _ = fieldOfMK k in
    Field (getCompField ci fname, NoOffset)

(* The order of the fields is very important, because it must match the
   code in the library. *)
let headerFieldsOrder = 
  if not doHeader then
    []
  else begin
    match doTemporal, doRTTI with
      | false, false ->
          [MKBase; MKSize]
      | false, true ->
          [MKBase; MKSize]
      | true, false ->
          [MKBase; MKSize; MKCapIndex] 
      | true, true ->
          [MKBlkRtti; MKBase; MKSize; MKCapIndex] 
  end

let infoFieldsOrder = 
  match doHeader, doTemporal, doRTTI with
    | false, false, false -> 
        [MKBase; MKSize; MKLink]
    | false, false, true -> 
        [MKPtrRtti; MKBlkRtti; MKBase; MKSize; MKLink]
    | false, true, false -> 
        [MKBase; MKSize; MKCapIndex; MKCapPtr; MKLink]
    | false, true, true -> 
        [MKPtrRtti; MKBlkRtti; MKBase; MKSize; MKCapIndex; MKCapPtr; MKLink]

    | true, false, false -> [MKBlkhdr; MKLink]
    | true, false, true -> [MKPtrRtti; MKBlkhdr; MKLink]
    | true, true, false -> [MKBlkhdr; MKCapPtr; MKLink]
    | true, true, true -> [MKPtrRtti; MKBlkhdr; MKCapPtr; MKLink]


let headerCI = 
  let ci = headerCIStub in
  let fields = 
    List.map
      (fun fld ->
         let fn, ft = fieldOfMK fld in
           { fcomp=ci; fname=fn; ftype=ft; 
             fbitfield=None; fattr=[]; floc=locUnknown })
      headerFieldsOrder
  in
    ci.cfields <- fields;
    ci

let headerType = TComp (headerCI, [])
let headerPtrType = TPtr (headerType, [])

let infoCI = 
  let ci = infoCIStub in
  let fields = 
    List.map 
      (fun fld ->
	 let fn, ft = fieldOfMK fld in
	   { fcomp=ci; fname=fn; ftype=ft;
             fbitfield=None; fattr=[]; floc=locUnknown })
      infoFieldsOrder
  in
    ci.cfields <- fields;
    ci

let infoType = TComp (infoCI, [])
let infoPtrType = TPtr (infoType, [])

(* Global Definitions *)

let csafeForeverBlkhdr = 
  makeGlobalVar "csafe_forever_blkhdr" headerType
let csafeNeverBlkhdr = 
  makeGlobalVar "csafe_never_blkhdr" headerType

(* metadata for a safeexp or a safelval. *)
type metadata = 
    { 
      _blktid : exp option;
      _tid: exp option;
      _fldtid: exp option;

      _base: exp option;
      _size: exp option;
      _cap_ptr: exp option;

      _blkhdr: exp option;
      _cap_index: exp option;

      _info_base: exp option;
      _link: exp option;

      _elem_sz: exp option;
      _info_sz: exp option;
    }

let d_metadata () (md: metadata) : doc = 
  let fields: doc list = 
    List.fold_left
      (fun acc (fld, whato) ->
	 match whato with
	     None -> acc
	   | Some e -> (dprintf "%s=%a" fld d_exp e) :: acc)
      []
      [ 
        ("_blktid", md._blktid); 
        ("_tid", md._tid); 
        ("_fldtid", md._fldtid); 
        ("_base", md._base);
	("_size", md._size);
	("_cap_ptr", md._cap_ptr);
	("_blkhdr", md._blkhdr); 
	("_cap_index", md._cap_index); 
	("_info_base", md._info_base);
        ("_link", md._link);
	("_elem_sz", md._elem_sz);
	("_info_sz", md._info_sz);
      ]
  in
    if fields = [] then
      nil
    else 
      (docList ~sep:(chr ',' ++ break) (fun x -> x)) () fields

let mdNone : metadata = 
  { 
    _blktid = None;
    _tid = None; 
    _fldtid = None;

    _base = None;
    _size = None;
    _cap_ptr = None;

    _blkhdr = None; 
    _cap_index = None; 
    _info_base = None; 
    _link = None;

    _elem_sz = None;
    _info_sz = None;
  }

let mkMetadata ?(blktid=None) ?(tid=None) ?(fldtid=None) 
  ?(base=None) ?(size=None) ?(cap_ptr=None) ?(cap_index=None) 
  ?(info_base=None) ?(link=None) 
  ?(elem_sz=None) ?(info_sz=None) 
  (blkhdr :exp option) = 
  {
    _blktid = blktid;
    _tid = tid;
    _fldtid = fldtid;

    _base = base;
    _size = size;
    _cap_ptr = cap_ptr;

    _blkhdr = blkhdr;
    _cap_index = cap_index;
    _info_base = info_base;
    _link = link;

    _elem_sz = elem_sz;
    _info_sz = info_sz;
  }
    
let mdHas (mk: mkind) (md: metadata) : bool =
  match mk, md with
    | MKBlkRtti, { _blktid = Some _ } -> true
    | MKPtrRtti, { _tid = Some _ } -> true
    | MKFldRtti, { _fldtid = Some _ } -> true

    | MKBase, { _base = Some _ } -> true
    | MKSize, { _size = Some _ } -> true
    | MKCapPtr, { _cap_ptr = Some _ } -> true

    | MKBlkhdr, { _blkhdr = Some _ } -> true
    | MKCapIndex, { _cap_index = Some _ } -> true
  
    | MKInfoBase, { _info_base = Some _ } -> true
    | MKLink, { _link = Some _ } -> true

    | MKElemSize, { _elem_sz = Some _ } -> true
    | MKInfoElemSize, { _info_sz = Some _ } -> true

    | _, _ -> false

let mdGet (mk: mkind) (where: string) (md: metadata) : exp =
  let bugmsg () = 
    (bug "mdGet(%a): %s md=%a\n" d_mkind mk where d_metadata md) in
  let warnmsg () = 
    if (!showWarnings) then
	(warn "mdGet(%a): %s md=%a\n" d_mkind mk where d_metadata md)
    else (E.log "")
  in
    match mk, md with
      | MKBlkRtti, { _blktid = Some blktid } -> blktid
      | MKPtrRtti, { _tid = Some tid } -> tid
      | MKFldRtti, { _fldtid = Some fldtid } -> fldtid
      | MKBase, { _base = Some base } -> base
      | MKSize, { _size = Some size } -> size
      | MKCapPtr, { _cap_ptr = Some cap_ptr } -> cap_ptr

      | MKBlkhdr, { _blkhdr = Some blkhdr } -> blkhdr
      | MKCapIndex, { _cap_index = Some cap_index } -> cap_index

      | MKInfoBase, { _info_base = Some info_base } -> info_base
      | MKLink, { _link = Some link } -> link

      | MKElemSize, { _elem_sz = Some elem_sz } -> elem_sz
      | MKInfoElemSize, { _info_sz = Some info_sz } -> info_sz

      (* Return invalid values when the request metadata is missing.
         This is useful for supporting casts from non-pointers to
         pointers.  
      *)
      | MKBlkRtti, { _blktid = None } -> 
          ignore (warnmsg ()); 
          integer CC.invalidTypeId
      | MKPtrRtti, { _tid = None } -> 
          ignore (warnmsg ());
          integer CC.invalidTypeId
      | MKFldRtti, { _fldtid = None } -> 
          ignore (warnmsg ());
          integer CC.invalidTypeId
      | MKBase, { _base = None } -> 
          ignore (warnmsg ());
          voidStarZero
      | MKSize, { _size = None } -> 
          ignore (warnmsg ());
          zero
      | MKCapPtr, { _cap_ptr = None } -> 
          ignore (warnmsg ());
          Lval (var CC.csafeCapNever)
      | MKBlkhdr, { _blkhdr = None } -> 
          ignore (warnmsg ());
          mkAddrOf (var csafeNeverBlkhdr)
      | MKCapIndex, { _cap_index = None } -> 
          ignore (warnmsg ());
          Lval (mkMem (Lval (var CC.csafeCapNever)) NoOffset)
      | MKInfoBase, { _info_base = None } -> 
          ignore (warnmsg ());
          voidStarZero
      | MKLink, { _link = None } -> 
          ignore (warnmsg ());
          voidStarZero
      | MKElemSize, { _elem_sz = None } -> 
          ignore (warnmsg ());
          zero
      | MKInfoElemSize, { _info_sz = None } -> 
          ignore (warnmsg ());
          zero


let mdSet (mk: mkind) (md: metadata) (what: exp) : metadata = 
  match mk with
    | MKBlkRtti -> { md with _blktid = Some what }
    | MKPtrRtti -> { md with _tid = Some what }
    | MKFldRtti -> { md with _fldtid = Some what }

    | MKBase -> { md with _base = Some what }
    | MKSize -> { md with _size = Some what }
    | MKCapPtr -> { md with _cap_ptr = Some what }

    | MKBlkhdr -> { md with _blkhdr = Some what }
    | MKCapIndex -> { md with _cap_index = Some what }

    | MKInfoBase -> { md with _info_base = Some what }
    | MKLink -> { md with _link = Some what }

    | MKElemSize -> { md with _elem_sz = Some what }
    | MKInfoElemSize -> { md with _info_sz = Some what }
	
let mdClear (mk: mkind) (md: metadata) : metadata = 
  match mk with
    | MKBlkRtti -> { md with _blktid = None }
    | MKPtrRtti -> { md with _tid = None }
    | MKFldRtti -> { md with _fldtid = None }

    | MKBase -> { md with _base = None }
    | MKSize -> { md with _size = None }
    | MKCapPtr -> { md with _cap_ptr = None }

    | MKBlkhdr -> { md with _blkhdr = None }
    | MKCapIndex -> { md with _cap_index = None }

    | MKInfoBase -> { md with _info_base = None }
    | MKLink -> { md with _link = None }
	
    | MKElemSize -> { md with _elem_sz = None }
    | MKInfoElemSize -> { md with _info_sz = None }

let mdCopy (mk: mkind) (mdto: metadata) (mdfrom: metadata) : metadata = 
  if mdHas mk mdfrom then
    mdSet mk mdto (mdGet mk "mdCopy" mdfrom)
  else
    mdClear mk mdto

(* Safelvals are prepared for taking their address. We keep the lval
   itself and its type (not the type of its address), the metadata of
   pointer to the lval *)
type safelval =
    { lv: lval;  	(* the lval itself *)
      lvt: typ;	  	(* typeOfLval(lv) *)
      lvm: metadata;  	(* the info data for the address of lval *)
      lvmlv: lval option; (* if present, it is the same as  *(lvm._link) *)
    }

and safeexp = 
    { ev: exp;    	(* This is the main expression. *)
      et: typ;    	(* Type type of the whole safeexp *)
      em: metadata;  	(* The info data for the value *)
      emlv: lval option; (* if present, it is the lval carrying on all 
			    the information in em *)
    }

let d_safeexp () (se: safeexp) : doc = 
  dprintf "SE(@[V=%a,@?M=%a@])\n" d_exp se.ev d_metadata se.em

let d_safelval () (slv: safelval) : doc = 
  dprintf "(LV=%a,LVT=%a,M=%a)" 
    d_lval slv.lv d_type slv.lvt d_metadata slv.lvm

let mkSafelval (lv: lval) (t: typ) (m: metadata) (mlv: lval option)
  : safelval = 
  { lv = lv; lvt = t; lvm = m; lvmlv = mlv }

let mkSafeexp (e: exp) (t: typ) (m: metadata) (mlv: lval option) : safeexp = 
  { ev = e; et = t; em = m; emlv = mlv }



let isHeaderComp (comp: compinfo) =
  (comp.cstruct && hasAttribute CC.astrHeaderComp comp.cattr)

let isHeaderType (t: typ) =
  match unrollType t with
      TComp (comp, _) when isHeaderComp comp -> true
    | _ -> false

let isInfoComp (comp: compinfo) =
  (comp.cstruct && hasAttribute CC.astrInfoComp comp.cattr)

let isInfoType (t: typ) =
  match unrollType t with
      TComp (comp, _) when isInfoComp comp -> true
    | _ -> false

let isInfoReturnComp (comp: compinfo) =
  (comp.cstruct && hasAttribute CC.astrInfoReturn comp.cattr)

let isInfoReturnType (t: typ) =
  match unrollType t with
      TComp (comp, _) when isInfoReturnComp comp -> true
    | _ -> false

let isInfoFunType (t: typ) = 
  match unrollType t with
      TFun (rt,args,isva,a) -> hasAttribute CC.astrInfoFun a
    | _ -> false
	
let getFieldsOfHeader (t: typ) : (mkind * offset * typ) list = 
  match unrollType t with
      TComp (comp, _) when isHeaderComp comp -> begin
        let fields = comp.cfields in
          List.map
            (fun f ->
               (* Find which metadata is stored in this field *)
               let mk = mkOfField f.fname in
                 (mk, Field(f, NoOffset), f.ftype))
            fields
      end
    | _ -> E.s (bug "getFieldsOfHeader %a\n" d_type t)

let getFieldsOfInfo (t: typ) : (mkind * offset * typ) list =
  match unrollType t with
      TComp (comp, _) when isInfoComp comp -> begin
	let fields = comp.cfields in
	  List.map 
	    (fun f ->
	       (* Find which metadata is stored in this field *)
	       let mk = mkOfField f.fname in
		 (mk, Field(f, NoOffset), f.ftype))
	    fields
      end
    | _ -> E.s (bug "getFieldsOfInfo %a\n" d_type t)

let readFieldsOfInfo (t: typ) (m: lval) : metadata = 
  if isInfoType t then begin
    let mdFromInfo = 
      let infoFields = getFieldsOfInfo t in
        List.fold_left
	  (fun (acc: metadata) (mk, o, ft) ->
	     mdSet mk acc (Lval (addOffsetLval o m)))
	  mdNone
	  infoFields
    in
      if doHeader then begin
        let hdr = 
          mkMem (Lval (addOffsetLval 
                         (offsetOfMK infoCI MKBlkhdr) m)) NoOffset in
        let mdFromHeaderAndInfo = 
          let headerFields = getFieldsOfHeader (typeOfLval hdr) in
            List.fold_left
              (fun (acc: metadata) (mk, off, ft) ->
                 mdSet mk acc (Lval (addOffsetLval off hdr)))
              mdFromInfo
              headerFields
        in
          mdFromHeaderAndInfo
      end
      else
        mdFromInfo
  end
  else
    { mdNone with _link = Some (Lval m) }

