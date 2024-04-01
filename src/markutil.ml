(* Adapted from CCured *)

open Cil
open Pretty

module H = Hashtbl
module E = Errormsg

(* We will accumulate the marked globals in here *)
let theFile: global list ref = ref []
    
let currentFunction: fundec ref = ref Cil.dummyFunDec

(* We keep track of all functions that are declared or defined *)
type funinfo = Declared of varinfo | Defined of fundec

(* All functions indexed by name *)
let allFunctions: (string, funinfo) H.t = H.create 113

(* All compinfo indexed by name *)
let allCompinfo: (string, compinfo) H.t = H.create 113


(* Store all the functions indexed by the name before alpha conversion *)
let allFunctionBasenames: (string, funinfo) H.t = H.create 113

(* A list of functions that were called in the file *)
let calledFunctions: (string, varinfo) H.t = H.create 113

(* Apply a function to a function declaration or definition *)
let applyToFunctionMemory: (string, (funinfo -> unit)) H.t = H.create 13    

let applyToFunctionCommon (n: string) (f: funinfo -> unit) = 
  (* Memorize this action *)
  H.add applyToFunctionMemory n f;
  (* Now apply it to already defined functions *)
  let all = H.find_all allFunctionBasenames n in
  List.iter (fun one -> f one) all


let applyToFunction (n: string) (f: varinfo -> unit) = 
  applyToFunctionCommon n 
    (function Declared fvi -> f fvi | Defined fdec -> f fdec.svar)


let applyToFunctionDef (n: string) (f: fundec -> unit) = 
  applyToFunctionCommon n 
    (function Declared fvi -> () | Defined fdec -> f fdec)



let alreadyDefinedFunction n = 
  try
    match H.find allFunctions n with 
      Defined _ -> true
    | _ -> false
  with Not_found -> false

let getFunctionTypeAttributes (e: exp) = 
  match unrollType (typeOf e) with 
    TFun(_, _, _, fa) -> fa
  | _ ->  E.s (bug "getFunctionTypeAttribute: not a function type")

let addFunctionTypeAttribute (a: attribute) (vi: varinfo)  = 
  match vi.vtype with 
    TFun(rt, args, isva, fa) -> vi.vtype <- TFun(rt, args, isva, 
                                                addAttribute a fa)
  | _ -> E.s (bug "addFunctionTypeAttribute: not a function type")

let registerFunction (fi: funinfo) = 
  let fvi = match fi with Declared fvi -> fvi | Defined fdec -> fdec.svar in
  let lookup = getAlphaPrefix fvi.vname in
  (* ignore (E.log "Registering function %s\n" fvi.vname); *)
  (* See if it has the format attribute *)

  let fl = H.find_all applyToFunctionMemory lookup in
  List.iter (fun f -> f fi) fl;
  H.add allFunctions fvi.vname fi;
  H.add allFunctionBasenames lookup fi
    
let matchSuffix (lookingfor: string) (lookin: string) = 
  let inl = String.length lookin in
  let forl = String.length lookingfor in
  inl >= forl && String.sub lookin (inl - forl) forl = lookingfor


let allGlobals: (string, bool ref) H.t = H.create 113
let registerGlobal (v: varinfo) (isdef: bool) : unit = 
  let entry = 
    try H.find allGlobals v.vname 
    with Not_found -> begin
      let entry = ref false in
      H.add allGlobals v.vname entry;
      entry
    end
  in
  if isdef then entry := true


let isImported (vn: string) = 
  try
    let entry = H.find allGlobals vn in
    not !entry
  with Not_found -> begin
    ignore (E.warn "isImported for %s, which is not even declared\n" vn);
    true (* pretend it is imported *)
  end





let findFunc ~(name:string) ~(neededby:string) =
  try
    match H.find allFunctions name with 
      Declared vi -> vi
    | Defined fdec -> fdec.svar
  with Not_found ->
    E.s (error "Can't find declaration of %s needed by %s." 
	   name neededby)



let init () = 
  H.clear allFunctions;
  H.clear allCompinfo;
  H.clear allFunctionBasenames;
  H.clear applyToFunctionMemory;
  H.clear calledFunctions;
  H.clear allGlobals;
  theFile := []

(*
 *
 * Copyright (c) 2001-2002,
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

