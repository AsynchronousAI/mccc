(* Adapted from CCured *)

open Cil
open Pretty

module MU = Markutil

module H = Hashtbl
module E = Errormsg

let debugWrappers = false

(* maps functions to wrappers *)
let mesacWrappedBy: (string, string) H.t = H.create 15

(* a list of wrappers *)
let mesacWrappers: (string, unit) H.t = H.create 15

let theFile: global list ref = ref []

let processPragma = function
  | Attr("mesacwrapper", lst) -> begin
      match lst with 
	AStr wrappername :: rest -> begin
          (* Now process the rest *)
          let wrapped = ref "" in
          List.iter
            (function  
                ACons("of", [AStr x]) -> wrapped := x
              | _ -> ignore (warn "Invalid #pragma mesacwrapper"))
            rest;
          if !wrapped = "" then 
            E.s (error "Missing \"of\" argument in #pragma mesacwrapper");
          if debugWrappers then 
            ignore (E.log "Will use wrapper %s for %s\n"
                      wrappername !wrapped);

          H.add mesacWrappers wrappername ();
	  try
            (* Check whether the function already has a wrapper. *)
	    let previousWrapper = H.find mesacWrappedBy !wrapped in
	    if previousWrapper <> wrappername then
              ignore (E.unimp "Function %s has more than one wrapper" 
                        !wrapped )
            (*if the names are the same, it's probably just a repeated
              .h file. *)
	  with Not_found ->
            (* Good, there were no other wrappers for this function. *)
	    H.add mesacWrappedBy !wrapped wrappername;
	  
        end 
      | _ ->   
          ignore (warn "Invalid #pragma mesacwrapper")
  end

  | _ -> ()

(* Find the wrapper of a function. Might raise Not_found. *)
let findWrapperOf (s: string) =
  let wrappername = H.find mesacWrappedBy s in
  try
    match H.find MU.allFunctions wrappername with
      MU.Defined fdef -> fdef.svar
    | MU.Declared fdec -> fdec
  with Not_found ->
    E.s (error "Cannot find the definition of wrapper %s of %s.\n"
           wrappername s)

let hasAWrapper (s:string) = 
  H.mem mesacWrappedBy s

let isAWrapper (s:string) = 
  H.mem mesacWrappers s


let initFile () = 
  H.clear mesacWrappedBy;
  H.clear mesacWrappers

(** Make a pass over the file and replace the wrapped functions with their 
 * wrappers. Also, fill in the bodies of the deep wrappers *)

let currentFunctionName = ref ""
(* First a visitor that replaces the calls with wrappers *)
class replaceWrapperVisitor 
    (inWrapperFunction: bool) : cilVisitor = object
  inherit nopCilVisitor

  method vinst (i: instr) : instr list visitAction = 
    match i with
      Call(opt, Lval(Var vf, NoOffset), params, loc) 
        when not inWrapperFunction 
      -> 
        begin
          (* We are not in a wrapper *)
          try
	    let wrapper = findWrapperOf vf.vname in
            if debugWrappers then
              ignore (E.log "changing call to %s to %s in %s.\n" vf.vname 
                        wrapper.vname !currentFunctionName);
	    ChangeDoChildrenPost 
              ([Call(opt, Lval(Var wrapper, NoOffset), params, loc)],
               (fun x -> x))
          with Not_found ->
	    DoChildren
        end
            
    | _ -> DoChildren
      
            (* Intercept taking the address of a function *)
  method vexpr e = 
    match e with 
      AddrOf (Var vf, NoOffset) when isFunctionType vf.vtype -> begin
        try
	  let wrapper = findWrapperOf vf.vname in
          if debugWrappers then
            ignore (E.log "changing &%s to &%s in %s.\n" vf.vname 
                      wrapper.vname !currentFunctionName);
	  ChangeTo (AddrOf (Var wrapper, NoOffset))
        with Not_found -> begin
	  SkipChildren
        end
      end
          
    | _ -> DoChildren
          
  method vblock b = 
    if hasAttribute "nosafe" b.battrs then
      SkipChildren
    else 
      DoChildren

end


(** Replace all the wrapped calls with the wrappers. The resulting file is in 
 * reverse order !!! *)

let replaceWrappers (fl: file) : unit = 

  (* Now we must replace the calls with the appropriate wrappers *)
  theFile := [];
  let doReplaceWrappers (g: global): unit = 
    let doit, name, inWrapper = 
      match g with 
        GFun (fdec, _) -> true, fdec.svar.vname, isAWrapper fdec.svar.vname
      | GVar (v, _, _) -> true, v.vname, false
      | _ -> false, "", false
    in
    if doit then begin
      currentFunctionName := name;
      let glist = visitCilGlobal (new replaceWrapperVisitor inWrapper) g in
      List.iter (fun g -> theFile := g :: !theFile) glist
    end else
      theFile := g :: !theFile
  in
    iterGlobals fl doReplaceWrappers;
    ( match fl.globinit with
	  None -> ();
	| Some g -> begin
	    match !theFile with
		GFun(gi, _) :: rest ->
		  theFile := rest (* Take out the global intializer
				    * (last thing added *)
	      | _ -> 
		  E.s (bug "replaceWrappers: Cannot find global initializer")
	  end
    );
    fl.globals <- !theFile;
    ()
  
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

