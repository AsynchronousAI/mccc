(* Adapted from CCured *)

open Cil
open Csafeutil

module E = Errormsg
module H = Hashtbl

type allocInfo = {
  mutable aiZeros: bool;   (* Whether the allocator initializes 
			      the memory it allocates *)
  mutable aiGetSize: exp list -> exp; (* Extract the size argument 
					 out of a list of arguments *)
  mutable aiNewSize: exp -> exp list -> exp list; (* Rewrite the argument 
						     list with a new size *)
}

let allocFunctions : (string, allocInfo) H.t = H.create 13
let deallocFunctions : (string, bool) H.t = H.create 13

let getAllocInfo fname = 
  try Some (H.find allocFunctions fname)
  with _ -> None

let isAllocFunction name = 
  isSome (getAllocInfo name)

let isDeallocFunction name = 
  H.mem deallocFunctions name


(* Now a constructor of allocation information from csafealloc pragmas *)
let csafeAllocPragma (name: string) (args: attrparam list) : unit =
  let getArg n args = 
    try List.nth args n
    with _ -> E.s (bug "no size arguments in call to allocator %s\n" name)
  in
  let replaceArg n what args = 
    let rec loop n = function
	_ :: rest when n = 0 -> what :: rest
      | a :: rest when n > 0 -> a :: loop (n - 1) rest
      | _ -> E.s (bug "cannot replace size argument for allocator %s\n" name)
    in
      loop n args
  in
    (* Initialize like for malloc *)
  let ai = 
    { aiZeros = false;
      aiGetSize = getArg 0;
      aiNewSize = replaceArg 0;
    }
  in
  let rec loop = function
      [] -> ()
    | ACons("nozero", _) :: rest -> ai.aiZeros <- false; loop rest
    | ACons("zero", _) :: rest -> ai.aiZeros <- true; loop rest
    | ACons("sizein", [AInt n]) :: rest ->
	ai.aiGetSize <- getArg (n-1); 
	ai.aiNewSize <- replaceArg (n-1);
	loop rest
    | ACons("sizemul", [AInt n1; AInt n2]) :: rest ->
	ai.aiGetSize <- (fun args ->
			   BinOp(Mult, getArg (n1-1) args, getArg (n2-1) args,
				 intType));
	ai.aiNewSize <- (fun what args ->
			   (replaceArg (n1-1) one
			      (replaceArg (n2-1) what args)));
	loop rest
    | ACons("sizenone", _) :: rest ->
	ai.aiGetSize <- (fun _ -> zero);
	ai.aiNewSize <- (fun _ args -> args);
	loop rest
    | a :: rest ->
	(ignore (E.warn "Don't understand csafealloc attribute: %a@!"
		   d_attrparam a));
	loop rest
  in
    loop args;
    (* Add to the hash *)
    H.add allocFunctions name ai

(* Now a constructor of deallocation information from csafedealloc pragmas *)
let csafeDeallocPragma (name: string) (args: attrparam list) : unit =
  let rec loop = function
      [] -> ()
    | a :: rest ->
	(ignore (E.warn "Don't understand csafedealloc attribute: %a@!"
		   d_attrparam a));
	loop rest
  in
    loop args;
    (* Add to the hash *)
    H.add deallocFunctions name true


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

