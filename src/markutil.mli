(* Adapted from CCured *)

(** Call before processing a file *)
val init: unit -> unit

(** Accumulate here the marked file *)
val theFile: Cil.global list ref 

(** A pointer to the current function *)
val currentFunction: Cil.fundec ref

(* Keep track of functions that are declared and defined *)
type funinfo = Declared of Cil.varinfo | Defined of Cil.fundec

val allFunctions: (string, funinfo) Hashtbl.t
val allCompinfo: (string, Cil.compinfo) Hashtbl.t


val findFunc: name:string -> neededby:string -> Cil.varinfo

(** Called functions *)
val calledFunctions: (string, Cil.varinfo) Hashtbl.t

(** Register a function *)
val registerFunction: funinfo -> unit

(** Register a global variable. Say if it is defined. *)
val registerGlobal: Cil.varinfo -> bool -> unit

(** See if a function is already defined *)
val alreadyDefinedFunction: string -> bool


(** Apply a function to a function of a given name. If the function was not 
 * registered already we will apply the function when we will register it *)
val applyToFunction: string -> (Cil.varinfo -> unit) -> unit

(** Like the above but apply only to definitions. *)
val applyToFunctionDef: string -> (Cil.fundec -> unit) -> unit


(** Use to get the function type attributes for an expression *)
val getFunctionTypeAttributes: Cil.exp -> Cil.attributes


(** Use to add attributes to a function variable *)
val addFunctionTypeAttribute: Cil.attribute -> Cil.varinfo -> unit


(** Check if a global variable is imported, declared but not defined. *)
val isImported: string -> bool  

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

