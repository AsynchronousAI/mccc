
module F = Frontc
module C = Cil
module CK = Check
module E = Errormsg
open Pretty

let doCheck = ref false
let printStages = ref false
let printStats = ref false

  (* E.logChannel *) 
let cilChannel = ref None
let mergeChannel = ref None
let globinitChannel = ref None
let memsafeChannel = ref None
let optChannel = ref None
let defaultChannel = ref None

let parseOneFile (fname: string) : Cil.file = begin
  (* PARSE and convert to CIL *)
  if !printStages then ignore (E.log "Parsing %s\n" fname);
  let cil = F.parse fname () in
  
  (* See if we had errors *)
  if !E.hadErrors then
    E.s (E.error "We had errors during parsing\n");
  if !doCheck then begin
    ignore (E.log "Checking CIL after CABS2CIL\n");
    ignore (CK.checkFile [] cil);
  end;
  cil
end

class stripASMVisitor = object
  inherit C.nopCilVisitor
  method vglob g = match g with
    C.GAsm(_,l) -> C.ChangeTo([C.GText("/* Removed ASM */\n")])
  | _ -> C.DoChildren
  method vinst i = match i with
    C.Asm(_) -> C.ChangeTo([])
  | _ -> C.SkipChildren
end 

(****** MAIN *************)
let main () = begin
  
  Cil.useLogicalOperators := false;

  (* what stages should we run? *)
  let doMerge = ref true in 
  let doGlobInit = ref true in 
  let doMemSafe = ref true in 
  let doOpt = ref true in 

  let doNothing () = 
    doMerge := false ;
    doGlobInit := false ;
    doMemSafe := false ; 
    doOpt := false ;
    ()
  in 

  let stripASM = ref false in 

  (* open and set an output channel *) 
  let outChannel (what:string) (cr: out_channel option ref) (fname: string) = 
    match fname with
	"-" | "stdout" -> cr := Some(stdout) ;
          if !E.verboseFlag then
            ignore (Printf.printf "Writing %s output to stdout\n" what );
      | _ -> 
	  try 
            cr := Some(open_out fname) ;
            if !E.verboseFlag then
              ignore (Printf.printf "Writing %s output to %s\n" what fname );
	  with e -> 
            raise (Arg.Bad ("Cannot open " ^ what ^ " file " ^ fname))
  in 

  let setDebugFlag v name = 
    E.debugFlag := v;
    if v then Pretty.flushOften := true
  in
  let setTraceDepth n =
    Pretty.printDepth := n
  in
  let rec recursiveAddThing n =
    if (n = 0) then 0 else 1 + (recursiveAddThing (n-1))
  in

  let infileList = ref [] in 

  let recordFile fname = infileList := fname :: (!infileList) in

  let parseExtraFile (s: string) = 
    try
      let sfile = open_in s in
	while true do
          let line = 
	    try input_line sfile with e -> (close_in sfile; raise e) in
          let linelen = String.length line in
          let rec scan (pos: int) (* next char to look at *)
            (start: int) : unit (* start of the word, 
                                   or -1 if none *) =
            if pos >= linelen then 
              if start >= 0 then 
		recordFile (String.sub line start (pos - start))
              else 
		() (* Just move on to the next line *)
            else
              let c = String.get line pos in
		match c with 
		    ' ' | '\n' | '\r' | '\t' -> 
                      (* whitespace *)
                      if start >= 0 then begin
			recordFile (String.sub line start (pos - start));
                      end;
                      scan (pos + 1) (-1)

		  | _ -> (* non-whitespace *)
                      if start >= 0 then 
			scan (pos + 1) start 
                      else
			scan (pos + 1) pos
          in
            scan 0 (-1)
	done
    with Sys_error _ -> E.s (E.error "Cannot find extra file: %s\n" s)
      |  End_of_file -> () 
  in

  let default_stages = 
    Printf.sprintf "(default stages:%s%s%s%s)\n" 
      (if !doMerge then " merge" else " (no merge)") 
      (if !doGlobInit then " globinit" else " (no globinit)") 
      (if !doMemSafe then " memsafe" else " (no memsafe)") 
      (if !doOpt then " opt" else " (no opt)") 
  in 

  let argDescr = [
    (* Stage Control Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tStage Control Options\n" 
      ^ default_stages; 

    "--noMerge", Arg.Unit (fun () -> doMerge := false),
    "do not merge multiple input files" ; 
    "--noGlobInit", Arg.Unit (fun () -> doGlobInit := false),
    "do not pull out global initializers" ;
    "--noMemSafe", Arg.Unit (fun () -> doMemSafe := false), 
    "do not instrument the code with run-time safety checks" ;
    "--noOpt", Arg.Unit (fun () -> doOpt := false), 
    "do not optimize the instrumented code" ; 

    "--doMerge", Arg.Unit (fun () -> doMerge := true),
    "do merge multiple input files" ;
    "--doGlobInit", Arg.Unit (fun () -> doGlobInit := true),
    "do pull out global initializers" ;
    "--doMemSafe", Arg.Unit (fun () -> doMemSafe := true), 
    "do instrument the code with run-time safety checks" ;
    "--doOpt", Arg.Unit (fun () -> doOpt := true), 
    "do optimize the instrumented code" ; 
    
    "--onlyMerge", Arg.Unit (fun () -> doNothing () ; doMerge := true),
    "only merge the input files" ;
    "--onlyGlobInit", Arg.Unit (fun () -> doNothing () ; doGlobInit := true),
    "only pull out global initializers" ; 
    "--onlyMemSafe", Arg.Unit (fun () -> doNothing () ; doMemSafe := true), 
    "only instrument the code" ; 
    "--onlyOpt", Arg.Unit (fun () -> doNothing () ; doOpt := true), 
    "only optimize the instrumented code" ; 

    "--splitInfo", Arg.Unit (fun () -> Csafesplit.dontSplit := false),
    "split local INFO variables" ;
    "--splitArgs", Arg.Unit (fun () -> Csafesplit.splitArguments := true),
    "split argument INFO variables" ;

    (* Output Channel Options *) 
    "", Arg.Unit (fun () -> ()), 
    "\n\t\tOutput Options\n(you may specify '-' or 'stdout' for output file names)\n" ; 

    "--out", Arg.String (outChannel "default" defaultChannel) ,
    "the name of the final result file" ; 
    "--log", Arg.String (fun s -> 
			   E.logChannel := open_out s), 
    "the name of the log file" ; 
    "--cilout", Arg.String (outChannel "cil" cilChannel) , 
    "the name of the cil file (or merged file, if many input files)" ; 
    "--mergedout", Arg.String (outChannel "merger" mergeChannel) ,
    "the name of the merged file" ; 
    "--globinitout", Arg.String (outChannel "globinit" globinitChannel) , 
    "the name of the global-initializer-containting file" ; 
    "--memsafeout", Arg.String (outChannel "memsafe" memsafeChannel) , 
    "the name of the memory-safe file" ; 
    "--optimout", Arg.String (outChannel "optim" optChannel) , 
    "the name of the optimized cured file" ; 
    "--noPrintLn", Arg.Unit (fun _ -> Cil.lineDirectiveStyle := None),
    "don't output #line directives";
    "--commPrintLn", Arg.Unit (fun _ -> Cil.lineDirectiveStyle := Some Cil.LineComment),
    "output #line directives in comments";
    "--printCilAsIs", Arg.Unit (fun _ -> Cil.printCilAsIs := true),
    "do not try to simplify the CIL when printing";

    (* General Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tGeneral Options\n" ; 

    "--verbose", Arg.Unit (fun _ -> E.verboseFlag := true),
    "turn on verbose mode";
    "--warnall", Arg.Unit (fun _ -> E.warnFlag := true), "Show all warnings";
    "--debug", Arg.String (setDebugFlag true),
    "<xxx> turns on debugging flag xxx";
    "--nodebug", Arg.String (setDebugFlag false), 
    "<xxx> turns off debugging flag xxx";
    "--flush", Arg.Unit (fun _ -> Pretty.flushOften := true),
    "Flush the output streams often (aids debugging)" ;
    "--check", Arg.Unit (fun _ -> doCheck := true),
    "turns on consistency checking of CIL";
    "--nocheck", Arg.Unit (fun _ -> doCheck := false),
    "turns off consistency checking of CIL";
    "--stats", Arg.Unit (fun _ -> printStats := true),
    "print some statistics";
    "--stages", Arg.Unit (fun _ -> printStages := true),
    "print the stages of the algorithm as they happen";
    "--tr",         Arg.String Trace.traceAddMulti,
    "<sys>: subsystem to show debug trace printfs for";
    "--pdepth",     Arg.Int setTraceDepth,
    "<n>: set max debug trace print depth (default: 5)";
    "--recurse",    Arg.Int (fun n -> (ignore (recursiveAddThing n)); ()),
    "<n>: deliberately make stack grow to O(n) bytes";
    "--extrafiles", Arg.String parseExtraFile,
    "<filename>: the name of a file that contains a list of additional files to process, separated by whitespace of newlines";

    (* Merging Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tMerging Options\n" ; 

    "--keepunused", Arg.Unit (fun _ -> Rmtmps.keepUnused := true),
    "do not remove the unused variables and types";

    (* Globinit Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tGlobal Initializer Options\n" ; 

    "--entryPoint", Arg.String (fun s -> Globinit.mainname := s),
    "<xxx> call globinit from program entry point xxx";

    (* Transformation Options *)
    "--stripASM", Arg.Unit (fun _ -> stripASM := true),
    "remove inline assembly statements";

    (* Optimization Options *) 

    (* Parsing Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tParsing Options\n" ; 

    "--MSVC", Arg.Unit (fun _ -> 
                          C.msvcMode := true;
                          F.setMSVCMode ();
                       ), 
    "Produce MSVC output. Default is GNU";

  (* MISC options *)

  ] @ F.args in 
  let usageMsg = "Usage: mesac [options] source-files" in
  let send_merged_output_to_cilChannel = ref false in 
    (* this point in the code is the program entry point *)

(*    Stats.reset false ; *)

    (* parse the command-line arguments *)
    Arg.parse argDescr recordFile usageMsg;
    Cil.initCIL ();

    (* Now change the type of some builtins *)
    Hashtbl.add Cil.gccBuiltins 
      "GCC_VARARGS_START" (Cil.ulongType, [ ], false);
    Hashtbl.add Cil.gccBuiltins 
      "GCC_STDARG_START" (Cil.ulongType, [ ], false);

    infileList := List.rev !infileList; 

    (**********************************************************************
      * STAGE 1
      *
      * Parse input files into CIL.
    **********************************************************************)
    let cils = 
      if !printStages then ignore (E.log "Stage 1: Parsing\n") ;
      let cils_ = List.map parseOneFile !infileList (* Stats.time "parsing" (
	fun () -> List.map parseOneFile !infileList) () *) in
	begin 
	  match cils_, !cilChannel with
	    | _, None -> () 
	    | [one], Some(x) -> 
		if not !Rmtmps.keepUnused then begin
		  if !printStages then 
		    ignore (E.log "Stage 1.1: Removing unused temporaries\n") ;
		  Rmtmps.removeUnusedTemps one;
		end;
		let oldpci = !C.print_CIL_Input in
		  C.print_CIL_Input := oldpci; (* Leave alone this one *)
		  (* Stats.time "printCil" *) (C.dumpFile C.defaultCilPrinter x) one;
		  C.print_CIL_Input := oldpci
	    | many, Some x -> send_merged_output_to_cilChannel := true 
	end ; 
	cils_
    in

    (**********************************************************************
      * STAGE 2
      *
      * Merge input files into one file. 
    **********************************************************************)
    let merged : Cil.file =
      let merged_ = 
	(* Stats.time "merge" (
	  fun () -> *) 
	    match !doMerge, cils with
	      | _, [] -> E.s (E.error "No arguments\n") 
	      | _, [one] -> one 
	      | false, lst -> 
		  E.s (E.error "Too many input files with no merging")
	      | true, lst -> 
		  if !printStages then 
		    ignore (E.log "Stage 2: Merging\n") ;
		  Mergecil.merge lst "merged"
(*	) () *) in 

	(**********************************************************************
	  * STAGE 2.1
	  *
	  * Removing unused temporaries
	**********************************************************************)
	if not !Rmtmps.keepUnused then begin
	  if !printStages then 
	    ignore (E.log "Stage 2.1: Removing unused temporaries\n") ;
	  Rmtmps.removeUnusedTemps merged_;
	end;

	if !send_merged_output_to_cilChannel then begin
	  match !cilChannel with
	      None -> ()
	    | Some(x) ->
		let oldpci = !C.print_CIL_Input in
		  C.print_CIL_Input := oldpci;
		  (* Stats.time "printCil" *) ( 
		    C.dumpFile C.defaultCilPrinter x) merged_;
		  C.print_CIL_Input := oldpci
	end ;
	begin
	  match !mergeChannel with
	      None -> ()
	    | Some(x) ->
		let oldpci = !C.print_CIL_Input in
		  C.print_CIL_Input := oldpci;
		  (* Stats.time "printCil" *) (
		    C.dumpFile C.defaultCilPrinter x) merged_;
		  C.print_CIL_Input := oldpci
	end ;
	(* See if we had errors *)
	if !E.hadErrors then 
	  E.s (E.error "We had errors during removal of unused temporaries\n");

	merged_
    in

    (**********************************************************************
      * STAGE 3
      *
      * Pull out global initializers (and inline assmebly, if desired)
    **********************************************************************)
    let globinit = 
      let globinit_ = match !doGlobInit with
	  true -> 
	    if !printStages then ignore (E.log "Stage 3: Glob-Init\n") ;
	    Globinit.doFile merged 
	| false -> merged
      in 
	begin 
	  match !globinitChannel with
	      None -> ()
	    | Some(x) -> 
		(* Stats.time "printCil" *) (
		  C.dumpFile C.defaultCilPrinter x) globinit_;
	end ; 

	let globinit_ = match !stripASM with
	    true ->
	      C.visitCilFileSameGlobals (new stripASMVisitor) globinit_ ;
	      globinit_
		
	  | false -> globinit_
	in 
	  globinit_
    in

    (**********************************************************************
      * STAGE 4
      *
      * Program transformations to ensure memory safety
    **********************************************************************)
    let memsafe = 
      let memsafe_ = match !doMemSafe with
	  true -> 
	    if !printStages then 
	      ignore (E.log "Stage 4: Transformations for Memory Safety\n") ;
	    Memsafe.doFile globinit
	| false -> globinit
      in

	if not !Rmtmps.keepUnused then begin
	  if !printStages then
	    ignore (E.log "Stage 4.1: Removing unused temporaries\n") ;
	  Rmtmps.removeUnusedTemps memsafe_;
	end;
	
	begin
	  match !memsafeChannel with
	      None -> ()
	    | Some(x) ->
		(* Stats.time "printCil" *) (
		  C.dumpFile C.defaultCilPrinter x) memsafe_;
	end ;
	memsafe_
    in

    (**********************************************************************
      * STAGE 5
      *
      * Optimize the transformed output. 
    **********************************************************************)
    let optim = 
      let optim_ = match !doOpt with
	  true ->
	    if !printStages then 
	      ignore (E.log "Stage 5: Optimization\n") ;
	    Optim.optimFile memsafe 
	| false -> memsafe
      in
	begin 
	  match !optChannel with
	      None -> () 
	    | Some(c) -> 
		(* Stats.time "printOptim" *) (
		  C.dumpFile C.defaultCilPrinter c) optim_;
	end ; 
	(* Return the optimized version *)
	optim_
    in
      
      if !printStages then ignore (E.log "CMemSafe complete\n");

      begin 
	match !defaultChannel with
	    None -> ()
	  | Some(x) -> 
	      (* Stats.time "printCil" *) (C.dumpFile C.defaultCilPrinter x) optim;
      end ; 
      () 
end ;; 

(* this mystery code was copied from George's original main.ml *)
let failed = ref false 
let wrapMain () = 
  (* This code implements a "try finally clause". We first run the "main" and 
   * we produce a continuation *)
  let term = 
    try 
      main (); 
      (* Main did not throw an exception. The continuation just exits *)
      fun () -> exit (if !failed then 1 else 0)
    with 
        Cil.SizeOfError t -> begin
          (fun () ->
             ignore (E.log "Exception: SizeOfError(%a) at %a\n" 
                       C.d_type t C.d_loc !C.currentLoc);
             exit 2)
        end
      | e -> begin
          (* Main did throw an exception. Print a message about it 
             and then exit *)
          (fun () -> 
             print_string ("Uncaught exception: " ^ (Printexc.to_string e)
                           ^ "\n");
             exit 2)
        end
  in
  begin
    if !E.verboseFlag || !printStats then
      Stats.print stderr "Timings:\n";
    if !E.logChannel != stderr then 
      close_out (! E.logChannel);  
    (match ! cilChannel with Some (c) -> close_out c | _ -> ());
    (match ! mergeChannel with Some (c) -> close_out c | _ -> ());
    (match ! globinitChannel with Some (c) -> close_out c | _ -> ());
    (match ! memsafeChannel with Some (c)-> close_out c | _ -> ());
    (match ! optChannel with Some (c)-> close_out c | _ -> ());

    term ()
  end
;;

Printexc.catch wrapMain ()
;;

