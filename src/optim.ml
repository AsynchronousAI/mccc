
open Pretty
open Cil

module E = Errormsg
module CC = Csafechecks

let infoAttrs = [ CC.astrInfoComp; 
		  CC.astrInfoShadowComp; 
		  CC.astrInfoReturn; 
		  CC.astrInfoFun ];


class dropInfoAttrVisitorClass = object
  inherit nopCilVisitor

  method vattr (a: attribute) = begin
    let aname = match a with Attr (s, _) -> s in
      if List.exists (fun s -> (s == aname)) infoAttrs then begin
	ChangeTo ([])
      end
      else begin
	SkipChildren
      end
  end			  

end

class dropDeepInfoAttrVisitorClass = object
  inherit nopCilVisitor

  method vglob (g:global) = begin
    match g with
	GCompTag(cinfo, a) ->
	  List.iter (
	    fun a -> (cinfo.cattr <- dropAttribute a cinfo.cattr)
	  ) infoAttrs;
	  ChangeTo ([GCompTag(cinfo, a)])
      | _ ->
	  DoChildren
  end

  method vtype (t:typ) = begin
    match t with
	TComp(cinfo, a) ->
	  List.iter (
	    fun a -> (cinfo.cattr <- dropAttribute a cinfo.cattr)
	  ) infoAttrs;
	  ChangeTo (TComp(cinfo, a))
      | _ ->
	  DoChildren
  end


end

let dropInfoAttrVisitor = new dropInfoAttrVisitorClass
let dropDeepInfoAttrVisitor = new dropDeepInfoAttrVisitorClass

let optimFile (f: file) : file = 
  visitCilFileSameGlobals dropInfoAttrVisitor f;
  visitCilFileSameGlobals dropDeepInfoAttrVisitor f;
  f 
