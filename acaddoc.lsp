;;; this code, added to your acaddoc.lsp file, will show the current measure unit (insunits) in the taskbar
(SETVAR "modemacro" "UNITS: $(index,$(getvar,insunits),\"-,in,ft,mi,mm,cm,m,km,µin,mil,yd,A,nm,µm,dm,dam,hm,Gm,au,ly,pc\")
