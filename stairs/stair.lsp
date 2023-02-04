;;; ****Stairs.lsp  Version 1.0
;;; ****(C) Copyright 1997 by <DETOUR>  Dan E. Thomas
;;;
;;;     Stairs.lsp:  This routine will calculate and draw stair sections
;;; based on information you supply from your keyboard and graphics screen.
;;; Stairs.lsp draws in the current UCS, current linetype, current color,
;;; and on the current layer.
;;;     To figure stairs, first determine the total height, then divide
;;; that height by a desired riser height.  Round that result up or down
;;; to the nearest integer to yield the actual number of risers.  Divide
;;; total height by the number of risers to yield the size of each riser.
;;;     There are four rules for stairs that Stairs.lsp will employ.
;;; They are:
;;;         1) Riser + tread = 17" to 17.5"	+++++++++++++ 43 to 48 cm !!!!
;;;         2) 2x riser + tread = 24" to 25" ************** 61 to 64 cm !!!!!!!!!
;;;         3) Riser x tread = 70" to 75" **** 490 510 CM!!!!!!!!!!! sostituire con 1p-1a 12cm
;;;         4) Stairs should be 29 degrees to 35 degrees
;;;     After loading stairs.lsp, type "stairs" to activate.
;;; This routine will prompt you to pick a point on the lower level.
;;; Osmode is set to nearest for this pick so you can pick anywhere.
;;; Next you will pick the top step.  Osmode is set to endp., int. for
;;; this pick. It will then prompt for stair direction and nosing 
;;; style (square or round).  You will then be able to choose to accept
;;; defaults or customize.  If you choose to accept defaults, this
;;; routine will do division, round numbers and draw stairs that will
;;; meet the tests mentioned above. We can assume that all stairs are
;;; constrained vertically, going from one floor to another or from
;;; a porch to a sidewalk etc.  In some cases stairs are also constrained
;;; horizontally. If you choose to customize, you will be asked if you
;;; need to consider horizontal constraints.  If you answer yes, the 
;;; routine will prompt you to pick a point for the bottom riser after  
;;; the number of risers has been determined.  If that is your choice,
;;; the routine will draw stairs that fit in that space, and display the
;;; angle of the stairs.  However, there is no guarantee your stairs
;;; will meet the tests mentioned above.  In any case, if you choose to
;;; customize, you can enter your own number of risers, tread thickness,
;;; and nosing overhang. The routine will divide the height determined 
;;; from the two points by the desired riser height you enter, display 
;;; that result and ask how many risers you want.  It will display 
;;; the actual riser height. You will then be asked for a tread size if 
;;; you did not choose to consider horizontal constraints.
;;; A range of recommended tread sizes will be displayed.  They will
;;; be the minimum and maximum treads that will yield a stair to meet
;;; the tests outlined above. A default will be displayed. The default 
;;; tread size will be the average of the median values from the first 
;;; three rules above, if it meets the fourth rule.  If not, the default
;;; will be the tread size required to make a stair at 32.5 degrees. You  
;;; can accept it or not.  It will prompt for nosing size (tread 
;;; thickness & nozing overhang).  Defaults have been set for these.  
;;; Square tread thickness is set at 1.5" (as if a "2 by" is going to be
;;; used as a tread) and the overhang is 1.25".  Round nosing defaults 
;;; are both set at 1.25 inches.  You can accept them or not.  If you
;;; don't want any nosing, choose square nosing, and enter zero for 
;;; both tread thickness and nosing overhang.  Stairs.lsp will display
;;; riser, tread, and angle information at the command line and prompt
;;; if you want to print that information on your drawing.  If you
;;; print it on the drawing, the text will be the current text style
;;; and size if the current text style has a size assigned.  If it
;;; doesn't, the text will have a size of 6.  Stairs.lsp always draws 
;;; stairs from the top down.
;;;     Stairs.lsp has its own error handler that will display an error
;;; message and then exit, resetting all ACAD variables and setting
;;; stairs.lsp variables to nil.  It will restore the original error
;;; handler.  However, if you find it necessary to cancel this routine,
;;; just "Control C" one time so the error handler will have time to do
;;; what it has to do.  This "Control C" business may only work in windows if
;;; you have your keyboard preferences set to Autocad classics.  Otherwise
;;; you may must "Esc" to cancel the routine.
;;;     This routine was written by a true amateur.  It is the intellectual
;;; property of DETOUR.  Outrageous fees may be charged by him for use
;;; of this routine.
;;; NO WARRANTIES EXPRESSED OR IMPLIED!!  NO RECOURSE!!   <DETOUR DAN>
;;; Notes follow ( ;-> ).

;;;------------------------------------------------------------------------------;;;
;;;
;;;-> define internal error handler: if cancelled or on error, 
;;;-> display error msg. and exit

(defun str_err (msg)

  (if (/= msg nil)
    (progn
      (princ (strcat "\nError: " msg))
      (princ)
      (str_normex)
      (str_varnil)
    )					;progn
  )					;if
)					;defun

;;;----------------------------------------------------------------------------;;; 

;;;-> resets ACAD variables (normal exit)

(defun str_normex ()

  (setvar "lunits" slun)
  (setvar "luprec" slprec)
  (setvar "osmode" sosm)
  (setvar "plinewid" splw)
  (setvar "cmdecho" scmd)
  (setvar "textsize" stxtsz)
  (setq *error* old_error)
  (setq old_error nil)

)					;defun

;;;---------------------------------------------------------------------------;;;

;;;-> set all stairs.lsp variables to nil   

(defun str_varnil ()

  (foreach varlst '(ff	    th	    apx	    dvsr    tavg    mintrd
		    midtrd  maxtrd  nzh	    nzl	    slun    slprec
		    sosm    splw    nor	    lft	    rt	    dn
		    rht	    trdflt  tsiz    tdir    nostp   step
		    nostp   rnded   dflts   pt1	    pt2	    pt3
		    pt4	    pt5	    strshp  scmd    ang	    dist
		    horiz   botris  stxtsz  txtpt   prntxt  fixtrd
		    lenght
		   )
    (set varlst nil)
  )					;foreach
  (princ)
)					;defun

;;;---------------------------------------------------------------------------;;;

;;;-> define math functions

(defun dtr (a)
  (* pi (/ a 180.0))
)

(defun tan (a)
  (/ (sin a) (cos a))
)

(defun rnd (n / fn sfn)
  (setq fn (fix n))
  (setq sfn (- n fn))
  (if (>= sfn 0.5)
    (setq n (+ fn 1))
    (setq n fn)
  )					;if
)					; defun

;;;---------------------------------------------------------------------------;;;


(defun str_setup ()

;;;-> set variables

  (str_varnil)				;-> set stairs.lsp variables to nil
  (setq	old_error *error*		;-> trap old error handler
	*error*	str_err
  )					;-> set error handler
  (setq slun (getvar "lunits"))
  (setvar "lunits"
	  2				;-> decimal
  )
  (setq slprec (getvar "luprec"))
  (setvar "luprec"
	  4				;-> four decimal places
  )
  (setq splw (getvar "plinewid"))
  (setvar "plinewid"
	  0.0				;-> set polyline width
  )
  (setq	sosm (getvar "osmode")
  )
  (setq scmd (getvar "cmdecho"))
  (setvar "cmdecho"
	  0				;-> set cmdecho off
  )
  (setq stxtsz (getvar "textsize"))
  (setvar "textsize"
	  5				;-> set textsize to 5
  )

)					;defun

;;;---------------------------------------------------------------------------;;; 

;;;-> call other info functions

(defun str_info	()

  (str_ht)				;1
  (str_dir)				;2
  (str_shp)				;3
  (str_default)				;4
  (str_ris_ht)				;5
  (str_trd_siz)				;6
  (str_trd_th)				;7     
  (str_nozlen)				;8

)					;defun    

;;;---------------------------------------------------------------------    

;;;->determine total height #1 from info

(defun str_ht (/ ;|ff|;)		;-> first floor    

  (princ "\nDetermine Overall Height...")
  (setvar "osmode" 512)			;-> set osnap to nearest
  (initget 9)
  (setq ff (getpoint "\nPick Any Point On The Lower Level: "))
  (setvar "osmode" 33)
  (initget 9)
  (setq pt1 (getpoint "\nPick The Top Step: "))
  (setq th (- (cadr pt1) (cadr ff)))
  (setq txtpt (polar pt1 (/ pi 2.0) 36))

)					;defun    

;;;-------------------------------------------------------------------------    

;;;->determine stair direction #2 from info !!!!sostituito con direzione pick

(defun str_dir ()

  (princ "\nDetermine Stair Direction...")
  (initget 1 "Left Right")
;;; !!!!!!!!! qui provo ad automatizzare il destra/sinistra OK !!

  (if (> (car ff) (car pt1))
    (setq tdir "R")
  )
  (if (< (car ff) (car pt1))
    (setq tdir "L")
  )
  (if (= (car ff) (car pt1))
    (setq
      tdir (getkword "\nStairs Go Down To Right or Left? (R or L) ")
    )
  )
  (princ "tdir LR = ")
  (princ tdir)				;!if
  (if (= (substr tdir 1 1) "R")
    (setq tdir nil)
    (setq tdir 1)

  )
  (princ ff)
  (princ pt1)
  (princ " tdirNum = ")
  (princ tdir)				;if

)					;defun    

;;;---------------------------------------------------------------------

;;;->determine stair shape #3 from info

(defun str_shp ()

  (princ "\nDetermine Nosing Shape...")
  (initget 6 "Square Round")
  (setq
    strshp (getkword
	     "\nDo You Want Square Or Round Nose? (S or R) <Square>"
	   )
  )
  (if (or
	(= strshp nil)
	(= (substr strshp 1 1) "S")
      )					;or
    (setq strshp nil)
    (setq strshp 1)
  )
)					;defun  stair shape  

;;;-------------------------------------------------------------------------;;;    

;;;->accept defaults or customize #4 from info

(defun str_default ()

  (princ "\nSelect Design Method...")
  (initget 6 "Customize Defaults")
  (setq dflts (getkword "\nCustomize or Defaults (C or D) <Defaults>"))
  (if (or
	(= dflts nil)
	(= (substr dflts 1 1) "D")
      )					;or
    (setq dflts 1)
    (setq dflts nil)
  )					;if
)					;defun str_defaults

;;;-------------------------------------------------------------------------;;;

;;;->determine riser height #5 from info !!! CALCOLO ALZATA

(defun str_ris_ht (/
		   dvsr			;->desired riser height (divisor)
		   apx
		   rnded
		  )			;->rounded apx

  (princ "\nDetermine Riser Height...")
  (if dflts
    (progn				;then
      (setq nor (rnd (/ th 16.5)))	;!!!!!!!!!!!ERA 7
      (setq rht (/ th nor))
    )					;progn then
    (progn				;else
      (initget 6)
      (setq dvsr (getreal (strcat
			    "\nDesired Riser Height Is: <16.5> "
			  )
		 )
      )
      (if (= dvsr nil)
	(setq dvsr 16.5)		;!!!!!!!!!!!ERA 7'
      )					;if
      (setq apx (/ th dvsr))
      (princ (strcat
	       "\nTotal Height Divided By "
	       (rtos dvsr 2 4)
	       " is: "
	       (rtos apx)
	     )
      )					;princ
      (setq rnded (rnd apx))
      (initget 6)
      (setq nor	(getint	(strcat
			  "\nHow Many Risers Do You Want?: <"
			  (itoa rnded)
			  "> "
			)
		)
      )
      (if (= nor nil)
	(setq nor rnded)
      )					;if
      (setq rht (/ th nor))
      (princ (strcat "\nRiser Height Is: " (rtos rht 2 5) " cm"))
    )					;progn else
  )					;if
)					;defun str_ris_ht

;;;------------------------------------------------------------------------

;;;->determine tread size #6 from info

(defun str_trd_siz (/ horiz		;-> horizontal constraints y or n
		    fixtrd		;-> fixed tread size y or n
		    botris		;-> point for bottom riser
		    tavg		;-> average of first three tests
		    mintrd		;-> tread size @ 35 degrees
		    midtrd		;->   "    "   @ 32.5  "
		    maxtrd		;->   "    "   @ 30    "
		    trdflt)		;-> default tread size

;;;->if constrained horizontally, set tread size to fit !!! OR to ask for a fixed value !!!




  (princ "\nDetermine Tread And Nosing Size...")
  (if (not dflts)			;customize
    (progn				;then
      (initget 6 "Yes No")
      (setq horiz (getkword
		    "\nHorizontal Constraints? (Y or N): <N>"
		  )
      )
      (if (or
	    (= horiz nil)
	    (= (substr horiz 1 1) "N")
	  )				;or
	(setq horiz nil)
	(setq horiz 1)
      )					;if
    )					;progn then
  )					;if not dflts    

  (if horiz
    (progn				;then
;;; provo a inserire l'opzione fireescape>ask Thread !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;; oppure dare scelta tra pedata e lunghezza totale rampa
      (initget 6 "T S")
      (setq fixtrd
	     (getkword
	       "\nFixed Thread size or Stair lenght? (T or S): <T>"
	     )
      )
      (if (or
	    (= fixtrd nil)
	    (= (substr fixtrd 1 1) "T")
	  )				;or
	(setq fixtrd 1)
	(setq fixtrd nil)
      )					;if

;;; fine tentativo !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      (if fixtrd
	(progn
	  (initget 70)
	  (setq tsiz (abs (getdist "n\Tread size (Pedata)? cm :")))
	)				;else
	(progn
	  (setvar "osmode" 33)
	  (initget 9)
	  (setq botris (getpoint "\nPick Point For Bottom Riser:"))
	  (setq tsiz (abs (/ (- (car pt1) (car botris)) (- nor 1))))
	)				;progn
      )					;if
    )					;progn then

;;;->first three rules of stairs average   !!!! portare a centimetri 

    (progn				;else
      (setq tavg (- 63 (* 2 rht))
		 ;|formula originale (/ (+ (- 48 rht) (- 63 (* 2 rht)) (+ 12 rht)) 3)|;
      )

;;;->test for angle of stairs and set tread size default   

      (setq mintrd (/ rht (tan (dtr 38)))) ; !! 35
      (setq midtrd (/ rht (tan (dtr 29))))
					; !! 32.5 (gradino 17/29 30.379)
      (setq maxtrd (/ rht (tan (dtr 22)))) ; !! 30
      (if (and
	    (>= tavg mintrd)
	    (<= tavg maxtrd)
	  )				;and
	(setq trdflt tavg)
	(setq trdflt midtrd)
      )					;if
      (if dflts
	(setq tsiz trdflt)		;then
	(progn				;else

;;;->display range of tread sizes for 27-35 degree stair

	  (princ "\nFor Stair Between 27 and 35 Degrees...")
	  (princ (strcat
		   "\nTread Should Be (Alzata): "
		   (rtos mintrd 2 4)
		   " cm to "
		   (rtos maxtrd 2 4)
		   " cm"
		 )
	  )				;princ
	  (initget 6)
	  (setq
	    tsiz (getreal
		   (strcat
		     "\nEnter Tread Size (Alzata) In Cm: <"
		     (rtos trdflt 2 4)
		     ">"
		   )
		 )
	  )
	  (if (= tsiz nil)
	    (setq tsiz trdflt)
	  )				;if
	)				;progn else
      )					;if dflts
    )					;progn
  )					;if horiz   

;;;->display angle of stairs        

  (princ (strcat
	   "\nAngle Of Stairs Is: "
	   (angtos (atan rht tsiz) 0 2)
	   "°"
	 )
  )					;princ    

)
;;defun

;;;---------------------------------------------------------------------------;;; 

;;;->determine tread thickness #7 from info !!! (spessore sormonto)

(defun str_trd_th ()

  (if (and
	(= dflts 1)
	(not strshp)
      )					;and
    (setq nzh 2)
  )					;if

  (if (and
	(not dflts)
	(not strshp)
      )					;and
    (progn				;then
      (initget 4)
      (setq nzh
	     (getreal
	       (strcat
		 "\nEnter Tread Thickness (Spessore sormonto) In Cm: <2> "
	       )
	     )
      )
      (if (= nzh nil)
	(setq nzh 2)
      )
    )					;progn then
  )					;if

  (if (and
	(= dflts 1)
	(= strshp 1)
      )					;and
    (setq nzh 2)
  )					;if

  (if (and
	(not dflts)
	(= strshp 1)
      )					;and
    (progn				;then
      (initget 6)
      (setq nzh
	     (getreal
	       (strcat
		 "\nEnter Tread Thickness (Spessore sormonto) In Cm: <2> "
	       )
	     )
      )
      (if (= nzh nil)
	(setq nzh 2)
      )
    )					;progn then
  )					;if
)					;defun

;;;--------------------------------------------------------------------------

;;;->determine nosing overhang #8 from info !!!(sormonto)

(defun str_nozlen ()

  (if dflts
    (setq nzl 2)			;then !!! in pollici era 1.25
    (progn				;else
      (if (and
	    (not dflts)
	    (not strshp)
	  )				;and
	(initget 4)
      )					;if
      (if (and
	    (not dflts)
	    (= strshp 1)
	  )				;and    
	(initget 6)
      )					;if      
      (setq nzl
	     (getreal (strcat
			"\nEnter Nosing Overhang (Sormonto) In Cm: <2> "
		      )
	     )
      )
      (if (= nzl nil)
	(setq nzl 2)			;!!! in pollici era 1.25
      )					;if
    )					;progn else
  )					;if dflts
)					;defun

;;;--------------------------------------------------------------------------

;;;-> draw steps

(defun str_draw	(/ pt2			;->points for polylines
		 pt3			;->   "    "      "
		 pt4			;->   "    "      "
		 pt5			;->   "    "      "
		 lft			;-> left for polar
		 rt			;-> right      "
		 dn			;-> dn         "
		 ang			;-> angle      "
		 dist			;-> distance   "
		 nostp			;-> counter for while loop
		 step			;-> entity to copy
		 step0)			;-> entity to join

  (if (not tdir)
    (progn				;then
      (setq rt 0.0)
      (setq lft pi)
    )					;progn                       
    (progn				;else
      (setq rt pi)
      (setq lft 0.0)
    )					;progn                       
  )					;if
  (setq dn (* pi 1.5))
  (if strshp
    (setq nzl (- nzl (* nzh 0.5)))	;->subtract radius   
  )					;if                  ;  from nosing
  (setvar "osmode" 0)			;-> set osnap to none
  (setq pt2 (polar pt1 rt nzl))
  (setq pt3 (polar pt2 dn nzh))
  (setq pt4 (polar pt3 lft nzl))
  (if (not strshp)
    (command "_.pline" pt1 pt2 pt3 pt4 "")
    (command "_.pline" pt1 pt2 "_arc" pt3 "_line" pt4 "")
  )					;if
  (setq pt1 (polar pt1 dn rht))
  (setq pt2 (polar pt1 rt (+ tsiz nzl)))
  (setq pt3 (polar pt2 dn nzh))
  (setq pt4 (polar pt3 lft nzl))
  (setq pt5 (polar pt4 dn (- rht nzh)))
  (if (not strshp)
    (command "_.pline" pt1 pt2 pt3 pt4 pt5 "")
    (command "_.pline" pt1 pt2 "_arc" pt3 "_line" pt4 pt5 "")
  )					;if
  (setq ang (angle pt1 pt5))
  (setq dist (distance pt1 pt5))
  (setq nostp (- nor 2))		;-> set counter for while loop
  (if (> nostp 50)
    (setq nostp 50)
  )					;-> draws maximum 50 steps

  (setq step0 (entlast))

  (setq lenght (abs (* (- 1 nor) tsiz))) ; lunghezza finale rampa

  (while (>= nostp 1)
    (setq pt1 pt5)
    (setq step (entlast))
    (setq pt5 (polar pt1 ang dist))
    (command "_.copy" step "" pt1 pt5)
    (setq nostp (1- nostp))

;;; cerco di unirle
    (command "_.PEDIT" step0 "_Join" step0 step "" "") ;
  )					;while
  (setq step (entlast))
  (command "_.PEDIT" step0 "_Join" step0 step "" "")
)					;defun

(setvar "CMDECHO" 0)

;;;---------------------------------------------------------------------------;;; 

;;;->display properties of stairs at command line and prompt if you
;;;->want to print that information on the drawing.

(defun str_display (/ prntxt)

  (initget "Yes No")
  (setq	prntxt (getkword
		 "\nPrint Stair Dimensions On Drawing? (Y or N) <Y>"
	       )
  )
  (if (or
	(= prntxt nil)
	(= prntxt "Yes")
      )					;or
    (setq prntxt 1)
  )					;if

  (if (= prntxt 1)
    (progn
      (if (= (cdr (assoc 40
			 (tblsearch "style" ;if no fixed
				    (getvar "textstyle")
			 )
		  )
	     )
	     0.0
	  )				;text height
	(command "_.text"
		 txtpt
		 ""
		 ""			;then
		 (strcat "Altezza totale: " (rtos th 2 2) " cm")
	)				;!! (rtos th 4 4) !! conversione in pollici rimossa
	(command "_.text"
		 txtpt
		 ""			;else
		 (strcat "Altezza totale: " (rtos th 2 2) " cm")
	)				;!! (rtos th 4 4)

      )					;if					;if
      (command "_.text"
	       ""
	       (strcat "Lunghezza totale " (rtos lenght 2 2) " cm")
      )
      (command "_.text"
	       ""
	       (strcat (itoa nor) " Alzate @ " (rtos rht 2 2) " cm")
      )					;!! rtos rht 5 5                     
      (command
	"_.text"
	""
	(strcat (itoa (- nor 1)) " Pedate @ " (rtos tsiz 2 2) " cm")
      )					;!! rtos tsiz 5 4
      (command "_.text"
	       ""
	       (strcat "Angolo: "
		       (angtos (atan rht tsiz) 0 2)
		       "° - 2A+P="
		       (rtos (+ rht rht tsiz) 2 3)
	       )
      )
    )					;progn
  )					;if

  (princ (strcat
	   "\nAltezza totale: "
	   (rtos th 2 2)
	   " cm | "
	 )
  )

  (princ (strcat
	   "\nLunghezza totale: "
	   (rtos lenght 2 2)
	   " cm | "
	 )
  )					;!! rtos th 4 4
  (princ (strcat
	   "\n"
	   (itoa nor)
	   " Alzate @ "
	   (rtos rht 2 2)
	   " cm | "
	 )
  )					;!! (rtos rht 5 5)
  (princ (strcat
	   (itoa (- nor 1))
	   " Pedate @ "
	   (rtos tsiz 2 2)
	   " cm | "
	 )
  )					;!! (rtos tsiz 5 4)
  (princ (strcat
	   "Angolo "
	   (angtos (atan rht tsiz) 0 2)
	   "° | "
	 )
  )
  (princ (strcat
	   "2A+P = "
	   (rtos (+ rht rht tsiz) 2 3)
	   " cm"
	 )
  )

)					;defun

;;;---------------------------------------------------------------------------;;;

;;;-> defines ACAD command "stairs"

(defun c:stair (/      slun		;-> trap lunits [memorizza formato unità]
		slprec			;-> trap lunits precision [memorizza precisione]
		sosm			;-> trap osnap setting [memorizza stato Osnap]
		splw			;-> trap plinewidth [memorizza plinewidth]
		scmd			;-> trap cmdecho [memorizza cmdecho]
		stxtsz			;-> trap textsize [memorizza altrezza testo]
		nor			;-> number of risers [numero alzate]
		rht			;-> riser height [altezza alzate]
		tsiz			;-> tread size [misura pedate]
		nostp			;-> number of steps [numero gradini]
		strshp			;-> stair shape (round or square nose) [forma del naso]
		th			;-> total height [altezza totale]
		pt1			;-> top step [punto inserimento (in alto)]
		txtpt			;-> point for text on drawing [inserimento testo]
		tdir			;-> stair direction [direzione scale]
		dflts			;-> defaults or customize flag [flag scala default/personalizza]
		nzh			;-> nosing height (tread thickness) [altezza naso]
		nzl			;-> nosing overhang length [sormonto]
		lenght			;-> total stair lenght [lunghezza totale scale]
	       )

  (prompt
    "\nStairs.lsp by DETOUR (C) Copyright By Dan E. Thomas 1997, edited and ported to CM by Andrea Ricci 2005"
  )
  (str_setup)				;input parametri scala
  (str_info)				;elaborazione parametri dall'input
  (str_draw)				;tracciato disegno
  (str_display)				;display dei dati
  (str_normex)				;stivaggio e restituzione impostazioni variabili
  (princ)

)					;defun
(princ
  "\nType STAIR to draw a stair section outline (works in cm)"
)

;;;---------------------------------------------------------------------------;;;

;;;-> END OF STAIRS.LSP  Thank You

;;;---------------------------------------------------------------------------;;;
