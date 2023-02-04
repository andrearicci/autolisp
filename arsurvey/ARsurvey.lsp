;;; *****************************************************************************************************
;;; ARsurvey v0.1.0 © 2009 Andrea Ricci http://andrearicci.it  <°)));
;;;
;;; IT
;;; tentativo 01 di creare una routine per semplificare l'immissioni di triangolazioni di rilievo
;;; il prodotto finale dovrebbe, date tre misure, verso, punto di inserimento e allineamento,
;;; tracciare un triangolo delle misure date nel punto e con l'allineamento dato
;;;
;;; si rilascia con licenza LGPL v3.0 <http://www.opensource.org/licenses/lgpl-3.0.html>
;;; per cui, fra l'altro, non è possibile ricavarne un prodotto commerciale, e qualsiasi
;;; derivato (modifiche, incorporamenti in altro codice) deve avere la medesima licenza
;;;
;;; EN
;;; draws a triangle given three sides, insertion point and alignement,
;;; intended to help in the boring survey procedure
;;; relased under the LGPL licence v3.0 <http://www.opensource.org/licenses/lgpl-3.0.html>
;;; *****************************************************************************************************

;;;	IT
;;;	uso:
;;;	avviare il programma digitando "srv" (da SuRVey).
;;;	Il programma chiede anzitutto tre misure (si può inserirle con mouse),
;;;	poi punto d'inserimento e direzione, infine chiede se il triangolo vada
;;;	tracciato in senso antiorario (default, basta dare l'invio)
;;;	oppure orario (premere C per "clockwise")
;;;
;;;	EN
;;;	use:
;;;	start the command with "srv" (as in SuRVey)
;;;	The routine will ask you for three sides' measures (you can input them by mouse),
;;;	then the insertion point, and the direction. Lastly it will ask you if you want
;;;	to draw it counterclockwise (default, just press "enter") or Clockwise (press "C")


;; cazzeggio bell'e buono
(defun signature ()
  (princ
    "\n   __    _  _  ____  ____  ____    __    ____  ____  ___  ___  ____
\r  /__\\  ( \\( )(  _ \\(  _ \\( ___)  /__\\  (  _ \\(_  _)/ __)/ __)(_  _)
\r /(__)\\  )  (  )(_) ))   / )__)  /(__)\\  )   / _)(_( (__( (__  _)(_ 
\r(__)(__)(_)\\_)(____/(_)\\_)(____)(__)(__)(_)\\_)(____)\\___)\\___)(____)"
  )
)

							   
;;per convertire radianti in gradi
(defun rad2grad	(radianti / gradi)
  (setq gradi (* radianti (/ 180 pi)))
  gradi
)
;; per convertire gradi in radianti
(defun grad2rad	(gradi / radianti)
  (setq radianti (* gradi (/ pi 180)))
  radianti
)
;; dati tre lati ricava angoli interni (formule di Briggs - http://www.math.it/formulario/goniometria.htm)
(defun briggs
	      (latoadiacente1 latoadiacente2 latoopposto / corner)
  (setq	corner				; angolo opposto al lato sideC
	 (abs
	   (*
	     (atan
	       (sqrt
		 (/
		   (*
		     (- p latoadiacente1)
		     (- p latoadiacente2)
		   )			;fine_numeratore
		   (* p
		      (- p latoopposto)
		   )			;fine_denominatore
		 ) ;_end frazione
	       ) ;_end radice quadrata
	     ) ;_end reciproco tangente
	     2
	   ) ;_end moltiplica per 2
	 ) ;_end abs (rimuove eventuale "-")
  )
  corner
) ;_end funzione briggs

;;; inizio main function
(defun c:srv ()				; mnemonico da SuRVey
  (signature);_he he he
  (princ
    "\nCommand SRV (SuRVey) v0.1.0 © 2009 Andrea Ricci http://andrearicci.it  _m__(*_°)__ooo____"
  )
;(setq lprecision (getvar "luprec"))	; memorizza la precisione lineare in uso per successivo ripristino
;(setvar "luprec" 8)
;(setq aprecision (getvar "auprec"))	; memorizza la precisione angolare in uso per successivo ripristino
;(setvar "auprec" 8)
(initget 103)				; non consente valore nullo (+1), zero (+2), negativo (+4), usa linea tratteggiata (+32), ignora Z (+64).
(setq sideA
       (getdist "\nfirst side lenght: ")
)
(initget 103)
(setq sideB
       (getdist "\nSecond side lenght: ")
)
(initget 103)
(setq sideC
       (getdist "\nThird side lenght: ")
)
  ;; controllo che sia un triangolo (la somma dei lati minori supera il lato maggiore?)
  ;; is the triangle possible?
  (if
    (or	(>
	  (max sideA sideB sideC)
	  (- (+ sideA sideB sideC) (max sideA sideB sideC))
	) ;_">" valuta se un valore è maggiore dell'elemento alla sua destra
	(=
	  (max sideA sideB sideC)
	  (- (+ sideA sideB sideC) (max sideA sideB sideC))
	) ;_oppure uguale
    ) ;_end or
     (progn
       (alert "sorry, not a triangle")
       ;;(setvar "luprec" lprecision) ;_resetta la precisione lineare originale
       ;;(setvar "auprec" aprecision) ;_resetta la precisione angolare originale
       (gc)
       (princ)
       (exit)
     ) ;_end progn
  ) ;_endif

  (initget 65)				;non consente valore nullo (+1), zero (+2), negativo (+4), usa linea tratteggiata (+32), ignora Z (+64).
  (setq	pointA				;origine triangolo
	 (getpoint "\nInsertion Point: ")
  )
  (initget 103)
  ;; angolo iniziale
  (setq	delta
	 (angle	pointA
		(getpoint (princ pointA) "\nFirst side direction")
					; (princ pointA) all'interno di getpoint serve a piazzare l'origine della linea tratteggiata
	 )
  )
  (princ (strcat " [angle= " (rtos (rad2grad delta) 2 8)"]"))

  (setq p (/ (+ sideA sideB sideC) 2))	; p= semiperimetro

  ;; angoli ottenuti con briggs' formula
  ;; (setq alpha (briggs sidec sideb sidea))
  ;; (setq beta (briggs sidea sidec sideb))
  (setq gamma (briggs sidea sideb sidec))
  (setq gamma (- pi gamma)) ;_correzione dell'angolo reciproco ****************************************
  
  ;; correzione del verso di tracciamento (default: antiorario) http://www.lee-mac.com/promptwithdefault.html
  (initget "Cw or ccW")
  (setq	dir
	 (getkword
	   "\nSpecify order of given measures [Cw/ccW (clockwise/counterclockwise)]: <ccW>"
	 )
  )
  (if (= dir "Cw")
    (setq verso -1)
    (progn
      (setq dir "ccW")
      (setq verso 1) ;_else
    ) ;_end progn
  ) ;_end if
  (princ dir)
  (setq dir nil) ;_resetting the variable for next use
  ;; end of triangle direction routine
  
  ;; punto B in coordinate relative da pointA e radianti
  (setq	pointB
	 (strcat "@"
		 (rtos sideA 2 8)
		 "<"
		 (rtos delta 2 8)
		 "r"
	 )
  )
  ;; punto C in coordinate relative da pointB e radianti
  (setq	pointC (strcat "@"
		       (rtos sideB 2 8)
		       "<"
		       (rtos (+ delta (* verso gamma)) 2 8);_corretion by direction->verso
		       "r"
	       )
  )

  ;; disegno della polilinea
  (setq snapmode (getvar "osmode"))
  (setvar "osmode" 0)
  (command "_.PLINE" pointA pointB pointC "_close")
  (setvar "osmode" snapmode)
  ;;
  ;(setvar "luprec" lprecision) ;_resetta la precisione lineare originale
  ;(setvar "auprec" aprecision) ;_resetta la precisione angolare originale

  ;; stampa le tre lunghezze
  (princ (strcat "\n1st side= "
		 (rtos sideA 2 8)
		 "; 2nd side= "
		 (rtos sideB 2 8)
		 "; 3rd side= "
		 (rtos sideC 2 8)
	 )
  )
  ;| per verifica stampo l'ultimo lato calcolato, corrisponde con il terzo inserito?
     verify: is the calculated last side equal to imput?|;
  (princ (strcat " [Last side: "
		 (rtos (distance (getvar "LASTPOINT") pointa) 2 8)
		 "]"
	 )
  )
) ;_end_defun
;;;
;;; ------------ Command Line Load Sequence--------------------------------------------
(princ "\nARsurvey v0.1.0 \n© Andrea Ricci, \n July, 2009....loaded.")
(princ
  "\nARsurvey v0.1.0
\r©  __    _  _  ____  ____  ____    __    ____  ____  ___  ___  ____
\r  /__\\  ( \\( )(  _ \\(  _ \\( ___)  /__\\  (  _ \\(_  _)/ __)/ __)(_  _)
\r /(__)\\  )  (  )(_) ))   / )__)  /(__)\\  )   / _)(_( (__( (__  _)(_ 
\r(__)(__)(_)\\_)(____/(_)\\_)(____)(__)(__)(_)\\_)(____)\\___)\\___)(____)
       \nhttp://andrearicci.it July, 2009....loaded."
)
(princ "Type \"SRV\" (SuRVey) to draw a triangle given tree sides, insertion point and alignment")
(print)
;;;;;;

(defun c:ricci ()
  (command "_browser" "http://andrearicci.it")
)
;;;  	.__.     .         .__         		;
;;;	[__]._  _|._. _  _.[__)* _. _.*		;
;;;	|  |[ )(_][  (/,(_]|  \|(_.(_.|		;

;;;	   __    _  _  ____  ____  ____    __    ____  ____  ___  ___  ____ 	.
;;;	  /__\  ( \( )(  _ \(  _ \( ___)  /__\  (  _ \(_  _)/ __)/ __)(_  _)	.
;;;	 /(__)\  )  (  )(_) ))   / )__)  /(__)\  )   / _)(_( (__( (__  _)(_ 	.
;;;	(__)(__)(_)\_)(____/(_)\_)(____)(__)(__)(_)\_)(____)\___)\___)(____)	.

;;;	   _             _                  __ _          _ 
;;;	  /_\  _ __   __| |_ __ ___  __ _  /__(_) ___ ___(_)
;;;	 //_\\| '_ \ / _` | '__/ _ \/ _` |/ \// |/ __/ __| |
;;;	/  _  \ | | | (_| | | |  __/ (_| / _  \ | (_| (__| |
;;;	\_/ \_/_| |_|\__,_|_|  \___|\__,_\/ \_/_|\___\___|_|

;;;	 /\  _  _| _ _  _ |~). _ _.
;;;	/~~\| |(_|| (/_(_||~\|(_(_|



;;;---------------------------------------------------------------------------;;;
;;; NOTES
;;; 
;;; trigonometria: http://discussion.autodesk.com/forums/message.jspa?messageID=6159454
;;;
;;; http://it.wikipedia.org/wiki/Formule_di_Briggs
;;;
;;;---------------------------------------------------------------------------;;;
