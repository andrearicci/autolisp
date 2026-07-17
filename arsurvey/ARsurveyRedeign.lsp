;;; *****************************************************************************************************
;;; ARsurvey v5.2 - FIXED VERSION (Layer Creation & Safety)
;;; 

; difetti: non funziona il preview, non chiede l'orientamento, cancella la preview senza lasciare un oggetto

;;; Copyright (C) 2026 Andrea Ricci
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;;
;;; Author: Andrea Ricci
;;; Version: 5.2


*****************************************************************************************************

(vl-load-com)

;; --- Global Error Handler ---
(defun *error* (msg) 
  (clear-preview-safe)
  (setvar "CMDECHO" 1)
  (if 
    (not 
      (member msg 
              '("Function cancelled" "quit / exit abort" "\nConvenience break")
      )
    )
    (princ (strcat "\n*** Error: " msg " ***"))
  )
  (princ)
)

;; --- Safe Cleanup of Preview ---
(defun clear-preview-safe (/ ss i) 
  ;; Assicuriamoci che il layer esista prima di cercare oggetti
  (if (tblsearch "LAYER" "AR_PREVIEW") 
    (progn 
      (setq ss (ssget "_X" (list '(0 . "LWPOLYLINE") (cons 8 "AR_PREVIEW"))))
      (if ss 
        (repeat (setq i (sslength ss)) 
          (entdel (ssname ss (setq i (1- i))))
        )
      )
    )
    ;; Se il layer non esiste, non c'è nulla da pulire, niente warning
  )
)

;; --- Create Layer if missing ---
(defun ensure-preview-layer (/ acadObj doc space) 
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-activedocument acadObj))
  (if (not (tblsearch "LAYER" "AR_PREVIEW")) 
    (vla-add (vla-get-layers doc) "AR_PREVIEW")
  )
)

;; --- Utility Functions ---
(defun rad2grad (r) (* r (/ 180.0 pi)))

(defun calc-angle (sideA sideB sideOpposite / p) 
  (setq p (/ (+ sideA sideB sideOpposite) 2.0))
  (* 2.0 
     (atan 
       (sqrt 
         (/ 
           (* (- p sideA) (- p sideB))
           (* p (- p sideOpposite))
         )
       )
     )
  )
)

(defun valid-triangle? (a b c / max-side sum-others) 
  (setq max-side (max a b c))
  (setq sum-others (- (+ a b c) max-side))
  (> sum-others max-side)
)

;; Create/Update Preview
(defun update-preview (pt1 pt2 pt3 color / acadObj doc space coords sa plineObj) 
  (ensure-preview-layer) ; Garantisce che il layer esista
  (clear-preview-safe) ; Pulisce solo se esiste

  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-activedocument acadObj))
  (setq space (vla-get-modelspace doc))

  ;; Prepare coordinates list
  (setq coords (list 
                 (car pt1)
                 (cadr pt1)
                 (car pt2)
                 (cadr pt2)
                 (car pt3)
                 (cadr pt3)
               )
  )

  ;; Create variant and safearray
  (setq sa (vlax-make-safearray vlax-vbDouble '(0 . 5)))
  (vlax-safearray-fill sa coords)

  ;; Create polyline
  (setq plineObj (vla-addlightweightpolyline space (vlax-make-variant sa)))
  (vla-put-closed plineObj :vlax-true)
  (vla-put-layer plineObj "AR_PREVIEW")
  (vla-put-colorindex plineObj color)
  plineObj
)

;; Draw final triangle
(defun draw-final (pt1 pt2 pt3) 
  (entmake 
    (list 
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      (cons 90 3)
      '(70 . 1)
      (cons 10 pt1)
      (cons 10 pt2)
      (cons 10 pt3)
    )
  )
)

;; --- Main Command ---
(defun c:SRV (/ sA sB sC pA gamma lastAngle delta verso phase done gr key val 
              final-pt1 final-pt2 final-pt3
             ) 
  ;; Wrap entire command in error handler
  (vl-catch-all-apply 
    '(lambda () 
       ;; --- 1. Input Lati ---
       (princ "\n--- ARsurvey v5.2 (Fixed) ---")

       (setq lastAngle (getvar "ARsurvey_LastAngle"))
       (if (not lastAngle) (setq lastAngle 0.0))

       (initget 7)
       (setq sA (getdist "\nFirst side length: "))
       (if (not sA) (exit))

       (initget 7)
       (setq sB (getdist "\nSecond side length: "))
       (if (not sB) (exit))

       (initget 7)
       (setq sC (getdist "\nThird side length: "))
       (if (not sC) (exit))

       (if (not (valid-triangle? sA sB sC)) 
         (progn (alert "Invalid triangle sides.") (exit))
       )

       (setq gamma (calc-angle sA sB sC))

       ;; --- 2. Punto di inserimento ---
       (princ "\nSelect Insertion Point (use OSNAP)...")
       (setq pA (getpoint))
       (if (not pA) (exit))
       (princ 
         (strcat "\nPoint selected: " 
                 (rtos (car pA) 2 2)
                 ", "
                 (rtos (cadr pA) 2 2)
         )
       )

       ;; --- Setup State Machine ---
       (setq phase 1) ; 1 = Rotate, 2 = Orient
       (setq done nil)
       (setq delta lastAngle)
       (setq verso 1.0) ; 1 = CCW, -1 = CW

       ;; Initial calculation
       (setq final-pt1 (trans pA 1 0))
       (setq final-pt2 (polar final-pt1 delta sA))
       (setq final-pt3 (polar final-pt2 (+ delta (* verso gamma)) sB))

       ;; Crea la preview iniziale
       (update-preview final-pt1 final-pt2 final-pt3 8)
       (princ "\nPreview Active. Move mouse to rotate, Click/Enter to confirm angle.")

       ;; --- Main Loop ---
       (while (not done) 
         (setq gr (grread T 2 0))
         (setq key (car gr))
         (setq val (cadr gr))

         ;; Debug: stampa il codice del tasto premuto (opzionale, rimuovi se fastidioso)
         ;; (princ (strcat "\nKey: " (itoa key)))

         ;; Handle Phase 1: Rotation
         (if (= phase 1) 
           (cond 
             ;; ESC
             ((= key 2)
              (clear-preview-safe)
              (princ "\nCancelled.")
              (setq done T)
             )
             ;; Mouse Move
             ((= key 5)
              (setq delta (angle pA val))
              (setq final-pt1 (trans pA 1 0))
              (setq final-pt2 (polar final-pt1 delta sA))
              (setq final-pt3 (polar final-pt2 (+ delta (* verso gamma)) sB))
              (update-preview final-pt1 final-pt2 final-pt3 8)
             )
             ;; Click or Enter -> Go to Phase 2
             ((or (= key 4) (= key 13))
              (setq phase 2)
              (princ "\n[Phase 2] Press 'T' to toggle CW/CCW, Enter/Click to confirm.")
              (princ (strcat " (Current: " (if (= verso 1.0) "CCW" "CW") ")"))
             )
           )
         )

         ;; Handle Phase 2: Orientation Choice
         (if (= phase 2) 
           (cond 
             ;; ESC
             ((= key 2)
              (clear-preview-safe)
              (princ "\nCancelled.")
              (setq done T)
             )
             ;; Toggle 'T'
             ((or (= key 84) (= key 116))
              (setq verso (- verso))
              (princ (strcat "\n  Orientation: " (if (= verso 1.0) "CCW" "CW")))
              ;; Recalculate only the third point
              (setq final-pt3 (polar final-pt2 (+ delta (* verso gamma)) sB))
              (update-preview final-pt1 final-pt2 final-pt3 8)
             )
             ;; Confirm
             ((or (= key 4) (= key 13))
              (clear-preview-safe)
              (draw-final final-pt1 final-pt2 final-pt3)
              (setvar "ARsurvey_LastAngle" delta)
              (princ 
                (strcat "\nTriangle drawn. Angle: " 
                        (rtos (rad2grad delta) 2 2)
                        " deg, Orientation: "
                        (if (= verso 1.0) "CCW" "CW")
                )
              )
              (setq done T)
             )
           )
         )
       )
     )
  )
  (princ)
)

;; Reset command
(defun c:SRVReset () 
  (setvar "ARsurvey_LastAngle" nil)
  (princ "\nOrientation memory reset.")
  (princ)
)

(princ "\nARsurvey v5.2 loaded. Type 'SRV' to start.")
(princ)