(vl-load-com)
;------------------------------------------------------------
; STAIR v1.0.0 - First release
; State Infrastructure
;  ✅ Geometry Engine frozen
;  ✅ UCS Independent
;  ✅ Tread submenu
;  ✅ Nosing submenu
;  ✅ Preview report
;  ✅ Final report su command line
;  ✅ Accept / Cancel
;  ✅ MTEXT report
;  ✅ added FIT (constrained) mode (from A to B)
;..    #### next step: add landings ####
;
; Stair section generator
;
;;; Copyright (C) 2026 Andrea Ricci con l'aiuto dell'A.I. (amico immaginario).
;;; https://andrearicci.it
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
;;; Version: v1.0.0

;------------------------------------------------------------
; STAIR - USER GUIDE
;------------------------------------------------------------
;
; PURPOSE
;
; Generates a stair section from two picked points.
; The stair is drawn as a lightweight polyline and can optionally generate a nosing and an MTEXT design report
;
; COMMAND
;
; STAIR
;     Main command.
;
;
; WORKFLOW
;
;  1) Pick Base Point
;  2) Pick Arrival Point
;  3) Review Preview
;  4) Modify stair parameters if needed
;  5) Accept or Exit
;
;
; MAIN MENU
;
;  [+/-/Tread/Nosing/Accept/Exit] <Accept>
;
;  +         Add one riser
;  -         Remove one riser
;  Tread     Tread settings
;  Nosing    Nosing settings
;  Accept    Create final geometry
;  Exit      Cancel command
;
;
; MENU TREE
;
; STAIR
; ├─ Pick Base Point
; ├─ Pick Arrival Point
; │
; ├─ Main Menu
; │   ├─ +
; │   │   └─ Add riser
; │   │
; │   ├─ -
; │   │   └─ Remove riser
; │   │
; │   ├─ Tread
; │   │   ├─ Value
; │   │   │   └─ Fixed tread value
; │   │   │
; │   │   ├─ Ergonomic
; │   │   │   └─ Automatic Blondel rule
; │   │   │
; │   │   ├─ Fit
; │   │   │   └─ Fit stair to picked run
; │   │   │
; │   │   └─ Accept
; │   │
; │   ├─ Nosing
; │   │   ├─ None
; │   │   │
; │   │   ├─ Square
; │   │   │   ├─ Nosing X
; │   │   │   └─ Nosing Y
; │   │   │
; │   │   ├─ Round
; │   │   │   └─ Diameter
; │   │   │
; │   │   └─ Cancel
; │   │
; │   ├─ Accept
; │   │   ├─ Final geometry
; │   │   └─ Optional MTEXT report
; │   │
; │   └─ Exit
; │       └─ Delete preview
; │
; └─ End
;
;
; TREAD MODES
;
; ERGONOMIC
;
;     Tread automatically calculated using:
;         2R + T ≈ 63 cm
;     according to drawing INSUNITS.
;
; FIXEDTREAD
;
;     User specifies a tread value.
;     Same value is remembered between sessions.
;
; FIT
;
;     Total run is constrained by picked points.
;
;     Formula:
;         tread = totalRun / (risers - 1)
;     where:
;         totalRun = abs(ep.x - bp.x)
;
;
; NOSING TYPES
;
; NONE
;     Standard stair profile.
;
; SQUARE
;     Rectangular nosing.
;     Parameters:
;         X projection
;         Y drop
;
;
; ROUND
;     Semicircular nosing.
;     Parameter:
;         Diameter
;
; REPORT
; Preview report is shown during editing.
; Final report contains:
;
;     Height
;     Run
;     Number of risers
;     Number of treads
;     Rise value
;     Tread value
;     2R+T
;     Stair angle
;
;
; MTEXT REPORT
; Optional.
;
; Automatically inserted:
;
;     - Above last step
;     - UCS aligned
;     - Height = rise / 3
;     - Remembers Yes/No preference
;
;
; NOTES
;
; - UCS independent.
; - insunits independent.
; - Run direction automatically detected.
; - Preview geometry is temporary.
; - Accept promotes preview to final geometry.
; - Exit deletes preview geometry.
;
;------------------------------------------------------------
; Commands:
; STAIR
;------------------------------------------------------------
;------------------------------------------------------------
; Runtime state
;------------------------------------------------------------
(setq *stair-doc* nil)
(setq *stair-preview* nil)
(setq *stair-mode* "ERGONOMIC")
(setq *stair-fixed-tread* 30.0)
(setq *stair-nosing-type* "NONE")
(setq *stair-bp* nil)
(setq *stair-ep* nil)
(setq *stair-height* 0.0)
(setq *stair-risers* 0)
(setq *stair-rise* 0.0)
(setq *stair-tread* 0.0)
(setq *stair-rundir* 1.0)
(setq *stair-report-mtext* "No")
(setq *stair-total-run* 0.0)
;------------------------------------------------------------
; Helpers
;------------------------------------------------------------
(defun stair:f2 (x) 
  (rtos x 2 2)
)
(defun stair:sign (x) 
  (if (>= x 0.0) 
    1.0
    -1.0
  )
)
(defun stair:round-int (x) 
  (fix (+ x 0.5))
)
;------------------------------------------------------------
; Unit conversion
;------------------------------------------------------------
(defun stair:cm->units (v / u) 
  (setq u (getvar "INSUNITS"))
  (* v 
     (cond 
       ((= u 0) 1.0)
       ((= u 4) 10.0)
       ((= u 5) 1.0)
       ((= u 6) 0.01)
       ((= u 1) 0.3937007874)
       ((= u 2) 0.03280839895)
       (T 1.0)
     )
  )
)
(defun stair:ergonomic-min () 
  (stair:cm->units 63.0)
)
(defun stair:ergonomic-max () 
  (stair:cm->units 64.0)
)
(defun stair:ideal-riser () 
  (stair:cm->units 16.5)
)
(defun stair:default-square-x () 
  (stair:cm->units 2.0)
)
(defun stair:default-square-y () 
  (stair:cm->units 2.0)
)
(defun stair:default-round-dia () 
  (stair:cm->units 2.0)
)
(setq *stair-nosing-x* (stair:default-square-x))
(setq *stair-nosing-y* (stair:default-square-y))
(setq *stair-fixed-tread* (stair:cm->units 30.0))
;------------------------------------------------------------
; Ergonomic helpers
;------------------------------------------------------------
(defun stair:get-height (bp ep) 
  ;; Height is always Delta Y in UCS
  (abs 
    (- (cadr ep) 
       (cadr bp)
    )
  )
)
(defun stair:propose-risers (height) 
  (max 
    2
    (stair:round-int 
      (/ 
        height
        (stair:ideal-riser)
      )
    )
  )
)
(defun stair:ergonomic-tread (rise) 
  (- 
    (stair:ergonomic-min)
    (* 2.0 rise)
  )
)
(defun stair:ergonomic-ok-p (rise tread) 
  (and 
    (>= 
      (+ (* 2.0 rise) tread)
      (stair:ergonomic-min)
    )
    (<= 
      (+ (* 2.0 rise) tread)
      (stair:ergonomic-max)
    )
  )
)
;------------------------------------------------------------
; Recompute stair state
;------------------------------------------------------------
(defun stair:recompute (/) 
  (setq *stair-rise* (/ 
                       *stair-height*
                       *stair-risers*
                     )
  )
  (cond 
    ((= *stair-mode* "ERGONOMIC")
     (setq *stair-tread* (stair:ergonomic-tread 
                           *stair-rise*
                         )
     )
    )
    ((= *stair-mode* "FIXEDTREAD")
     (setq *stair-tread* *stair-fixed-tread*)
    )
    ((= *stair-mode* "CONSTRAINED")
     (if (> (stair:tread-count *stair-risers*) 0) 
       (setq *stair-tread* (/ 
                             *stair-total-run*
                             (stair:tread-count 
                               *stair-risers*
                             )
                           )
       )
       (setq *stair-tread* 0.0)
     )
    )
  )
  (princ)
)
;------------------------------------------------------------
; Refresh preview
;------------------------------------------------------------
(defun stair:refresh-preview (/) 
  (if 
    (and 
      *stair-bp*
      (> *stair-risers* 1)
    )
    (progn 
      (stair:update-preview *stair-bp* *stair-risers* *stair-rise* *stair-tread* 
                            *stair-rundir*
      )
      (stair:preview-report 
        *stair-height*
        *stair-risers*
        *stair-rise*
        *stair-tread*
      )
    )
  )
  (princ)
)
;------------------------------------------------------------
; Nosing clamp
;------------------------------------------------------------
(defun stair:clamp-nosing (rise tread nosingType nx ny / maxX maxY changed) 
  (setq changed nil)
  (cond 
    ((= nosingType "SQUARE")
     (setq maxX (/ tread 2.0))
     (setq maxY (/ rise 2.0))
     (if (> nx maxX) 
       (progn 
         (setq nx maxX)
         (setq changed T)
       )
     )
     (if (> ny maxY) 
       (progn 
         (setq ny maxY)
         (setq changed T)
       )
     )
    )
    ((= nosingType "ROUND")
     (setq maxY (/ rise 2.0))
     (if (> ny maxY) 
       (progn 
         (setq ny maxY)
         (setq changed T)
       )
     )
    )
  )
  (if changed 
    (prompt "\nnosing dimensions out of range")
  )
  (list nx ny)
)
;------------------------------------------------------------
; Preview helpers
;------------------------------------------------------------
(defun stair:delete-preview (/) 
  (if 
    (and 
      *stair-preview*
      (= (type *stair-preview*) 'VLA-OBJECT)
    )
    (vl-catch-all-apply 
      'vla-delete
      (list *stair-preview*)
    )
  )
  (setq *stair-preview* nil)
  (princ)
)
;------------------------------------------------------------
; Geometry builders
;------------------------------------------------------------
(defun stair:step-none (x y tread rise runDir /) 
  (list 
    (list 
      (list x (+ y rise) 0.0)
      0.0
    )
    (list 
      (list (+ x (* runDir tread)) 
            (+ y rise)
            0.0
      )
      0.0
    )
  )
)
(defun stair:step-square (x y tread rise nx ny runDir /) 
  (list 
    ;; Top of riser minus nose height
    (list 
      (list 
        x
        (+ y (- rise ny))
        0.0
      )
      0.0
    )
    ;; Nose projection
    (list 
      (list 
        (+ x (* runDir (- nx)))
        (+ y (- rise ny))
        0.0
      )
      0.0
    )
    ;; Nose top
    (list 
      (list 
        (+ x (* runDir (- nx)))
        (+ y rise)
        0.0
      )
      0.0
    )
    ;; Tread
    (list 
      (list 
        (+ x (* runDir tread))
        (+ y rise)
        0.0
      )
      0.0
    )
  )
)
(defun stair:step-round (x y tread rise dia runDir / bulge) 
  ;; Vertical semicircular nosing
  (setq bulge (if (> runDir 0.0) 
                -1.0
                1.0
              )
  )
  (list 
    ;; Start of vertical diameter
    (list 
      (list 
        x
        (+ y (- rise dia))
        0.0
      )
      bulge
    )
    ;; End of diameter / top of riser
    (list 
      (list 
        x
        (+ y rise)
        0.0
      )
      0.0
    )
    ;; Tread
    (list 
      (list 
        (+ x (* runDir tread))
        (+ y rise)
        0.0
      )
      0.0
    )
  )
)
;------------------------------------------------------------
; Geometry engine
;------------------------------------------------------------
(defun stair:build-geometry (risers rise tread runDir nosingType nosingX nosingY / 
                             pts x y v
                            ) 
  ;; Start point of stair profile
  (setq pts (list 
              (list 
                (list 0.0 0.0 0.0)
                0.0
              )
            )
  )
  (setq x 0.0)
  (setq y 0.0)
  ;; Main stair profile
  (repeat (1- risers) 
    (setq pts (append 
                pts
                (cond 
                  ((= nosingType "SQUARE")
                   (stair:step-square x y tread rise nosingX nosingY runDir)
                  )
                  ((= nosingType "ROUND")
                   (stair:step-round x y tread rise nosingY runDir)
                  )
                  (T
                   (stair:step-none x y tread rise runDir)
                  )
                )
              )
    )
    (setq x (+ x (* runDir tread)))
    (setq y (+ y rise))
  )
  ;; Final riser closure
  ;; Last step completion
  (setq pts (append 
              pts
              (cond 
                ;; Last square nose
                ((= nosingType "SQUARE")
                 (list 
                   ;; Top of final riser
                   (list 
                     (list 
                       x
                       (+ y (- rise nosingY))
                       0.0
                     )
                     0.0
                   )
                   ;; Nose projection
                   (list 
                     (list 
                       (+ x (* runDir (- nosingX)))
                       (+ y (- rise nosingY))
                       0.0
                     )
                     0.0
                   )
                   ;; Nose top
                   (list 
                     (list 
                       (+ x (* runDir (- nosingX)))
                       (+ y rise)
                       0.0
                     )
                     0.0
                   )
                   ;; Return to stair axis
                   (list 
                     (list 
                       x
                       (+ y rise)
                       0.0
                     )
                     0.0
                   )
                 )
                )
                ;; Last round nose
                ((= nosingType "ROUND")
                 (list 
                   (list 
                     (list 
                       x
                       (+ y (- rise nosingY))
                       0.0
                     )
                     (if (> runDir 0.0) 
                       -1.0
                       1.0
                     )
                   )
                   (list 
                     (list 
                       x
                       (+ y rise)
                       0.0
                     )
                     0.0
                   )
                 )
                )
                ;; NONE
                (T nil)
              )
            )
  )
  ;; Final riser only for NONE
  (if (= nosingType "NONE") 
    (setq pts (append 
                pts
                (list 
                  (list 
                    (list 
                      x
                      (+ y rise)
                      0.0
                    )
                    0.0
                  )
                )
              )
    )
  )
  pts
)
;------------------------------------------------------------
; Polyline creation
;------------------------------------------------------------
(defun stair:create-polyline (vertices / dxf item pt bulge en) 
  (setq dxf (list 
              '(0 . "LWPOLYLINE")
              '(100 . "AcDbEntity")
              '(100 . "AcDbPolyline")
              (cons 90 (length vertices))
              '(70 . 0)
            )
  )
  (foreach item vertices 
    (setq pt (car item))
    (setq bulge (cadr item))
    (setq dxf (append 
                dxf
                (list 
                  (cons 10 pt)
                  (cons 42 bulge)
                )
              )
    )
  )
  (setq en (entmakex dxf))
  (if en 
    (vlax-ename->vla-object en)
  )
)
;------------------------------------------------------------
; Preview creation
;------------------------------------------------------------
(defun stair:update-preview (basePt risers rise tread runDir / geom) 
  (stair:delete-preview)
  ;; Build geometry in local UCS coordinates
  (setq geom (stair:build-geometry risers rise tread runDir *stair-nosing-type* 
                                   *stair-nosing-x* *stair-nosing-y*
             )
  )
  ;; Translate geometry from local stair coordinates
  ;; to UCS coordinates based on picked base point,
  ;; then convert UCS -> WCS before creating polyline.
  (setq geom (mapcar 
               '(lambda (v / pt) 
                  (setq pt (list 
                             (+ (car basePt) 
                                (car (car v))
                             )
                             (+ (cadr basePt) 
                                (cadr (car v))
                             )
                             0.0
                           )
                  )
                  ;; UCS -> WCS transformation
                  (setq pt (trans pt 1 0))
                  (list 
                    pt
                    (cadr v)
                  )
                )
               geom
             )
  )
  (setq *stair-preview* (stair:create-polyline geom))
  (princ)
)
;------------------------------------------------------------
; STAIR:INFO Internal Debug Helper
;------------------------------------------------------------
(defun stair:info (/) 
  (prompt 
    (strcat 
      "\nINSUNITS = "
      (itoa (getvar "INSUNITS"))
      "\nIdeal riser = "
      (stair:f2 (stair:ideal-riser))
      "\nErgonomic minimum = "
      (stair:f2 (stair:ergonomic-min))
      "\nErgonomic maximum = "
      (stair:f2 (stair:ergonomic-max))
    )
  )
  (princ)
)
;------------------------------------------------------------
; STAIR:CALC (internal function)
;------------------------------------------------------------
(defun stair:calc (/ bp ep height risers rise tread)
  (setq bp (getpoint "\nBase point: "))
  (if bp 
    (progn 
      (setq ep (getpoint bp "\nArrival point: "))
      (if ep 
        (progn 
          (setq height (stair:get-height bp ep))
          (setq *stair-total-run* (abs 
                                    (- (car ep) 
                                       (car bp)
                                    )
                                  )
          )
          (setq risers (stair:propose-risers height))
          (setq rise (/ height risers))
          (setq tread (stair:ergonomic-tread rise))
          (prompt 
            (strcat 
              "\nHeight = "
              (stair:f2 height)
              "\nIdeal riser = "
              (stair:f2 (stair:ideal-riser))
              "\nRisers = "
              (itoa risers)
              "\nRise = "
              (stair:f2 rise)
              "\nTread = "
              (stair:f2 tread)
              "\n2R+T = "
              (stair:f2 
                (+ (* 2.0 rise) 
                   tread
                )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)
;------------------------------------------------------------
; Stair helpers
;------------------------------------------------------------
(defun stair:get-rundir (bp ep) 
  ;; Left if arrival X < base X
  ;; Right otherwise (including same X)
  (if 
    (< (car ep) 
       (car bp)
    )
    -1.0
    1.0
  )
)
(defun stair:tread-count (risers) 
  (max 0 (1- risers))
)
(defun stair:total-run (risers tread) 
  (* (stair:tread-count risers) 
     tread
  )
)
;------------------------------------------------------------
; Preview report
;------------------------------------------------------------
(defun stair:preview-report (height risers rise tread) 
  (prompt 
    (strcat 
      "\nPreview -> "
      (itoa risers)
      " risers of "
      (stair:f2 rise)
      " | "
      (itoa 
        (stair:tread-count risers)
      )
      " treads of "
      (stair:f2 tread)
      " | Height "
      (stair:f2 height)
      " | Run "
      (stair:f2 
        (stair:total-run 
          risers
          tread
        )
      )
      " | 2R+T "
      (stair:f2 
        (+ (* 2.0 rise) 
           tread
        )
      )
      " | Tread "
      (cond 
        ((= *stair-mode* "ERGONOMIC")
         "ERGONOMIC"
        )
        ((= *stair-mode* "FIXEDTREAD")
         (strcat 
           "FIXED ("
           (stair:f2 *stair-fixed-tread*)
           ")"
         )
        )
        ((= *stair-mode* "CONSTRAINED")
         "FIT"
        )
        (T
         "UNKNOWN"
        )
      )
      " | Nosing "
      *stair-nosing-type*
    )
  )
  (princ)
)
;------------------------------------------------------------
; Final report
;------------------------------------------------------------
(defun stair:final-report (/ treadDesc nosingDesc) 
  ;; Tread description
  (setq treadDesc (cond 
                    ((= *stair-mode* "ERGONOMIC")
                     "ERGONOMIC"
                    )
                    ((= *stair-mode* "FIXEDTREAD")
                     (strcat 
                       "FIXED ("
                       (stair:f2 *stair-fixed-tread*)
                       ")"
                     )
                    )
                    ((= *stair-mode* "CONSTRAINED")
                     "CONSTRAINED"
                    )
                    (T
                     *stair-mode*
                    )
                  )
  )
  ;; Nosing description
  (setq nosingDesc (cond 
                     ((= *stair-nosing-type* "NONE")
                      "NONE"
                     )
                     ((= *stair-nosing-type* "SQUARE")
                      (strcat 
                        "SQUARE ("
                        (stair:f2 *stair-nosing-x*)
                        " x "
                        (stair:f2 *stair-nosing-y*)
                        ")"
                      )
                     )
                     ((= *stair-nosing-type* "ROUND")
                      (strcat 
                        "ROUND (Ø"
                        (stair:f2 *stair-nosing-y*)
                        ")"
                      )
                     )
                     (T
                      *stair-nosing-type*
                     )
                   )
  )
  (prompt 
    (strcat 
      "\nStair accepted -> "
      (itoa *stair-risers*)
      " risers of "
      (rtos *stair-rise* 2 3)
      " | "
      (itoa 
        (stair:tread-count *stair-risers*)
      )
      " treads of "
      (rtos *stair-tread* 2 3)
      " | Height "
      (stair:f2 *stair-height*)
      " | Run "
      (stair:f2 
        (stair:total-run 
          *stair-risers*
          *stair-tread*
        )
      )
      " | 2R+T "
      (stair:f2 
        (+ 
          (* 2.0 *stair-rise*)
          *stair-tread*
        )
      )
      " | Tread "
      treadDesc
      " | Nosing "
      nosingDesc
    )
  )
  (princ)
)
;------------------------------------------------------------ 
; MTEXT report 
;------------------------------------------------------------
(defun stair:create-report-mtext (/ run ang txt txtHeight insPt rot obj) 
  (vl-load-com)
  ;; Total run
  (setq run (stair:total-run 
              *stair-risers*
              *stair-tread*
            )
  )
  ;; Stair angle in degrees
  (setq ang (* 
              180.0
              (/ 
                (atan (/ *stair-height* run))
                pi
              )
            )
  )
  ;; Text height
  (setq txtHeight (/ *stair-rise* 3.0))
  ;; MTEXT contents
  (setq txt (strcat 
              "Height = "
              (stair:f2 *stair-height*)
              "\\P"
              "Run = "
              (stair:f2 run)
              "\\P"
              (itoa *stair-risers*)
              " risers of "
              (stair:f2 *stair-rise*)
              "\\P"
              (itoa 
                (stair:tread-count *stair-risers*)
              )
              " treads of "
              (stair:f2 *stair-tread*)
              "\\P"
              "2R+T = "
              (stair:f2 
                (+ 
                  (* 2.0 *stair-rise*)
                  *stair-tread*
                )
              )
              "\\P"
              "Angle = "
              (rtos ang 2 1)
              "\\U+00B0" ;; "°"
            )
  )
  ;; Insertion point in UCS
  (setq insPt (list 
                (+ (car *stair-bp*) 
                   (* *stair-rundir* run)
                )
                (+ (cadr *stair-bp*) 
                   *stair-height*
                   (/ *stair-rise* 2.0)
                   (* 6.0 txtHeight)
                   *stair-rise*
                )
                0.0
              )
  )
  ;; UCS -> WCS
  (setq insPt (trans insPt 1 0))
  ;; UCS rotation
  (setq rot (angle 
              '(0.0 0.0 0.0)
              (getvar "UCSXDIR")
            )
  )
  ;; Create MTEXT
  (setq obj (vla-AddMText 
              (vla-get-ModelSpace 
                (vla-get-ActiveDocument 
                  (vlax-get-acad-object)
                )
              )
              (vlax-3d-point insPt)
              0.0
              txt
            )
  )
  ;; Properties
  (vla-put-Height obj txtHeight)
  (vla-put-Rotation obj rot)
  ;; Bottom Left
  (vla-put-AttachmentPoint obj 7)
  (princ)
)
;------------------------------------------------------------
; STAIR
; 042B PART 4
;------------------------------------------------------------
(defun c:STAIR (/ bp ep runDir height risers rise tread cmd done ncmd nx ny dia dims 
                tcmd tv tdone rcmd
               ) 
  (defun *error* (msg) 
    (if 
      (and msg 
           (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*"))
      )
      (prompt 
        (strcat 
          "\nError: "
          msg
        )
      )
    )
    (princ)
  )
  (setq bp (getpoint 
             "\nBase point: "
           )
  )
  (if bp 
    (progn 
      (setq ep (getpoint 
                 bp
                 "\nArrival point: "
               )
      )
      (if ep 
        (progn 
          ;; Determine direction
          (setq runDir (stair:get-rundir 
                         bp
                         ep
                       )
          )
          ;; Stair height
          (setq height (stair:get-height 
                         bp
                         ep
                       )
          )
          (setq *stair-total-run* (abs 
                                    (- (car ep) 
                                       (car bp)
                                    )
                                  )
          )
          ;; Proposed risers
          (setq risers (stair:propose-risers 
                         height
                       )
          )
          ;; Resulting rise
          (setq rise (/ 
                       height
                       risers
                     )
          )
          ;; Ergonomic tread
          (setq tread (stair:ergonomic-tread 
                        rise
                      )
          )
          ;; Save current stair state
          (setq *stair-bp* bp)
          (setq *stair-ep* ep)
          (setq *stair-height* height)
          ;          (setq *stair-total-run* *stair-total-run*)
          (setq *stair-risers* risers)
          (setq *stair-rise* rise)
          (setq *stair-tread* tread)
          (setq *stair-rundir* runDir)
          ;; Preview
          (stair:update-preview bp risers rise tread runDir)
          ;; Preview report
          (stair:preview-report 
            height
            risers
            rise
            tread
          )
          ;; Correction loop
          (setq done nil)
          (while (not done) 
            (initget "+ -  Tread Nosing Accept Exit")
            (setq cmd (getkword "\n[+/-/Tread/Nosing/Accept/Exit] <Accept>: "))
            (if (null cmd) (setq cmd "Accept"))
            (cond 
              ;; Tread submenu
              ((= cmd "Tread")
               (setq tdone nil)
               (while (not tdone) 
                 (initget "Value Ergonomic Fit Accept")
                 (setq tcmd (getkword 
                              "\nTread [Value/Ergonomic/Fit/Accept] <Accept>: "
                            )
                 )
                 (if (null tcmd) 
                   (setq tcmd "Accept")
                 )
                 (cond 
                   ;; Value
                   ((= tcmd "Value")
                    (setq tv (getreal 
                               (strcat 
                                 "\nTread value <"
                                 (stair:f2 *stair-fixed-tread*)
                                 ">: "
                               )
                             )
                    )
                    (if (null tv) 
                      (setq tv *stair-fixed-tread*)
                    )
                    (if (> tv 0.0) 
                      (progn 
                        (setq *stair-fixed-tread* tv)
                        (setq *stair-mode* "FIXEDTREAD")
                        (stair:recompute)
                        (stair:refresh-preview)
                      )
                    )
                   )
                   ;; Ergonomic
                   ((= tcmd "Ergonomic")
                    (setq *stair-mode* "ERGONOMIC")
                    (stair:recompute)
                    (stair:refresh-preview)
                   )
                   ;; Fit
                   ((= tcmd "Fit")
                    (setq *stair-mode* "CONSTRAINED")
                    (stair:recompute)
                    (stair:refresh-preview)
                   )
                   ;; Accept
                   ((= tcmd "Accept")
                    (setq tdone T)
                   )
                 )
               )
              )
              ;; Nosing
              ((= cmd "Nosing")
               (initget "None Square Round Cancel")
               (setq ncmd (getkword 
                            (strcat 
                              "\nNosing [None/Square/Round/Cancel] <Cancel>: "
                            )
                          )
               )
               (cond 
                 ;; Cancel
                 ((or 
                    (null ncmd)
                    (= ncmd "Cancel")
                  )
                  (princ)
                 )
                 ;; NONE
                 ((= ncmd "None")
                  (setq *stair-nosing-type* "NONE")
                  (stair:refresh-preview)
                 )
                 ;; SQUARE
                 ((= ncmd "Square")
                  (setq nx (getreal 
                             (strcat 
                               "\nNosing X <"
                               (stair:f2 *stair-nosing-x*)
                               ">: "
                             )
                           )
                  )
                  (if (null nx) 
                    (setq nx *stair-nosing-x*)
                  )
                  (setq ny (getreal 
                             (strcat 
                               "\nNosing Y <"
                               (stair:f2 *stair-nosing-y*)
                               ">: "
                             )
                           )
                  )
                  (if (null ny) 
                    (setq ny *stair-nosing-y*)
                  )
                  (setq dims (stair:clamp-nosing *stair-rise* *stair-tread* "SQUARE" 
                                                 nx ny
                             )
                  )
                  (setq *stair-nosing-type* "SQUARE")
                  (setq *stair-nosing-x* (car dims))
                  (setq *stair-nosing-y* (cadr dims))
                  (stair:refresh-preview)
                 )
                 ;; ROUND
                 ((= ncmd "Round")
                  (setq dia (getreal 
                              (strcat 
                                "\nDiameter <"
                                (stair:f2 *stair-nosing-y*)
                                ">: "
                              )
                            )
                  )
                  (if (null dia) 
                    (setq dia *stair-nosing-y*)
                  )
                  (setq dims (stair:clamp-nosing *stair-rise* *stair-tread* "ROUND" 
                                                 0.0 dia
                             )
                  )
                  (setq *stair-nosing-type* "ROUND")
                  (setq *stair-nosing-y* (cadr dims))
                  (stair:refresh-preview)
                 )
               )
              )
              ;; Accept
              ((= cmd "Accept")
               ;; Promote preview to final geometry
               (setq *stair-preview* nil)
               (stair:final-report)
               (initget "Yes No")
               (setq rcmd (getkword 
                            (strcat 
                              "\nCreate text report? [Yes/No] <"
                              *stair-report-mtext*
                              ">: "
                            )
                          )
               )
               ;; Use previous choice as default
               (if (null rcmd) 
                 (setq rcmd *stair-report-mtext*)
               )
               ;; Remember user preference
               (setq *stair-report-mtext* rcmd)
               ;; Create MTEXT if requested
               (if (= rcmd "Yes") 
                 (stair:create-report-mtext)
               )
               (setq done T)
              )
              ;; Exit
              ((= cmd "Exit")
               (stair:delete-preview)
               (setq done T)
              )
              ;; Add riser
              ((= cmd "+")
               (setq *stair-risers* (1+ *stair-risers*))
               (stair:recompute)
               (stair:refresh-preview)
              )
              ;; Remove riser
              ((= cmd "-")
               (if (> *stair-risers* 2) 
                 (progn 
                   (setq *stair-risers* (1- *stair-risers*))
                   (stair:recompute)
                   (stair:refresh-preview)
                 )
               )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)
;------------------------------------------------------------
; STAIR (quick instructions)
;------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ
  (strcat

    "\n----------------------------------------"
    "\nSTAIR  v1.0.0 - Stair section generator -"
    "\n----------------------------------------"

    "\n"
    "\nQuick workflow:"
    "\n  1. Run STAIR"
    "\n  2. Pick Base Point"
    "\n  3. Pick Arrival Point"
    "\n  4. Adjust stair parameters"
    "\n  5. Accept"

    "\n"
    "\nTread modes:"
    "\n  Ergonomic | Fixed Value | Fit (between picked points)"
    "\n"
    "\n"
    "\nNosing modes:"
    "\n  None | Square | Round"
    "\n"
    "\nType STAIR to start."
  )
)
(princ)