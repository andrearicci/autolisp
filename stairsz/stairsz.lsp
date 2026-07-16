(vl-load-com)

;------------------------------------------------------------
; STAIR 042B Part 4.1 - geometry completion fix
;
; Stair section generator
;
; (c) 2026 Andrea Ricci (with AI help)
;
; License:
; https://creativecommons.org/licenses/by-nc-sa/4.0/
;
; Commands:
; STAIR
; STAIRINFO
; STAIRCALC
;------------------------------------------------------------

;------------------------------------------------------------
; Runtime state
;------------------------------------------------------------

(setq *stair-doc* nil)
(setq *stair-preview* nil)

(setq *stair-mode* "ERGONOMIC")
(setq *stair-fixed-tread* 30.0)

(setq *stair-nosing-type* "NONE")

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

(setq *stair-nosing-x*
      (stair:default-square-x))

(setq *stair-nosing-y*
      (stair:default-square-y))

;------------------------------------------------------------
; Ergonomic helpers
;------------------------------------------------------------

(defun stair:get-height
  (bp ep)

  ;; Height is always Delta Y in UCS

  (abs
    (- (cadr ep)
       (cadr bp)
    )
  )
)

(defun stair:propose-risers
  (height)

  (max

    2

    (stair:round-int

      (/ height
         (stair:ideal-riser)
      )
    )
  )
)

(defun stair:ergonomic-tread
  (rise)

  (-

    (stair:ergonomic-min)

    (* 2.0 rise)
  )
)

(defun stair:ergonomic-ok-p
  (rise tread)

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
; Nosing clamp
;------------------------------------------------------------

(defun stair:clamp-nosing
  (rise tread nosingType nx ny / maxX maxY changed)

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

(defun stair:step-none
  (x y tread rise runDir /)

  (list

    (list
      (list x (+ y rise) 0.0)
      0.0
    )

    (list
      (list (+ x (* runDir tread))
            (+ y rise)
            0.0)
      0.0
    )
  )
)

(defun stair:step-square
  (x y tread rise nx ny runDir /)

  (list

    (list
      (list (+ x (* runDir nx))
            y
            0.0)
      0.0
    )

    (list
      (list (+ x (* runDir nx))
            (- (+ y rise) ny)
            0.0)
      0.0
    )

    (list
      (list x
            (- (+ y rise) ny)
            0.0)
      0.0
    )

    (list
      (list x
            (+ y rise)
            0.0)
      0.0
    )

    (list
      (list (+ x (* runDir (+ tread nx)))
            (+ y rise)
            0.0)
      0.0
    )
  )
)

(defun stair:step-round
  (x y tread rise dia runDir / r bulge)

  (setq r (/ dia 2.0))

  (setq bulge
    (if (> runDir 0.0)
      -1.0
       1.0
    )
  )

  (list

    (list
      (list (+ x (* runDir r))
            y
            0.0)
      0.0
    )

    (list
      (list (+ x (* runDir r))
            (- (+ y rise) dia)
            0.0)
      bulge
    )

    (list
      (list (+ x (* runDir r))
            (+ y rise)
            0.0)
      0.0
    )

    (list
      (list (+ x (* runDir (+ tread r)))
            (+ y rise)
            0.0)
      0.0
    )
  )
)

;------------------------------------------------------------
; Geometry engine
;------------------------------------------------------------

(defun stair:build-geometry
  (risers rise tread runDir
          nosingType
          nosingX
          nosingY
          / pts x y)

(setq pts

  (list

    (list

      (list
        0.0
        0.0
        0.0
      )

      0.0
    )
  )
)

(setq x 0.0)
(setq y 0.0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;; debug ;;;;;;;;;;;;
  (prompt
  (strcat
    "\nRISERS="
    (itoa risers)
    " RISE="
    (stair:f2 rise)
    " TREAD="
    (stair:f2 tread)
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (repeat (1- risers)

    (setq pts

      (append
        pts

        (cond

          ((= nosingType "SQUARE")

            (stair:step-square
              x y tread rise
              nosingX nosingY
              runDir
            )
          )

          ((= nosingType "ROUND")

            (stair:step-round
              x y tread rise
              nosingY
              runDir
            )
          )

          (T

            (stair:step-none
              x y tread rise runDir
            )
          )
        )
      )
    )

    (setq x (+ x (* runDir tread)))
    (setq y (+ y rise))
  )
 (foreach v pts

    (prompt

      (strcat

        "\nX="
        (rtos (car (car v)) 2 3)

        " Y="
        (rtos (cadr (car v)) 2 3)
      )
    )
  )

    (setq pts

    (append

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

  pts

)

;------------------------------------------------------------
; Polyline creation
;------------------------------------------------------------

(defun stair:create-polyline
  (vertices / dxf item pt bulge en)

  (setq dxf

    (list

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

    (setq dxf

      (append

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

(defun stair:update-preview
  (basePt
   risers
   rise
   tread
   runDir
   / geom)

  (stair:delete-preview)

  (setq geom

    (stair:build-geometry

      risers
      rise
      tread
      runDir

      *stair-nosing-type*
      *stair-nosing-x*
      *stair-nosing-y*
    )
  )

  (setq geom

    (mapcar

      '(lambda (v)

         (list

           (list
             (+ (car basePt)
                (car (car v)))

             (+ (cadr basePt)
                (cadr (car v)))

             0.0
           )

           (cadr v)
         )
       )

      geom
    )
  )

  (setq *stair-preview*
    (stair:create-polyline geom)
  )

  (princ)
)

;------------------------------------------------------------
; STAIRINFO
;------------------------------------------------------------

(defun c:STAIRINFO (/)

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
; STAIRCALC
;------------------------------------------------------------

(defun c:STAIRCALC
  (/ bp ep height risers rise tread)

  (setq bp
    (getpoint "\nBase point: ")
  )

  (if bp

    (progn

      (setq ep
        (getpoint bp "\nArrival point: ")
      )

      (if ep

        (progn

          (setq height
            (stair:get-height bp ep)
          )

          (setq risers
            (stair:propose-risers height)
          )

          (setq rise
            (/ height risers)
          )

          (setq tread
            (stair:ergonomic-tread rise)
          )

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
                   tread)
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

(defun stair:get-rundir
  (bp ep)

  ;; Left if arrival X < base X
  ;; Right otherwise (including same X)

  (if (< (car ep)
         (car bp))
    -1.0
     1.0
  )
)

(defun stair:tread-count
  (risers)

  (max 0 (1- risers))
)

(defun stair:total-run
  (risers tread)

  (* (stair:tread-count risers)
     tread)
)

;------------------------------------------------------------
; Preview report
;------------------------------------------------------------

(defun stair:preview-report
  (height
   risers
   rise
   tread)

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
           tread)
      )

      " | Mode "
      *stair-mode*

      " | Nosing "
      *stair-nosing-type*
    )
  )

  (princ)
)

;------------------------------------------------------------
; STAIR
; 042B PART 4
;------------------------------------------------------------

(defun c:STAIR
  (/ bp
     ep

     runDir

     height

     risers
     rise
     tread)

  (setq bp
    (getpoint
      "\nBase point: "
    )
  )

  (if bp

    (progn

      (setq ep

        (getpoint

          bp

          "\nArrival point: "
        )
      )

      (if ep

        (progn

          ;; Determine direction

          (setq runDir
            (stair:get-rundir
              bp
              ep
            )
          )

          ;; Stair height

          (setq height

            (stair:get-height
              bp
              ep
            )
          )

          ;; Proposed risers

          (setq risers

            (stair:propose-risers
              height
            )
          )

          ;; Resulting rise

          (setq rise

            (/ height
               risers)
          )

          ;; Ergonomic tread

          (setq tread

            (stair:ergonomic-tread
              rise
            )
          )

          ;; Preview

          (stair:update-preview

            bp

            risers

            rise

            tread

            runDir
          )

          ;; Preview report

          (stair:preview-report

            height

            risers

            rise

            tread
          )
        )
      )
    )
  )

  (princ)
)
;;;;;;;;;;;;;;;;;;;;;; debug

;------------------------------------------------------------
; STAIR (geometry test)
;------------------------------------------------------------
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ "\nSTAIR 042B Part 4.1 - geometry completion fix")
(princ)