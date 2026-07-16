(vl-load-com)

;------------------------------------------------------------
; STAIR 042B REBUILD
;
; Stair section generator
;
; (c) 2026 Andrea Ricci (with AI help)
;
; CC BY-NC-SA 4.0
; https://creativecommons.org/licenses/by-nc-sa/4.0/
;
; Command:
; STAIR
;------------------------------------------------------------

;------------------------------------------------------------
; Runtime state
;------------------------------------------------------------

(setq *stair-doc* nil)
(setq *stair-temp* nil)

(setq *stair-mode* "ERGONOMIC")
(setq *stair-fixed-tread* 30.0)

(setq *stair-nosing-type* "NONE")
(setq *stair-nosing-x* 2.0)
(setq *stair-nosing-y* 2.0)

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
; Preview helpers
;------------------------------------------------------------

(defun stair:delete-preview ( / )

  (if
    (and
      *stair-temp*
      (= (type *stair-temp*) 'VLA-OBJECT)
    )

    (vl-catch-all-apply
      'vla-delete
      (list *stair-temp*)
    )
  )

  (setq *stair-temp* nil)

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

  (setq pts '())

  (setq x 0.0)
  (setq y 0.0)

  (repeat (1- risers)

    (setq pts

      (append

        pts

        (cond

          ((= nosingType "SQUARE")

            (stair:step-square
              x
              y
              tread
              rise
              nosingX
              nosingY
              runDir
            )
          )

          ((= nosingType "ROUND")

            (stair:step-round
              x
              y
              tread
              rise
              nosingY
              runDir
            )
          )

          (T

            (stair:step-none
              x
              y
              tread
              rise
              runDir
            )
          )
        )
      )
    )

    (setq x (+ x (* runDir tread)))
    (setq y (+ y rise))
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

  (setq en
    (entmakex dxf)
  )

  (if en
    (vlax-ename->vla-object en)
  )
)

;------------------------------------------------------------
; Preview draw
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

  (setq *stair-temp*
    (stair:create-polyline geom)
  )

  (princ)
)

(princ
 "\nSTAIR 042B REBUILD loaded - Part 1"
)

(princ)