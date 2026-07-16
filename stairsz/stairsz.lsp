(vl-load-com)

;------------------------------------------------------------
; STAIR 042B REBUILD - PART 2
;
; Stair section generator
;
; (c) 2026 Andrea Ricci (with AI help)
;
; License:
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

  (setq *stair-temp*
    (stair:create-polyline geom)
  )

  (princ)
)

;------------------------------------------------------------
; Test command
;------------------------------------------------------------

(defun c:STAIR
  (/ dir runDir mode nx ny clampData)

  (initget "Left Right")

  (setq dir
    (getkword
      "\nDirection [Left/Right] <Right>: "
    )
  )

  (if (null dir)
    (setq dir "Right")
  )

  (setq runDir
    (if (= (strcase dir) "LEFT")
      -1.0
       1.0
    )
  )

  (initget "None Square Round")

  (setq mode
    (getkword
      "\nNosing [None/Square/Round] <Square>: "
    )
  )

  (if (null mode)
    (setq mode "Square")
  )

  (setq mode (strcase mode))

  (setq nx 3.0)
  (setq ny 2.0)

  (cond

    ((= mode "SQUARE")

      (setq nx
        (cond
          ((getreal "\nNose X <3>: "))
          (3.0)
        )
      )

      (setq ny
        (cond
          ((getreal "\nNose Y <2>: "))
          (2.0)
        )
      )
    )

    ((= mode "ROUND")

      (setq ny
        (cond
          ((getreal "\nDiameter <3>: "))
          (3.0)
        )
      )
    )
  )

  (setq clampData
    (stair:clamp-nosing
      17.0
      30.0
      mode
      nx
      ny
    )
  )

  (setq nx (car clampData))
  (setq ny (cadr clampData))

  (setq *stair-nosing-type* mode)
  (setq *stair-nosing-x* nx)
  (setq *stair-nosing-y* ny)

  (stair:update-preview

    '(0.0 0.0 0.0)

    5
    17.0
    30.0

    runDir
  )

  (prompt
    (strcat
      "\nTest stair created. Nosing="
      mode
    )
  )

  (princ)
)

(princ "\nSTAIR 042B REBUILD loaded - Part 2")
(princ)