; ============================================================
; PANELS.LSP
; CC BY-NC-SA 4.0
; https://creativecommons.org/licenses/by-nc-sa/4.0/
;
; Version : 0.2.1
; Command : PANELS
;
; Fixed Panel + Fixed Gap
; Symmetric edge panels
;
; Block:
; PNL_PANEL
;
; Load:
; APPLOAD
;
; Run:
; PANELS
; ============================================================

(vl-load-com)

; ------------------------------------------------------------
; Repeat value N times
; ------------------------------------------------------------

(defun PNL-RepeatValue (val n / lst)

  (setq lst nil)

  (repeat n
    (setq lst (cons val lst))
  )

  (reverse lst)
)

; ------------------------------------------------------------
; Create PANEL block if missing
; ------------------------------------------------------------

(defun PNL-EnsurePanelBlock
       (/ doc blocks blk arr pl)

  (if (not (tblsearch "BLOCK" "PNL_PANEL"))

    (progn

      (setq doc
            (vla-get-ActiveDocument
              (vlax-get-acad-object)
            )
      )

      (setq blocks
            (vla-get-Blocks doc)
      )

      (setq blk
            (vla-Add
              blocks
              (vlax-3D-point '(0.0 0.0 0.0))
              "PNL_PANEL"
            )
      )

      (setq arr
            (vlax-make-safearray
              vlax-vbDouble
              '(0 . 7)
            )
      )

      (vlax-safearray-fill
        arr
        '(
          -0.5 -0.5
           0.5 -0.5
           0.5  0.5
          -0.5  0.5
         )
      )

      (setq pl
            (vla-AddLightWeightPolyline
              blk
              arr
            )
      )

      (vla-put-Closed pl :vlax-true)

      (vla-put-Layer pl "0")
      (vla-put-Color pl 0)

      (if (tblsearch "LTYPE" "BYBLOCK")
        (vla-put-Linetype pl "BYBLOCK")
      )

      (vla-put-Lineweight
        pl
        acLnWtByBlock
      )

      (prompt "\nPNL_PANEL block created.")
    )
  )

  (princ)
)

; ------------------------------------------------------------
; Insert scaled panel block
; ------------------------------------------------------------

(defun PNL-InsertPanel
       (center width height
        /
        doc
        ms
        br)

  (setq doc
        (vla-get-ActiveDocument
          (vlax-get-acad-object)
        )
  )

  (setq ms
        (vla-get-ModelSpace doc)
  )

  (setq br
        (vla-InsertBlock
          ms
          (vlax-3D-point center)
          "PNL_PANEL"
          width
          height
          1.0
          0.0
        )
  )

  (vla-put-Layer br "0")
  (vla-put-Color br 0)

  br
)
; ------------------------------------------------------------
; Solve FixedPanelGap
;
; Returns:
; (A B B B A)
;
; A = symmetric edge panels
; B = fixed panel width
; D = fixed gap
; ------------------------------------------------------------

(defun PNL-SolveFixedPanelGap
       (totalLen panel gap
        /
        n
        used
        remain
        edge)

  ;; At least one central panel

  (setq n 1)

  ;; Find maximum number of full central panels

  (while
    (<=
      (+ (* (1+ n) panel)
         (* (+ n 4) gap))
      totalLen
    )
    (setq n (1+ n))
  )

  ;; Length occupied by central panels and all gaps

  (setq used
        (+ (* n panel)
           (* (+ n 3) gap)
        )
  )

  ;; Remaining space available for the two edge panels

  (setq remain
        (- totalLen used)
  )

  ;; Symmetric edge panels

  (setq edge
        (/ remain 2.0)
  )

  ;; Safety

  (if (< edge 0.0)
    (setq edge 0.0)
  )

  ;; Return complete panel list

  (append
    (list edge)
    (PNL-RepeatValue panel n)
    (list edge)
  )
)

; ------------------------------------------------------------
; Build grid from X/Y panel lists
; ------------------------------------------------------------

(defun PNL-BuildGrid
       (xmin ymin
        xPanels yPanels
        gapX gapY
        /
        x
        y
        pw
        ph
        cx
        cy)

  (setq x (+ xmin gapX))

  (foreach pw xPanels

    (setq y (+ ymin gapY))

    (foreach ph yPanels

      (setq cx
            (+ x (/ pw 2.0))
      )

      (setq cy
            (+ y (/ ph 2.0))
      )

      (PNL-InsertPanel
        (list cx cy 0.0)
        pw
        ph
      )

      (setq y
            (+ y ph gapY)
      )
    )

    (setq x
          (+ x pw gapX)
    )
  )

  (princ)
)

; ------------------------------------------------------------
; Main command
; ------------------------------------------------------------

(defun C:PANELS
       (/ *error*
          oldCmdecho
          oldOsmode

          p1
          p2

          xmin
          ymin
          xmax
          ymax

          width
          height

          panelX
          panelY

          gapX
          gapY

          xPanels
          yPanels)

  (setq oldCmdecho (getvar "CMDECHO"))
  (setq oldOsmode  (getvar "OSMODE"))

  (defun *error* (msg)

    (setvar "CMDECHO" oldCmdecho)
    (setvar "OSMODE" oldOsmode)

    (if
      (and msg
           (not
             (member msg
               '(
                 "Function cancelled"
                 "quit / exit abort"
                )
             )
           )
      )
      (prompt
        (strcat "\nError: " msg)
      )
    )

    (princ)
  )

  (setvar "CMDECHO" 0)

  (PNL-EnsurePanelBlock)

  ; ----------------------------------------------------------
  ; Rectangle
  ; ----------------------------------------------------------

  (setq p1
        (getpoint
          "\nPick first corner: "
        )
  )

  (if (null p1)
    (exit)
  )

  (setq p2
        (getcorner
          p1
          "\nPick opposite corner: "
        )
  )

  (if (null p2)
    (exit)
  )

  ; ----------------------------------------------------------
  ; Inputs
  ; ----------------------------------------------------------

  (setq panelX
        (getdist
          "\nPanel width X: "
        )
  )

  (if (null panelX)
    (exit)
  )

  (setq gapX
        (getdist
          "\nGap X: "
        )
  )

  (if (null gapX)
    (exit)
  )

  (setq panelY
        (getdist
          "\nPanel height Y: "
        )
  )

  (if (null panelY)
    (exit)
  )

  (setq gapY
        (getdist
          "\nGap Y: "
        )
  )

  (if (null gapY)
    (exit)
  )
