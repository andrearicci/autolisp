;==============================================================================
; Column Surface Offsets
; crea degli offset a passo sinusoidale di un oggetto selezionato per emulare una superficie rigata di 1/4 di colonna 
; Command: CSRF
; Version: 1.0.0
; License: CC BY-NC-SA 4.0
; https://creativecommons.org/licenses/by-nc-sa/4.0/
;
; Load with APPLOAD
; Run command: CSRF
;==============================================================================

(defun C:CSRF
(
  /
  *error*
  oldCmdecho
  oldOsmode
  ent
  sidePt
  radius
  divisions
  stepAngle
  i
  angle
  offsetDist
)

  (vl-load-com)

  ;;--------------------------------------------------------------------------
  ;; Error handler
  ;;--------------------------------------------------------------------------
  (defun *error* (msg)

    (if oldCmdecho
      (setvar 'CMDECHO oldCmdecho)
    )

    (if oldOsmode
      (setvar 'OSMODE oldOsmode)
    )

    (if
      (and
        msg
        (not (wcmatch (strcase msg) "*CANCEL*,*EXIT*,*QUIT*"))
      )
      (princ (strcat "\nError: " msg))
    )

    (princ)
  )

  ;;--------------------------------------------------------------------------
  ;; Save environment
  ;;--------------------------------------------------------------------------
  (setq oldCmdecho (getvar 'CMDECHO))
  (setq oldOsmode  (getvar 'OSMODE))

  (setvar 'CMDECHO 0)

  ;;--------------------------------------------------------------------------
  ;; Select source entity
  ;;--------------------------------------------------------------------------
  (setq ent (car (entsel "\nSelect object to offset: ")))

  (if ent
    (progn

      ;;----------------------------------------------------------------------
      ;; Select offset side
      ;;----------------------------------------------------------------------
      (setq sidePt
        (getpoint "\nSpecify offset side: ")
      )

      (if sidePt
        (progn

          ;;------------------------------------------------------------------
          ;; Radius
          ;;------------------------------------------------------------------
          (setq radius
            (getdist "\nSpecify column radius: ")
          )

          (if radius
            (progn

              ;;--------------------------------------------------------------
              ;; Quarter circle divisions
              ;; Default = 6 → 15° spacing
              ;;--------------------------------------------------------------
              (initget 6)

              (setq divisions
                (getint
                  "\nNumber of quarter-circle divisions <6>: "
                )
              )

              (if (null divisions)
                (setq divisions 6)
              )

              (setq stepAngle (/ 90.0 divisions))
              (setq i 1)

              ;;--------------------------------------------------------------
              ;; Create offsets
              ;;--------------------------------------------------------------
              (repeat (1- divisions)

                (setq angle (* i stepAngle))

                (setq offsetDist
                  (* radius
                     (sin
                       (* pi (/ angle 180.0))
                     )
                  )
                )

                (command
                  "_.OFFSET"
                  offsetDist
                  ent
                  sidePt
                  ""
                )

                (setq i (1+ i))
              )

              (princ
                (strcat
                  "\nCreated "
                  (itoa (1- divisions))
                  " sinusoidal offsets."
                )
              )
            )
          )
        )
      )
    )
  )

  ;;--------------------------------------------------------------------------
  ;; Restore environment
  ;;--------------------------------------------------------------------------
  (setvar 'CMDECHO oldCmdecho)
  (setvar 'OSMODE oldOsmode)

  (princ)
)

(princ "\nCSRF loaded. Type CSRF to run.")
(princ)