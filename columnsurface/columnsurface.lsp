;==============================================================================
; CSRF - Column SuRFace
; Version: 1.0.0
; License: CC BY-NC-SA 4.0
; https://creativecommons.org/licenses/by-nc-sa/4.0/
;
; Load with APPLOAD
; Run command: CSRF
;==============================================================================

(defun CSRF-DegToRad (a)
  (* pi (/ a 180.0))
)

(defun CSRF-Offset
       (ent dist sidePt)
  (command "_.OFFSET" dist ent sidePt "")
)

(defun CSRF-GetRadius
       (obj pt / cpt)
  (setq cpt
        (vlax-curve-getClosestPointTo obj pt)
  )

  (distance pt cpt)
)

(defun CSRF-GetDiameter
       (obj1 obj2 / p p2 par)

  (setq par
        (/ (+ (vlax-curve-getStartParam obj1)
              (vlax-curve-getEndParam obj1))
           2.0
        )
  )

  (setq p
        (vlax-curve-getPointAtParam obj1 par)
  )

  (setq p2
        (vlax-curve-getClosestPointTo obj2 p)
  )

  (distance p p2)
)

(defun C:CSRF
(
 /
 *error*
 oldCmdecho
 oldOsmode
 mode
 angleStep
 ent
 ent1
 ent2
 obj
 obj1
 obj2
 sidePt
 radius
 diameter
 theta
 dist
)

  (vl-load-com)

  (defun *error* (msg)

    (if oldCmdecho
      (setvar 'CMDECHO oldCmdecho)
    )

    (if oldOsmode
      (setvar 'OSMODE oldOsmode)
    )

    (if
      (and msg
           (not (wcmatch (strcase msg)
                         "*CANCEL*,*QUIT*,*EXIT*"
                )
           )
      )
      (princ
        (strcat "\nError: " msg)
      )
    )

    (princ)
  )

  (setq oldCmdecho (getvar 'CMDECHO))
  (setq oldOsmode  (getvar 'OSMODE))

  (setvar 'CMDECHO 0)

  ;;--------------------------------------------------------------------------
  ;; Mode
  ;;--------------------------------------------------------------------------

  (initget "Quarter Diameter")

  (setq mode
        (getkword
          "\nMode [Quarter/Diameter] <Quarter>: "
        )
  )

  (if (null mode)
    (setq mode "Quarter")
  )

  ;;--------------------------------------------------------------------------
  ;; Angular increment
  ;;--------------------------------------------------------------------------

  (setq angleStep
        (getreal
          "\nDivision angle <15>: "
        )
  )

  (if (null angleStep)
    (setq angleStep 15.0)
  )

  (cond

    ;;======================================================================
    ;; QUARTER
    ;;======================================================================

    ((= mode "Quarter")

      (setq ent
            (car
              (entsel
                "\nSelect center profile: "
              )
            )
      )

      (if ent
        (progn

          (setq sidePt
                (getpoint
                  "\nSpecify border point: "
                )
          )

          (if sidePt
            (progn

              (setq obj
                    (vlax-ename->vla-object ent)
              )

              (setq radius
                    (CSRF-GetRadius obj sidePt)
              )

              (setq theta angleStep)

              (while (< theta 90.0)

                (setq dist
                      (* radius
                         (sin
                           (CSRF-DegToRad theta)
                         )
                      )
                )

                (CSRF-Offset
                  ent
                  dist
                  sidePt
                )

                (setq theta
                      (+ theta angleStep)
                )
              )

              (princ
                (strcat
                  "\nQuarter surface generated."
                  "\nRadius = "
                  (rtos radius 2 3)
                )
              )
            )
          )
        )
      )
    )

    ;;======================================================================
    ;; DIAMETER
    ;;======================================================================

    ((= mode "Diameter")

      (setq ent1
            (car
              (entsel
                "\nSelect first border profile: "
              )
            )
      )

      (if ent1

        (progn

          (setq ent2
                (car
                  (entsel
                    "\nSelect second border profile: "
                  )
                )
          )

          (if ent2

            (progn

              (setq obj1
                    (vlax-ename->vla-object ent1)
              )

              (setq obj2
                    (vlax-ename->vla-object ent2)
              )

              (setq diameter
                    (CSRF-GetDiameter obj1 obj2)
              )

              (setq radius (/ diameter 2.0))

              ;;----------------------------------------------------------
              ;; use midpoint between profiles as reference side
              ;;----------------------------------------------------------

              (setq sidePt
                    (vlax-curve-getClosestPointTo
                      obj2
                      (vlax-curve-getPointAtParam
                        obj1
                        (/ (+ (vlax-curve-getStartParam obj1)
                              (vlax-curve-getEndParam obj1))
                           2.0
                        )
                      )
                    )
              )

              (setq theta angleStep)

              (while (< theta 180.0)

                (setq dist
                      (* radius
                         (- 1.0
                            (cos
                              (CSRF-DegToRad theta)
                            )
                         )
                      )
                )

                (command
                  "_.OFFSET"
                  dist
                  ent1
                  sidePt
                  ""
                )

                (setq theta
                      (+ theta angleStep)
                )
              )

              (princ
                (strcat
                  "\nHalf-column surface generated."
                  "\nDiameter = "
                  (rtos diameter 2 3)
                  "\nRadius = "
                  (rtos radius 2 3)
                )
              )
            )
          )
        )
      )
    )
  )

  (setvar 'CMDECHO oldCmdecho)
  (setvar 'OSMODE oldOsmode)

  (princ)
)

(princ "\nCSRF v1.0.0 loaded. Type CSRF to run.")
(princ)