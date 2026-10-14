;==============================================================================
; CSRF - Column SuRFace
; Command: CSRF
; Version: 1.0.0
; License: CC BY-NC-SA 4.0
; https://creativecommons.org/licenses/by-nc-sa/4.0/
;
; Description:
; Generates sinusoidal offset profiles for the graphical representation
; of column surfaces.
;
; Quarter mode:
; - Select a center profile.
; - Pick a border point to define radius and offset side.
; - Creates intermediate profiles from 0° to 90°.
;
; Diameter mode:
; - Select two parallel boundary profiles representing a diameter.
; - Diameter is automatically measured between profiles.
; - Creates intermediate profiles from 0° to 180°.
; - Includes the center profile (90°).
;
; Notes:
; - Diameter mode assumes both profiles are parallel offsets
;   of the same source geometry.
; - A single Undo restores the drawing to the state before
;   command execution.
; - Default subdivision count is 6 (15° spacing).
;
; Load with APPLOAD
; Run command: CSRF
;==============================================================================

(defun CSRF-DegToRad (a)
  (* pi (/ a 180.0))
)

(defun CSRF-IsCurve (obj)
  (and
    (vlax-method-applicable-p obj 'GetClosestPointTo)
    (vlax-method-applicable-p obj 'GetDistAtParam)
  )
)

(defun CSRF-GetRadius (obj pt / cpt)
  (setq cpt (vlax-curve-getClosestPointTo obj pt))
  (distance pt cpt)
)

(defun CSRF-GetDiameter (obj1 obj2 / p p2 par)
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

(defun CSRF-GetLength (obj)
  (vlax-curve-getDistAtParam
    obj
    (vlax-curve-getEndParam obj)
  )
)

(defun CSRF-DoOffset
       (ent dist sidePt / before after)

  (setq before (entlast))

  (command "_.OFFSET" dist ent sidePt "")

  (setq after (entlast))

  (if (and after (/= before after))
    T
    nil
  )
)

(defun C:CSRF
(
 /
 *error*
 oldCmdecho
 oldOsmode
 undoStarted
 mode
 divisions
 stepAngle
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
 created
 len1
 len2
 tol
)

  (vl-load-com)

  (setq tol 1e-6)

  (defun *error* (msg)

    (if undoStarted
      (command "_.UNDO" "_END")
    )

    (if oldCmdecho
      (setvar 'CMDECHO oldCmdecho)
    )

    (if oldOsmode
      (setvar 'OSMODE oldOsmode)
    )

    (if
      (and msg
           (not
             (wcmatch
               (strcase msg)
               "*CANCEL*,*QUIT*,*EXIT*"
             )
           )
      )
      (princ (strcat "\nError: " msg))
    )

    (princ)
  )

  (setq oldCmdecho (getvar 'CMDECHO))
  (setq oldOsmode  (getvar 'OSMODE))

  (setvar 'CMDECHO 0)

  (command "_.UNDO" "_BEGIN")
  (setq undoStarted T)

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
  ;; Divisions
  ;;--------------------------------------------------------------------------

  (initget 7)

  (setq divisions
        (getint
          "\nNumber of quarter divisions <6>: "
        )
  )

  (if (null divisions)
    (setq divisions 6)
  )

  (if (<= divisions 0)
    (progn
      (prompt "\nInvalid division count.")
      (*error* nil)
      (exit)
    )
  )

  (setq stepAngle (/ 90.0 divisions))

  (if (<= stepAngle 0.0)
    (progn
      (prompt "\nInvalid angular step.")
      (*error* nil)
      (exit)
    )
  )

  (setq created 0)

  ;;--------------------------------------------------------------------------
  ;; QUARTER
  ;;--------------------------------------------------------------------------

  (cond

    ((= mode "Quarter")

      (setq ent
            (car
              (entsel
                "\nSelect center profile: "
              )
            )
      )

      (if (null ent)
        (progn
          (*error* nil)
          (exit)
        )
      )

      (setq obj (vlax-ename->vla-object ent))

      (if (not (CSRF-IsCurve obj))
        (progn
          (prompt "\nSelected entity is not supported.")
          (*error* nil)
          (exit)
        )
      )

      (setq sidePt
            (getpoint
              "\nSpecify border point: "
            )
      )

      (if (null sidePt)
        (progn
          (*error* nil)
          (exit)
        )
      )

      (setq radius
            (CSRF-GetRadius obj sidePt)
      )

      (if (< radius tol)
        (progn
          (prompt "\nInvalid radius.")
          (*error* nil)
          (exit)
        )
      )

      (setq theta stepAngle)

      (while (< theta 90.0)

        (setq dist
              (* radius
                 (sin (CSRF-DegToRad theta))
              )
        )

        (if (CSRF-DoOffset ent dist sidePt)
          (setq created (1+ created))
        )

        (setq theta (+ theta stepAngle))
      )

      (prompt
        (strcat
          "\nQuarter surface generated."
          "\nRadius = "
          (rtos radius 2 3)
          "\nProfiles created = "
          (itoa created)
        )
      )
    )

    ;;--------------------------------------------------------------------------
    ;; DIAMETER
    ;;--------------------------------------------------------------------------

    ((= mode "Diameter")

      (setq ent1
            (car
              (entsel
                "\nSelect first boundary profile: "
              )
            )
      )

      (if (null ent1)
        (progn
          (*error* nil)
          (exit)
        )
      )

      (setq ent2
            (car
              (entsel
                "\nSelect second boundary profile: "
              )
            )
      )

      (if (null ent2)
        (progn
          (*error* nil)
          (exit)
        )
      )

      (if (= ent1 ent2)
        (progn
          (prompt "\nSelect two different profiles.")
          (*error* nil)
          (exit)
        )
      )

      (setq obj1 (vlax-ename->vla-object ent1))
      (setq obj2 (vlax-ename->vla-object ent2))

      (if (or
            (not (CSRF-IsCurve obj1))
            (not (CSRF-IsCurve obj2))
          )
        (progn
          (prompt "\nOne or more selected entities are not supported.")
          (*error* nil)
          (exit)
        )
      )

      (setq len1 (CSRF-GetLength obj1))
      (setq len2 (CSRF-GetLength obj2))

      (if (> (abs (- len1 len2)) tol)
        (prompt
          "\nWarning: profiles do not have the same length."
        )
      )

      (setq diameter
            (CSRF-GetDiameter obj1 obj2)
      )

      (if (< diameter tol)
        (progn
          (prompt "\nInvalid diameter.")
          (*error* nil)
          (exit)
        )
      )

      (setq radius (/ diameter 2.0))

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

      (setq theta stepAngle)

      (while (< theta 180.0)

        (setq dist
              (* radius
                 (- 1.0
                    (cos (CSRF-DegToRad theta))
                 )
              )
        )

        (if (CSRF-DoOffset ent1 dist sidePt)
          (setq created (1+ created))
        )

        (setq theta (+ theta stepAngle))
      )

      (prompt
        (strcat
          "\nHalf-column surface generated."
          "\nDiameter = "
          (rtos diameter 2 3)
          "\nRadius = "
          (rtos radius 2 3)
          "\nProfiles created = "
          (itoa created)
        )
      )
    )
  )

  (command "_.UNDO" "_END")
  (setq undoStarted nil)

  (setvar 'CMDECHO oldCmdecho)
  (setvar 'OSMODE oldOsmode)

  (princ)
)

(princ "\nCSRF v1.0.0 loaded. Type CSRF to run.")
(princ)