;==============================================================================
; CSRF - Column SuRFace
; Command: CSRF
; Version: 1.1.0
; License: CC BY-NC-SA 4.0
; https://creativecommons.org/licenses/by-nc-sa/4.0/
;
; Description:
; Generates sinusoidal offset profiles for the graphical representation
; of column surfaces.
;
; Quarter mode:
; - Select a center profile.
; - Pick a border point.
; - The radius is computed from the minimum distance between the profile
;   and the picked point.
; - Intermediate profiles are generated from 0° to 90°.
;
; Diameter mode:
; - Select the first boundary profile.
; - Select the opposite boundary profile near the corresponding location.
; - Each selection click also provides the local measurement point.
; - The diameter is computed from the projected points on both profiles.
; - Intermediate profiles are generated from 0° to 180°.
; - The 90° center profile is included.
;
; Notes:
; - A single Undo restores the drawing to the state before command execution.
; - Division values are remembered during the current AutoCAD session.
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

(defun CSRF-DoOffset (ent dist sidePt / before after)

  (setq before (entlast))

  (command "_.OFFSET" dist ent sidePt "")

  (setq after (entlast))

  (and after (/= before after))
)

(defun C:CSRF
(
 /
 *error*
 oldCmdecho
 oldOsmode
 undoStarted
 tol
 mode
 divisions
 stepAngle
 created
 theta
 dist
 radius
 diameter
 sidePt
 ent
 obj
 sel1
 sel2
 ent1
 ent2
 obj1
 obj2
 p1
 p2
 pickPt
)

  (vl-load-com)

  ;;--------------------------------------------------------------------------
  ;; Session defaults
  ;;--------------------------------------------------------------------------

  (if (not CSRF:QuarterDivisions)
    (setq CSRF:QuarterDivisions 6)
  )

  (if (not CSRF:DiameterDivisions)
    (setq CSRF:DiameterDivisions 12)
  )

  (setq tol 1e-8)

  ;;--------------------------------------------------------------------------
  ;; Error handler
  ;;--------------------------------------------------------------------------

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
               "*CANCEL*,*EXIT*,*QUIT*"
             )
           )
      )
      (prompt (strcat "\nError: " msg))
    )

    (princ)
  )

  ;;--------------------------------------------------------------------------
  ;; Environment
  ;;--------------------------------------------------------------------------

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
  ;; Quarter mode
  ;;--------------------------------------------------------------------------

  (cond

    ((= mode "Quarter")

      (initget 7)

      (setq divisions
            (getint
              (strcat
                "\nNumber of divisions <"
                (itoa CSRF:QuarterDivisions)
                ">: "
              )
            )
      )

      (if (null divisions)
        (setq divisions CSRF:QuarterDivisions)
      )

      (setq CSRF:QuarterDivisions divisions)

      (setq stepAngle (/ 90.0 divisions))

      (setq sel1
            (entsel
              "\nSelect center profile: "
            )
      )

      (if (null sel1)
        (progn
          (*error* nil)
          (exit)
        )
      )

      (setq ent (car sel1))
      (setq obj (vlax-ename->vla-object ent))

      (if (not (CSRF-IsCurve obj))
        (progn
          (prompt "\nUnsupported entity.")
          (*error* nil)
          (exit)
        )
      )

      (setq pickPt
            (getpoint
              "\nSpecify border point: "
            )
      )

      (if (null pickPt)
        (progn
          (*error* nil)
          (exit)
        )
      )

      (setq radius
            (CSRF-GetRadius obj pickPt)
      )

      (if (< radius tol)
        (progn
          (prompt "\nInvalid radius.")
          (*error* nil)
          (exit)
        )
      )

      (setq created 0)
      (setq theta stepAngle)

      (while (< theta 90.0)

        (setq dist
              (* radius
                 (sin
                   (CSRF-DegToRad theta)
                 )
              )
        )

        (if (CSRF-DoOffset ent dist pickPt)
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

    ;;----------------------------------------------------------------------
    ;; Diameter mode
    ;;----------------------------------------------------------------------

    ((= mode "Diameter")

      (initget 7)

      (setq divisions
            (getint
              (strcat
                "\nNumber of divisions <"
                (itoa CSRF:DiameterDivisions)
                ">: "
              )
            )
      )

      (if (null divisions)
        (setq divisions CSRF:DiameterDivisions)
      )

      (setq CSRF:DiameterDivisions divisions)

      (setq stepAngle (/ 180.0 divisions))

      ;; First profile + measurement point

      (setq sel1
            (entsel
              "\nSelect first boundary profile: "
            )
      )

      (if (null sel1)
        (progn
          (*error* nil)
          (exit)
        )
      )

      (setq ent1 (car sel1))
      (setq obj1 (vlax-ename->vla-object ent1))

      (if (not (CSRF-IsCurve obj1))
        (progn
          (prompt "\nUnsupported first entity.")
          (*error* nil)
          (exit)
        )
      )

      (setq p1
            (vlax-curve-getClosestPointTo
              obj1
              (cadr sel1)
            )
      )

      ;; Second profile + measurement point

      (setq sel2
            (entsel
              "\nSelect opposite boundary profile near corresponding location: "
            )
      )

      (if (null sel2)
        (progn
          (*error* nil)
          (exit)
        )
      )

      (setq ent2 (car sel2))

      (if (= ent1 ent2)
        (progn
          (prompt "\nSelect two different profiles.")
          (*error* nil)
          (exit)
        )
      )

      (setq obj2 (vlax-ename->vla-object ent2))

      (if (not (CSRF-IsCurve obj2))
        (progn
          (prompt "\nUnsupported second entity.")
          (*error* nil)
          (exit)
        )
      )

      (setq p2
            (vlax-curve-getClosestPointTo
              obj2
              (cadr sel2)
            )
      )

      (setq diameter
            (distance p1 p2)
      )

      (if (< diameter tol)
        (progn
          (prompt "\nInvalid diameter.")
          (*error* nil)
          (exit)
        )
      )

      (setq radius (/ diameter 2.0))

      ;; Use projected second point as offset side

      (setq sidePt p2)

      (setq created 0)
      (setq theta stepAngle)

      (while (< theta 180.0)

        (setq dist
              (* 0.5
                 diameter
                 (- 1.0
                    (cos
                      (CSRF-DegToRad theta)
                    )
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

  ;;--------------------------------------------------------------------------
  ;; Restore environment
  ;;--------------------------------------------------------------------------

  (command "_.UNDO" "_END")
  (setq undoStarted nil)

  (setvar 'CMDECHO oldCmdecho)
  (setvar 'OSMODE oldOsmode)

  (princ)
)

(princ "\nCSRF v1.0.0 loaded. Type CSRF to run.")
(princ)
``