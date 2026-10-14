;==============================================================================
; CSRF - Column SuRFace
; Command: CSRF
; Version: 1.1.2
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
; - Radius is computed from the minimum distance between the profile
;   and the picked point.
; - Intermediate profiles are generated from 0° to 90°.
;
; Diameter mode:
; - Select the first boundary profile.
; - Select the opposite boundary profile near the corresponding location.
; - The picked points are projected onto the selected curves.
; - Diameter is computed from the two projected points.
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

(defun CSRF-IsCurve (ent)
  (not
    (vl-catch-all-error-p
      (vl-catch-all-apply
        'vlax-curve-getEndParam
        (list ent)
      )
    )
  )
)

(defun CSRF-GetRadius (obj pt / cpt)

  (setq cpt
        (vlax-curve-getClosestPointTo obj pt)
  )

  (distance pt cpt)
)

(defun CSRF-DoOffset (ent dist sidePt / before after)

  (setq before (entlast))

  (command "_.OFFSET" dist ent sidePt "")

  (setq after (entlast))

  (and after (/= before after))
)

(defun CSRF-EndUndo ()

  (if CSRF:UndoStarted
    (progn
      (command "_.UNDO" "_END")
      (setq CSRF:UndoStarted nil)
    )
  )
)

(defun C:CSRF
(
 /
 *error*
 oldCmdecho
 oldOsmode
 tol
 mode
 divisions
 stepAngle
 created
 theta
 dist
 radius
 diameter
 ent
 obj
 pickPt
 sel1
 sel2
 ent1
 ent2
 obj1
 obj2
 p1
 p2
 sidePt
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

  (if (not CSRF:UndoStarted)
    (setq CSRF:UndoStarted nil)
  )

  (setq tol 1e-8)

  ;;--------------------------------------------------------------------------
  ;; Error handler
  ;;--------------------------------------------------------------------------

  (defun *error* (msg)

    (if CSRF:UndoStarted
      (vl-catch-all-apply
        '(lambda ()
           (command "_.UNDO" "_END")
         )
      )
    )

    (setq CSRF:UndoStarted nil)

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
      (prompt (strcat "\nError: " msg))
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

  ;;==========================================================================
  ;; QUARTER
  ;;==========================================================================

  (if (= mode "Quarter")

    (progn

      (initget 6)

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

      (setq sel1
            (entsel
              "\nSelect center profile: "
            )
      )

      (if sel1

        (progn

          (setq ent (car sel1))

          (if (CSRF-IsCurve ent)

            (progn

              (setq obj (vlax-ename->vla-object ent))

              (setq pickPt
                    (getpoint
                      "\nSpecify border point: "
                    )
              )

              (if pickPt

                (progn

                  (setq radius
                        (CSRF-GetRadius obj pickPt)
                  )

                  (if (> radius tol)

                    (progn

                      ;; Open undo only now

                      (command "_.UNDO" "_BEGIN")
                      (setq CSRF:UndoStarted T)

                      (setq stepAngle
                            (/ 90.0 divisions)
                      )

                      (setq theta stepAngle)
                      (setq created 0)

                      (while (< theta 90.0)

                        (setq dist
                              (* radius
                                 (sin
                                   (CSRF-DegToRad theta)
                                 )
                              )
                        )

                        (if
                          (CSRF-DoOffset
                            ent
                            dist
                            pickPt
                          )
                          (setq created
                                (1+ created)
                          )
                        )

                        (setq theta
                              (+ theta stepAngle)
                        )
                      )

                      (CSRF-EndUndo)

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

                    (prompt "\nInvalid radius.")
                  )
                )
              )
            )

            (prompt "\nUnsupported entity.")
          )
        )
      )
    )

    ;;========================================================================
    ;; DIAMETER
    ;;========================================================================

    (progn

      (initget 6)

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

      (setq sel1
            (entsel
              "\nSelect first boundary profile: "
            )
      )

      (if sel1

        (progn

          (setq ent1 (car sel1))

          (if (CSRF-IsCurve ent1)

            (progn

              (setq obj1
                    (vlax-ename->vla-object ent1)
              )

              (setq p1
                    (vlax-curve-getClosestPointTo
                      obj1
                      (cadr sel1)
                    )
              )

              (setq sel2
                    (entsel
                      "\nSelect opposite boundary profile near corresponding location: "
                    )
              )

              (if sel2

                (progn

                  (setq ent2 (car sel2))

                  (if (/= ent1 ent2)

                    (progn

                      (if (CSRF-IsCurve ent2)

                        (progn

                          (setq obj2
                                (vlax-ename->vla-object ent2)
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

                          (if (> diameter tol)

                            (progn

                              ;; Open undo only now

                              (command "_.UNDO" "_BEGIN")
                              (setq CSRF:UndoStarted T)

                              (setq sidePt
                                    (mapcar
                                      '+
                                      p2
                                      (mapcar
                                        '(lambda (a b)
                                           (- a b)
                                         )
                                        p2
                                        p1
                                      )
                                    )
                              )

                              (setq stepAngle
                                    (/ 180.0 divisions)
                              )

                              (setq theta stepAngle)
                              (setq created 0)

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

                                (if
                                  (CSRF-DoOffset
                                    ent1
                                    dist
                                    sidePt
                                  )
                                  (setq created
                                        (1+ created)
                                  )
                                )

                                (setq theta
                                      (+ theta stepAngle)
                                )
                              )

                              (CSRF-EndUndo)

                              (prompt
                                (strcat
                                  "\nHalf-column surface generated."
                                  "\nDiameter = "
                                  (rtos diameter 2 3)
                                  "\nRadius = "
                                  (rtos (/ diameter 2.0) 2 3)
                                  "\nProfiles created = "
                                  (itoa created)
                                )
                              )
                            )

                            (prompt "\nInvalid diameter.")
                          )
                        )

                        (prompt "\nUnsupported entity.")
                      )
                    )

                    (prompt "\nSelect two different profiles.")
                  )
                )
              )
            )

            (prompt "\nUnsupported entity.")
          )
        )
      )
    )
  )

  (setvar 'CMDECHO oldCmdecho)
  (setvar 'OSMODE oldOsmode)

  (princ)
)

(princ "\nCSRF v1.1.2 loaded. Type CSRF to run.")
(princ)