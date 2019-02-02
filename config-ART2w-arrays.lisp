;;************************ config-ART2.lisp ****************************
;;
;;NOTES:
;; 1. LOAD-ART2-FILES FROM ART2.LISP
;; 2. Use (FOUT OUT0)  to output INITIALIZATION VALUES
;;


;;LOAD-ART2-FILES
;;
;;ddd
(defun load-ART2-files ()
;;these should load in .lispworks
 ;; (load "C:\\TOM\\LISP PROJECTS TS\\MyUtilities\\U-Arrays.lisp")
 ;; (load "C:\\TOM\\LISP PROJECTS TS\\MyUtilities\\U-function-plotter.lisp")
;;  (load "C:\\TOM\\LISP PROJECTS TS\\MyUtilities\\U-files.lisp")
;;  (load "C:\\TOM\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp")
;;  (load "C:\\TOM\\LISP PROJECTS TS\\MyUtilities\\U-debug.lisp")
;;  (load "C:\\TOM\\LISP PROJECTS TS\\MyUtilities\\U-tstring.lisp") 
  (load "C:\\TOM\\LISP PROJECTS TS\\MyUtilities\\U-capi.lisp")
;;  (load "C:\\TOM\\LISP PROJECTS TS\\MyUtilities\\U-photo-info.lisp")
  (load "C:\\TOM\\LISP PROJECTS TS\\MyUtilities\\U-sequences.lisp")
;;  (load "C:\\TOM\\LISP PROJECTS TS\\MyUtilities\\U-photos.lisp")
  (load "C:\\TOM\\LISP PROJECTS TS\\ART-LW\\U-art-math-utilities.lisp")
  (load "C:\\TOM\\LISP PROJECTS TS\\ART-LW\\ART2-multipane-interrface.lisp")
  (load "C:\\TOM\\LISP PROJECTS TS\\ART-LW\\ART2-data-analysis.lisp")
  (load "C:\\TOM\\LISP PROJECTS TS\\ART-LW\\ART2-inputs.lisp")
  )

 (load-art2-files)



;;USING FIND-SYMBOL MUST TYPE ALL CAPS FOR SYMBOL NAME
;;(find-symbol "ART2-MULTIPANE-INTERFACE" 'common-lisp-user)=> WORKS
;;LOAD ART2 FILES (unless already loaded)
(unless (or (null *load-art2-files-p)
            (find-symbol "ART2-MULTIPANE-INTERFACE" 'common-lisp-user))
  (load-ART2-files))

;;Sets curser and other Editor variables
(my-config-editor-after-start)
;
;; LATER?? Define a separate name space package for this software module??
;;(make-package :art2)   ;; optional
;;(export '(ART2 ART2-postprocess ART2-init)  :cg-user)  ;; optional


;;------------------------- SOME PARAMETERS (OTHERS LATER) ----------
(defparameter *print-detail 1)
;;done below (defparameter *learnPatterns-cycles 10)
;;done below (setf  *nInputs 5 *nOutputs 3)


;; ART2 FORMULA CALCULATION PARAMETERS ---------------
; Neuron size for plots:
(setq PEsize 18)
(setq PEsizem1 (- PEsize 1))
(setq PEsizep1 (+ PEsize 1))

; Model constants
(defparameter a 0.5 "LTM Weight parameter") ;;default= 0.5
(defparameter b 0.2 "v parameter")  ;;default= 0.2
(defparameter c -1.0 "??" )   ;;default= -1.0
(defparameter d 0.4 "wUp and wdn parameter")  ;;default= 0.4
(defparameter e 0.04 "norm parameter?")  ;;default= 0.04
(defparameter theta 0.3 "f(x) Activation criteria")  ;;default= 0.3
(defparameter vigilance 0.94 "Sigmoid degree of match criteria, max 1.00")  ;;default= 0.94
                       ;;higher number may increase final output
(defparameter alpha 1.0 "LTM weight w parameter")  ;;default= 1.0

(defparameter resetThreshold 0.02 "reset threshold")  ;;default= 0.05
(defparameter *reset-y-criteria 0.20 "y must be greater than this to cause reset") ;;;was 0.25)  
(defparameter  *show-last-reset-vals-n 20 "Number of reset-vals in graph")

(defparameter upLR 0.8 "wUp parameter")  ;;default= 0.12
(defparameter downLR 0.8 "wdn parameter")  ;;default= 0.12
(defparameter *wUpInitLo 0.001 "random init low value")
(defparameter *wUpInitHi  0.15 "random init low value. Must be SMALL.")
(defparameter *wDnInitLo 0.80 "random init low value. Must be LARGE.")
(defparameter *wDnInitHi 0.99 "random init low value")

;;FOR DRAWING GRAPHS ETC
(defparameter *graph-color-list '(:red :blue :orange :green :black :yellow :lightblue :magenta :pink  :gray :violet :cyan1 :darkorchid :tomato :slateblue :salmon3 :mistyrose1 :brown ))


;;THE ART2 CELLS-VARIABLES -------------------------------
(defun set-var-lists ()
  (declare (special *art2-vars *art2-cells *art2-array-names *reset-vals *reset-values-list))
  (setf *art2-vars '(INPUT X-ACTIVITY V R U Q P WUP WDN Y-Output TEMP RESET-VAL  RESET  RESET-CNTR N-CATS TEMP2))
  ;;CELLS ETC
  (setf *art2-cells '((INPUT0 INPUT1 INPUT2 INPUT3 INPUT4) (X-ACTIVITY0 X-ACTIVITY1 X-ACTIVITY2 X-ACTIVITY3 X-ACTIVITY4) (V0 V1 V2 V3 V4) (R0 R1 R2 R3 R4) (U0 U1 U2 U3 U4) (Q0 Q1 Q2 Q3 Q4) (P0 P1 P2 P3 P4) (WUP00 WUP10 WUP20 WUP30 WUP40) (WDN01 WDN11 WDN21 WDN31 WDN41) (Y-Output0 Y-Output1 Y-Output2) (TEMP0 TEMP1 TEMP2 TEMP3 TEMP4) (RESET-VAL0) (RESET0 RESET1 RESET2 RESET3 RESET4) (RESET-CNTR0 RESET-CNTR1 RESET-CNTR2) (N-CATS0 N-CATS1 N-CATS2) (TEMP20 TEMP21 TEMP22)))
  ;;ARRAYS
  (setf *art2-array-names '(*INPUT *X-ACTIVITY *V *R *U *Q *P *WUP *WDN *Y-Output *TEMP *RESET-VAL *RESET *RESET-CNTR *N-CATS *TEMP2))
   
  ;;ADDED TO TRACK RESET VALUES
  (setf *reset-values-list nil
        *reset-vals nil)
  )
;;run the function
(set-var-lists)

;;me
(afout 'out0 (format nil "a= ~a; b= ~a; c= ~a; d= ~a; e= ~a; theta= ~a; vigilance= ~a; alpha= ~a; ~%" a b c d e theta vigilance alpha))



;;XXX ------------------------------- END SOME PARAMETERS -----------------------

;;XXX ----------------- INPUTS TO RUNART ---------------------------------------
;;
;;GO TO ART2-inputs.lisp TO SEE INPUT-PATTERNS IN DETAIL ;;RUNART 
;;
;;SSS START HERE TO SET RUN PARAMETERS
(defun define-current-test-patterns ()
  "In ART2.lisp, also sets *num-cycles. *n-inputs, and *n-outputs--evaled in runart."
  (declare (special *n-inputs *n-outputs *num-cycles  *graph-every-x-cycle *x-activity-y-increment  *x-max))
  (setf *n-inputs  9
        *n-outputs  5
        ;;Is his default num-cycles is replaced by setf pattern-num-cycles (car p-list) in learnpatterns function within art2-manager function??
        *num-cycles 40  ;;was 20
        *graph-every-x-cycle 10
        *x-activity-y-increment  1    ;;was 10 ;;*x-activity-y-increment
        *x-max 400   ;;was 250
        )

  (setf *patternA `(,(find-symbol-digits "D") "D")
         *patternB `(,(find-symbol-digits "A") "A")
         *patternC `(,(find-symbol-digits "X") "X"))
  (setf  *pattern1 (list  *patternA *patternB  *patternC))

#| was (setf *pattern1 
        `((,(find-symbol-digits "A") "A")
          ( ,(find-symbol-digits "D") "D")
          (,(find-symbol-digits "X") "X")
          ;;*pattern1 = (((0 0 0 1 0 0 0 0 0) "A") ((0 0 0 1 0 0 0 1 1) "D"))
          )))|#
  ;;end define-current-test-patterns
  )

(define-current-test-patterns)

;;((3 (1 0 1) "S1")(2(0 1 0) "S2"))
;;  (find-symbol-digits "C")
;; works, returns values= (0 0 0 1 0 0 0 1 0)  (1 0 0 0 1 0)  "C"  "TypeLetter"


(defparameter *current-test-pattern-list *pattern1 "chooses current test pattern")

;;FOR  TEST-PHASE OF ART2 (To check to see what Outputs one (or more) presentations of the different Inputs cause.  Has reliable discrimination learning taken place?
#|(defparameter *num-test-cycles  1)
(defparameter *graph-every-test-cycle 1)
(defparameter *graph-last-test-cycle T)|#
;;(defparamenter *initial-test-x 0)
;;(defparamenter *incr-test-x 0.1)
;;also needed test-pattern-lists graphing-array-sym-list  graphing-symbols-list     )
;;-------------------------------------- end inputs ---------------------------------------------


 
;;MY-ART2-INIT
;; run (testmai) to test it--works, see below
;;
;;ddd
(defun my-ART2-init (nInputs nOutputs 
                             &key not-return-arrays  no-cell-type-arrays 
                             make-lower-cell-arrays initial-element prefix)
  "In ART2.lisp, initializes ART2 and creates strings, data lists, and arrays related to both running ART2 and analyzing and reporting it's data based upon lists instead of arrays (used in calcs)"
  ;;  (setf out nil)

;;FOR INITIALIZING THE ART2 NEURAL NETWORK
;;FROM initialize-cum-arrays
#|WAS (defun initialize-cum-arrays (num-cycles testPatterns)
  "In ART2.lisp, Makes and initializes with 0s the cummulative arrays for adding
  single-cycle array data from each cycle.  The *cum-array-list can be changed to
  include whatever arrays that writer wants to accumulate data from."|#
  (declare 
   (special 
    ;;*total-num-cycles
    *cum-array-list *cum-inputs *cum-x-activity *cum-wdn *cum-wup  *cum-y-output *cum-reset-vals))
   (setf  ;;*total-num-cycles 0 
     *cum-array-list nil *cum-inputs nil *cum-x-activity nil *cum-wdn nil *cum-wup nil  *cum-y-output nil *cum-reset-vals nil)
  ;;the cummulative list
  (setf  *cum-array-list  '((*input *x-activity *wdn *wup *y-output *reset )))
      ;;was  '((*input *cum-inputs)(*x-activity *cum-x-activity)(*wdn *cum-wdn)(*wup *cum-wup)(*y-output *cum-y-output)))

;;FOR THE GRAPHICS DATA DISPLAYS -----------------------------------
  (declare
   (special 
    n-dims
    value
    converted-arrays-list
    new-symbol-list
    *i-points
    *x-points
    *wup-points
    *wdn-points
    *y-points
    *i-cell-list
    *x-list
    *wup-list
    *wdn-list
    *y-cell-list
    *ART2-instance
    ))

  (setf 
   n-dims 0
   value 0
   converted-arrays-list nil
   new-symbol-list nil
   *i-points nil
   *x-points nil
   *wup-points nil
   *wdn-points nil
   *y-points nil
   *i-cell-list nil
   *x-list nil
   *wup-list nil
   *wdn-list nil
   *y-cell-list nil
   *reset-list nil
   *ART2-instance nil
   )

  (let 
      ((symbol-spec-lists
        `((,nInputs ("Input" ((0 1)) "" ""))
          (,nInputs ("X-Activity" ((0 1)) "" ""))
          (,nInputs ("V" ((0 1)) "" ""))
          (,nInputs ("R" ((0 1)) "" ""))
          (,nInputs ("U" ((0 1)) "" ""))
          (,nInputs ("Q" ((0 1)) "" ""))
          (,nInputs ("P" ((0 1)) "" ""))
          (,nInputs ("W" ((0 1)) "" ""))
          (,nInputs ("Wup" ((0 1)(0 0)) "" "")) 
          (,nInputs ("Wdn" ((0 1)(1 0)) "" "")) 
          (,nOutputs ("Y-Output" ((0 1)) "" ""))
          ;;others
          (,nInputs ("Temp" ((0 1)) "" ""))
          (1 ("reset-Val" ((0 1)) "" ""))
          (,nOutputs ("reset" ((0 1)) "" ""))
          (,nOutputs ("reset-cntr" ((0 1)) "" ""))
          (,nOutputs ("n-cats" ((0 1)) "" ""))
          (,nOutputs ("Temp2" ((0 1)) "" ""))))

       ;;other let vars
       (n-symbol-types)
       (dims-list)
       (array-symbol-list-of-lists)
       (all-dims-lists)
       (all-arrays-list)
       (array-sym-types-list)
       ;;end let vars
       )

    (dolist (spec-list symbol-spec-lists)
      (setf dims-list (second (second spec-list))
            all-dims-lists (append all-dims-lists  dims-list))
      )
    (afout 'out0 (format nil "dims-list= ~A~%" dims-list))

    ;;makes the symbols, dimension-lists etc, and (extra) strings of info for viewing?
    (multiple-value-setq (new-symbol-type-list  new-symbols-type-list-of-lists 
                                                new-symbol-type-spec-list-of-lists 
                                                new-symbol-type-symbol-string-list-of-lists)
        (make-new-index-symbol-types symbol-spec-lists))

    (setf n-symbol-types (length new-symbol-type-list))
    ;;was (length new-symbols-type-list-of-lists))

    ;;NOT REDUNDANT -- DONE AT LOWER LEVEL, but lower level creates an array for EVERY CELL not for each TYPE of CELL (lower level array creation may be more useful for more complex networks where num of cells vary more for each subtype of cell. 
    ;;loop to make array for each cell TYPE
    ;;mmm
    (unless no-cell-type-arrays
      (loop
       for symbol-type1 in new-symbol-type-list
       for symbol-type-list1 in new-symbols-type-list-of-lists
       for spec-list1 in new-symbol-type-spec-list-of-lists
       ;;NOTE: the following with statements ASSIGNED VALUES IN PARALLEL
       ;;NOT HOW IT IS SUPPOSED TO WORK?? That is why I added the setf
       ;; phrase below-- then it worked WASTED A LOT OF TIME
       ;;  DOES LW HAVE A PROBLEM?? 
       ;; NOTE USING 'AND INSTEAD OF WITH DID NOT HELP
       with ncells1 ;; =  (length spec-list1)
       with dims-lists1 ;; =  (second (car spec-list1))
       with n-dims1 ;;= (length dims-lists1)
       with array-dims1 ;;= 0
       ;;  with initial-contents = nil

       do
       (setf  ncells1   (length spec-list1)
              dims-lists1   (second (car spec-list1))
              n-dims1  (length dims-lists1))

       ;;(setf xy4 (second (car    '((X-Activity ((0 1))  ) (X-Activity ((1 1))  ) (X-Activity ((2 1))  ) (X-Activity ((3 1))  ) (X-Activity ((4 1))  )))))


       ;;  (afout 'out0 (format nil " (length spec-list1)= ~a (second (car spec-list1))= ~a (length dims-lists1)= ~a~%  "  (length spec-list1) (second (car spec-list1))   (length dims-lists1)  ))
       ;;  (afout 'out0 (format nil "1 PRExxx In new-symbol-type-spec-list-of-lists, spec-list1= ~A~% array-dims1= ~A~%ncells1= ~A  n-dims1= ~a~% dims-lists1= ~a~%" spec-list1 array-dims1 ncells1  n-dims1 dims-lists1 ))

       ;;PREFIX FOR ARRAY NAMES? (eg *)
       (if prefix
           (setf symbol-type1 
                 (my-make-symbol (format nil "~A~A" prefix symbol-type1))))
       ;;MAKE array-dims1 list
       (setf array-dims1 (make-list  n-dims1 :initial-element ncells1))

       ;;VARIABLE-VECTOR ARRAYS MADE HERE with INITIAL-ELEMENT
       (set symbol-type1 (make-array array-dims1
                                     :initial-element initial-element ));; :initial-contents initial-contents))
       ;;mmm
       (setf array-sym-types-list (append array-sym-types-list (list symbol-type1)))

       ;;set the each symbol-type make an array
       (afout 'out0 (format nil "2 xxx In new-symbol-type-spec-list-of-lists, spec-list1= ~A~% array-dims1= ~A~%ncells1= ~A  n-dims1= ~a~% dims-lists1= ~a~%" spec-list1 array-dims1 ncells1  n-dims1 dims-lists1 ))
       ;;end cell-type loop
       ))

    ;;loop to make an array for each cell (or cell subtype)
    (loop
     for new-symbol-type-list in new-symbols-type-list-of-lists
     for n from 0 to n-symbol-types
     for spec-lists in new-symbol-type-spec-list-of-lists
     with spec-list
     with dims-list
     do
     (afout 'out00 (format nil "In ART2init,new-symbol-type-list= ~A~%  spec-lists= ~A~% new-symbol-type-spec-list-of-lists= ~A~%" new-symbol-type-list spec-lists new-symbol-type-spec-list-of-lists))

     ;;makes the arrays to make the ART2 network
     ;;This makes higher level arrays (one for each cell TYPE) vs. the arrays made at the lower level make an array for EACH CELL--may be useful for more complex networks, where the "cell" represents a cell SUBTYPE and each one has an array. Use &key to determine.

     (cond
      (make-lower-cell-arrays       ;;(null no-cell-type-arrays)
                                    (multiple-value-setq (symbol-list array-symbol-list array-list)
                                        (make-arrays new-symbol-type-list all-dims-lists  :initial-element 0 :prefix "*"))
                                    ;;use one of these??    :initial-element  :initial-contents-list)
                                    (setf array-symbol-list-of-lists (append array-symbol-list-of-lists (list array-symbol-list))
                                          all-arrays-list (append all-arrays-list (list array-list))))
      (t 
       ;; (setf all-arrays-list (append all-arrays-list (list array-list)))
       ))
                    
     ;;end loop
     )

    ;;not-return-arrays  no-cell-type-arrays no-lower-cell-arrays)
    (cond
     ((null  not-return-arrays)  ;; no-cell-type-arrays))
      (values new-symbol-type-list  new-symbols-type-list-of-lists 
              array-sym-types-list  new-symbol-type-spec-list-of-lists 
              new-symbol-type-symbol-string-list-of-lists array-symbol-list-of-lists all-arrays-list))
     ((and no-cell-type-arrays make-lower-cell-arrays)
      (values new-symbol-type-list  new-symbols-type-list-of-lists 
              array-sym-types-list  new-symbol-type-spec-list-of-lists 
              new-symbol-type-symbol-string-list-of-lists all-arrays-list))
     ((or not-return-arrays (null make-lower-cell-arrays))
      (values new-symbol-type-list  new-symbols-type-list-of-lists 
              array-sym-types-list  new-symbol-type-spec-list-of-lists 
              new-symbol-type-symbol-string-list-of-lists))
     (t nil))
    ;;end defun my-ART2-init
    ))
                             
#|(progn (setf out nil) (my-ART2-init 5 3 :initial-element 0 :prefix "*"))|#
;;works? returns=>
#| CL-USER 1 > (testmai)
(INPUT X-ACTIVITY V R U Q P WUP WDN Y-Output TEMP RESET-VAL RESET RESET-CNTR N-CATS TEMP2)
((INPUT0 INPUT1 INPUT2 INPUT3 INPUT4) (X-ACTIVITY0 X-ACTIVITY1 X-ACTIVITY2 X-ACTIVITY3 X-ACTIVITY4) (V0 V1 V2 V3 V4) (R0 R1 R2 R3 R4) (U0 U1 U2 U3 U4) (Q0 Q1 Q2 Q3 Q4) (P0 P1 P2 P3 P4) (WUP00 WUP10 WUP20 WUP30 WUP40) (WDN01 WDN11 WDN21 WDN31 WDN41) (Y-Output0 Y-Output1 Y-Output2) (TEMP0 TEMP1 TEMP2 TEMP3 TEMP4) (RESET-VAL0) (RESET0 RESET1 RESET2) (RESET-CNTR0 RESET-CNTR1 RESET-CNTR2) (N-CATS0 N-CATS1 N-CATS2) (TEMP20 TEMP21 TEMP22))
(*INPUT *X-ACTIVITY *V *R *U *Q *P *WUP *WDN *Y-Output *TEMP *RESET-VAL *RESET *RESET-CNTR *N-CATS *TEMP2)
((("Input" ((0 1)) "" "") ("Input" ((1 1)) "" "") ("Input" ((2 1)) "" "") ("Input" ((3 1)) "" "") ("Input" ((4 1)) "" "")) (("X-Activity" ((0 1)) "" "") ("X-Activity" ((1 1)) "" "") ("X-Activity" ((2 1)) "" "") ("X-Activity" ((3 1)) "" "") ("X-Activity" ((4 1)) "" "")) (("V" ((0 1)) "" "") ("V" ((1 1)) "" "") ("V" ((2 1)) "" "") ("V" ((3 1)) "" "") ("V" ((4 1)) "" "")) (("R" ((0 1)) "" "") ("R" ((1 1)) "" "") ("R" ((2 1)) "" "") ("R" ((3 1)) "" "") ("R" ((4 1)) "" "")) (("U" ((0 1)) "" "") ("U" ((1 1)) "" "") ("U" ((2 1)) "" "") ("U" ((3 1)) "" "") ("U" ((4 1)) "" "")) (("Q" ((0 1)) "" "") ("Q" ((1 1)) "" "") ("Q" ((2 1)) "" "") ("Q" ((3 1)) "" "") ("Q" ((4 1)) "" "")) (("P" ((0 1)) "" "") ("P" ((1 1)) "" "") ("P" ((2 1)) "" "") ("P" ((3 1)) "" "") ("P" ((4 1)) "" "")) (("Wup" ((0 1) (0 0)) "" "") ("Wup" ((1 1) (0 0)) "" "") ("Wup" ((2 1) (0 0)) "" "") ("Wup" ((3 1) (0 0)) "" "") ("Wup" ((4 1) (0 0)) "" "")) (("Wdn" ((0 1) (1 0)) "" "") ("Wdn" ((1 1) (1 0)) "" "") ("Wdn" ((2 1) (1 0)) "" "") ("Wdn" ((3 1) (1 0)) "" "") ("Wdn" ((4 1) (1 0)) "" "")) (("Y-Output" ((0 1)) "" "") ("Y-Output" ((1 1)) "" "") ("Y-Output" ((2 1)) "" "")) (("Temp" ((0 1)) "" "") ("Temp" ((1 1)) "" "") ("Temp" ((2 1)) "" "") ("Temp" ((3 1)) "" "") ("Temp" ((4 1)) "" "")) (("reset-Val" ((0 1)) "" "")) (("reset" ((0 1)) "" "") ("reset" ((1 1)) "" "") ("reset" ((2 1)) "" "")) (("reset-cntr" ((0 1)) "" "") ("reset-cntr" ((1 1)) "" "") ("reset-cntr" ((2 1)) "" "")) (("n-cats" ((0 1)) "" "") ("n-cats" ((1 1)) "" "") ("n-cats" ((2 1)) "" "")) (("Temp2" ((0 1)) "" "") ("Temp2" ((1 1)) "" "") ("Temp2" ((2 1)) "" "")))
(("Input0" "Input1" "Input2" "Input3" "Input4") ("X-Activity0" "X-Activity1" "X-Activity2" "X-Activity3" "X-Activity4") ("V0" "V1" "V2" "V3" "V4") ("R0" "R1" "R2" "R3" "R4") ("U0" "U1" "U2" "U3" "U4") ("Q0" "Q1" "Q2" "Q3" "Q4") ("P0" "P1" "P2" "P3" "P4") ("Wup00" "Wup10" "Wup20" "Wup30" "Wup40") ("Wdn01" "Wdn11" "Wdn21" "Wdn31" "Wdn41") ("Y-Output0" "Y-Output1" "Y-Output2") ("Temp0" "Temp1" "Temp2" "Temp3" "Temp4") ("reset-Val0") ("reset0" "reset1" "reset2") ("reset-cntr0" "reset-cntr1" "reset-cntr2") ("n-cats0" "n-cats1" "n-cats2") ("Temp20" "Temp21" "Temp22"))  NIL NIL
CL-USER 2 > input
(INPUT0 INPUT1 INPUT2 INPUT3 INPUT4)
CL-USER 3 > *input
#(0 0 0 0 0)
|#


   
;;OUTPUT FROM MAKE-ARRAYS IN U-ARRAYS.LISP
;; use for writing functions to initialize ART2
#|
((INPUT0 INPUT1 INPUT2 INPUT3 INPUT4) (X-ACTIVITY0 X-ACTIVITY1 X-ACTIVITY2 X-ACTIVITY3 X-ACTIVITY4) (V0 V1 V2 V3 V4) (R0 R1 R2 R3 R4) (U0 U1 U2 U3 U4) (Q0 Q1 Q2 Q3 Q4) (P0 P1 P2 P3 P4) (WUP00 WUP11 WUP22 WUP33 WUP44) (WDN00 WDN11 WDN22 WDN33 WDN44) (Y-Output0 Y-Output1 Y-Output2) (TEMP0 TEMP1 TEMP2 TEMP3 TEMP4) (RESET-VAL0) (RESET0 RESET1 RESET2) (RESET-CNTR0 RESET-CNTR1 RESET-CNTR2) (N-CATS0 N-CATS1 N-CATS2) (TEMP20 TEMP21 TEMP22))
((("Input" ((0 1)) "" "") ("Input" ((1 1)) "" "") ("Input" ((2 1)) "" "") ("Input" ((3 1)) "" "") ("Input" ((4 1)) "" "")) (("X-Activity" ((0 1)) "" "") ("X-Activity" ((1 1)) "" "") ("X-Activity" ((2 1)) "" "") ("X-Activity" ((3 1)) "" "") ("X-Activity" ((4 1)) "" "")) (("V" ((0 1)) "" "") ("V" ((1 1)) "" "") ("V" ((2 1)) "" "") ("V" ((3 1)) "" "") ("V" ((4 1)) "" "")) (("R" ((0 1)) "" "") ("R" ((1 1)) "" "") ("R" ((2 1)) "" "") ("R" ((3 1)) "" "") ("R" ((4 1)) "" "")) (("U" ((0 1)) "" "") ("U" ((1 1)) "" "") ("U" ((2 1)) "" "") ("U" ((3 1)) "" "") ("U" ((4 1)) "" "")) (("Q" ((0 1)) "" "") ("Q" ((1 1)) "" "") ("Q" ((2 1)) "" "") ("Q" ((3 1)) "" "") ("Q" ((4 1)) "" "")) (("P" ((0 1)) "" "") ("P" ((1 1)) "" "") ("P" ((2 1)) "" "") ("P" ((3 1)) "" "") ("P" ((4 1)) "" "")) (("Wup" ((0 1) (0 1)) "" "") ("Wup" ((1 1) (1 1)) "" "") ("Wup" ((2 1) (2 1)) "" "") ("Wup" ((3 1) (3 1)) "" "") ("Wup" ((4 1) (4 1)) "" "")) (("Wdn" ((0 1) (0 1)) "" "") ("Wdn" ((1 1) (1 1)) "" "") ("Wdn" ((2 1) (2 1)) "" "") ("Wdn" ((3 1) (3 1)) "" "") ("Wdn" ((4 1) (4 1)) "" "")) (("Y-Output" ((0 1)) "" "") ("Y-Output" ((1 1)) "" "") ("Y-Output" ((2 1)) "" "")) (("Temp" ((0 1)) "" "") ("Temp" ((1 1)) "" "") ("Temp" ((2 1)) "" "") ("Temp" ((3 1)) "" "") ("Temp" ((4 1)) "" "")) (("reset-Val" ((0 1)) "" "")) (("reset" ((0 1)) "" "") ("reset" ((1 1)) "" "") ("reset" ((2 1)) "" "")) (("reset-cntr" ((0 1)) "" "") ("reset-cntr" ((1 1)) "" "") ("reset-cntr" ((2 1)) "" "")) (("n-cats" ((0 1)) "" "") ("n-cats" ((1 1)) "" "") ("n-cats" ((2 1)) "" "")) (("Temp2" ((0 1)) "" "") ("Temp2" ((1 1)) "" "") ("Temp2" ((2 1)) "" "")))
(("Input0" "Input1" "Input2" "Input3" "Input4") ("X-Activity0" "X-Activity1" "X-Activity2" "X-Activity3" "X-Activity4") ("V0" "V1" "V2" "V3" "V4") ("R0" "R1" "R2" "R3" "R4") ("U0" "U1" "U2" "U3" "U4") ("Q0" "Q1" "Q2" "Q3" "Q4") ("P0" "P1" "P2" "P3" "P4") ("Wup00" "Wup11" "Wup22" "Wup33" "Wup44") ("Wdn00" "Wdn11" "Wdn22" "Wdn33" "Wdn44") ("Y-Output0" "Y-Output1" "Y-Output2") ("Temp0" "Temp1" "Temp2" "Temp3" "Temp4") ("reset-Val0") ("reset0" "reset1" "reset2") ("reset-cntr0" "reset-cntr1" "reset-cntr2") ("n-cats0" "n-cats1" "n-cats2") ("Temp20" "Temp21" "Temp22"))
|#

