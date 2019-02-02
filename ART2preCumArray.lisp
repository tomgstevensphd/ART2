;;****************************** ART2.lisp **************************
;;
;;
 ;        Adaptive Resonance 2 Model in Common Lisp.
 ;        Copyright 1990 by Mark Watson.
 ;
 ;        Based on the Adaptive Resonance Theory
 ;        by Gail Carpenter and Stephen Grossberg.
;;
;;syntax error in (DOLIST VALUE-LIST (LIST *INPUT* *F1* *F2*



;;TO RUN THIS ART2 PROJECT (From M Watson, Common LISP Modules CH-7 ART2: pp 77ff;
;; TO RUN INSTRUCTIONS p94: (In Listener Window at prompt, type:
;; 1-(ART2-init 5 3); then 2- (ART2) or (art2 :plot? t) then 3- (ART2-postprocess)
;;TO TEST ORIGINAL WATSON
#|(defun testart1 ()
  (setf out nil)
  (ART2-init 5 3)
  (ART2)
  (fout out)
  )|#



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
  (load "C:\\TOM\\LISP PROJECTS TS\\ART-LW\\ART2-multipane-interrface.lisp")
  (load "C:\\TOM\\LISP PROJECTS TS\\ART-LW\\ART2-data-analysis.lisp")
  ;;  (load "C:\\TOM\\LISP PROJECTS TS\\ART-LW\\ 
  )
;;USING FIND-SYMBOL MUST TYPE ALL CAPS FOR SYMBOL NAME
;;(find-symbol "ART2-MULTIPANE-INTERFACE" 'common-lisp-user)=> WORKS
;;LOAD ART2 FILES (unless already loaded)
(unless (find-symbol "ART2-MULTIPANE-INTERFACE" 'common-lisp-user)
  (load-ART2-files))

(my-config-editor-after-start)
;
 ; Define a separate name space package for this software module:
 ;;

;;(make-package :art2)   ;; optional
;;(export '(ART2 ART2-postprocess ART2-init)  :cg-user)  ;; optional


;;------------------------- SOME PARAMETERS (OTHERS LATER) ----------
;;(defparameter *print-detail 1)
;;done below (defparameter *learnPatterns-cycles 10)
;;done below (setf  *nInputs 5 *nOutputs 3)

; DEFINE SOME TEST PATTERNS:

;;original
(setq testPatterns15
      '(
        (0.0 0.4 0.9 0.4 0.0)
        (1.0 0.3 0.0 0.3 1.0)
        (0.75 0.5 0.02 0.05 0.07)
        (0.02 0.41 0.91 0.36 0.08)
        (1.1 0.12 0.0 0.3 1.0)
        (0.0 0.2 0.9 0.4 0.0)
        (0.93 0.31 0.0 0.78 1.0)
        (0.6 0.6 0.02 0.05 0.07)
        (0.79 0.54 0.02 0.08 0.07)
        (0.0 0.45 0.7 0.25 0.0)
        (1.0 0.3 0.0 0.3 0.65)
        (0.0 0.4 0.9 0.4 0.0)
        (0.0 0.5 0.75 0.5 0.0)
        (1.0 0.2 0.0 0.4 0.8)
        (0.82 0.46 0.02 0.09 0.01)
        ))

(setq testPatterns2
      '(
        (0.9 0.9 0.9 0.1 0.1)
        (0.7 0.9 0.8 0.1 0.2)
        ))
(setq testPatterns6
      '(
        (0.0 0.4 0.9 0.4 0.0)
        (1.0 0.3 0.0 0.3 1.0)
        (0.75 0.5 0.02 0.05 0.07)
        (0.02 0.41 0.91 0.36 0.08)
        (1.1 0.12 0.0 0.3 1.0)
        (0.0 0.2 0.9 0.4 0.0)
        ))
      
;;(setq *testPatterns testPatterns6)

;;XXX ------------------------------- END SOME PARAMETERS -----------------------


;;ART2-MANAGER
;;mmm
;;ddd  
(defun ART2-manager (num-cycles ninputs noutputs test-pattern-lists
                                initial-x incr-x)
  "In ART2, manages full ART2 execution, initializes, calculates, simple data analysis, and graphing"
  (let
      ((graphing-array-list)
       (new-symbol-type-list)
       (new-symbols-type-list-of-lists)
       (array-sym-types-list)
       (new-symbol-type-spec-list-of-lists)
       (new-symbol-type-symbol-string-list-of-lists)
       (array-symbol-list-of-lists all-arrays-list)
       )

    ;;STEP 1: INITIALIZE THE ART2 VARIABLES
    (multiple-value-setq ( new-symbol-type-list  new-symbols-type-list-of-lists 
                                       array-sym-types-list  
                                       new-symbol-type-spec-list-of-lists 
                                             new-symbol-type-symbol-string-list-of-lists 
                                             array-symbol-list-of-lists all-arrays-list)
        (my-ART2-init nInputs nOutputs :not-return-arrays nil 
                      :no-cell-type-arrays  nil 
                      :make-lower-cell-arrays  nil
                      :initial-element  0  :prefix "*"))

    (afout 'out (format nil "array-sym-types-list= ~A~%" array-sym-types-list))

    ;;STEP 2: INITIALIZE THE ART2 CONSTANTS
    ;;done in calculation function area

    ;;STEP 3: INITIALIZE THE NETWORK (ART2 VARIABLES-in arrays)
    ;;is this done elsewhere??
    ( initNetwork nInputs nOutputs)

    ;;STEP 4: RUN ART2 -- CALCULATE THE VALUES FOR EACH CYCLE 
    ;; ON EACH FIELD AND CELL
    (setf *learned-patterns
          (learnPatterns nInputs nOutputs test-pattern-lists num-Cycles))   ;; (&optional (numCycles 10))


    ;;OLD VERSION USING MY GRAPHING -- WORKED
    ;;RUN ART2
   ;; (ART2) replaced by initNetwork plus setf *learned-patterns ... above

    ;;now make the array list of lists
    (setf graphing-array-sym-list '(*input  *x-activity  *wUp *wdn *y-output)
     graphing-array-list `(,*input  ,*x-activity  ,*wUp ,*wdn ,*y-output))
    ;;find the graphing-symbols-list
    (setf  graphing-symbols-list `(,input ,x-activity  ,wUp ,wdn ,y-output))
          ;; (find-graphing-symbols new-symbol-type-list new-symbols-type-list-of-lists))
     (afout 'out (format nil "In ART2-manager, graphing-array-list= ~A~%graphing-symbols-list= ~% "  graphing-array-list graphing-symbols-list ))

   ;;STEP ?: CONVERT ART2 ARRAY VALUES TO X, Y COORDS IN LISTS
   ;; SSS  NEED TO PUT THIS INSIDE EACH CYCLE ?
  (convert-ART2-array-lists graphing-array-sym-list  graphing-symbols-list
                                                             initial-x incr-x)

  ;;STEP ?:  GRAPH THE TERMINAL CELL VALUES 
  (graph-ART2)

;;convert-ART2-state (array-list symbol-list  initial-x incr-x)
   ;;end ART2-manager
    ))

;;TESTING ART2
;;ttt
(defun testart ()
  (setf out nil)
  (declare (special *print-detail  *num-cycles *test-patterns ))
  (setf  *print-detail 1
           *num-cycles 5
           *ninputs 5
           *noutputs 3
           *test-patterns testPatterns6
           *initial-x 40 *incr-x 40
        ; Global variable to remember input patterns and
        ; their associated output category code for plotting
        ; by function ART2-Postprocess:
           *learned-categories* nil)

  ;;run ART2
  (ART2-manager *num-cycles *ninputs *noutputs *test-patterns
                                *initial-x *incr-x)
  (fout out)
  )

#|(defun test& ()
  (declare (special xnn))
  (setf xnn 44)
)
(test&)|#


    

;;MY-ART2-INIT
;; run (testmai) to test it--works, see below
;;
;;ddd
(defun my-ART2-init (nInputs nOutputs 
                             &key not-return-arrays  no-cell-type-arrays 
                             make-lower-cell-arrays initial-element prefix)
  "In ART2.lisp, initializes ART2 and creates strings, data lists, and arrays related to both running ART2 and analyzing and reporting it's data based upon lists instead of arrays (used in calcs)"
;;  (setf out nil)

(declare
 (special
  N-DIMS
  VALUE
  CONVERTED-ARRAYS-LIST
  NEW-SYMBOL-LIST
  *I-POINTS
  *X-POINTS
  *WUP-POINTS
  *WDN-POINTS
  *Y-POINTS
  *I-CELL-LIST
  *X-LIST
  *WUP-LIST
  *WDN-LIST
  *Y-CELL-LIST
  *ART2-INSTANCE
  ))
(setf 
  N-DIMS 0
  VALUE 0
  CONVERTED-ARRAYS-LIST nil
  NEW-SYMBOL-LIST nil
  *I-POINTS nil
  *X-POINTS nil
  *WUP-POINTS nil
  *WDN-POINTS nil
  *Y-POINTS nil
  *I-CELL-LIST nil
  *X-LIST nil
  *WUP-LIST nil
  *WDN-LIST nil
  *Y-CELL-LIST nil
  *ART2-INSTANCE nil
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
  (afout 'out (format nil "dims-list= ~A~%" dims-list))

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


    ;;  (afout 'out (format nil " (length spec-list1)= ~a (second (car spec-list1))= ~a (length dims-lists1)= ~a~%  "  (length spec-list1) (second (car spec-list1))   (length dims-lists1)  ))
    ;;  (afout 'out (format nil "1 PRExxx In new-symbol-type-spec-list-of-lists, spec-list1= ~A~% array-dims1= ~A~%ncells1= ~A  n-dims1= ~a~% dims-lists1= ~a~%" spec-list1 array-dims1 ncells1  n-dims1 dims-lists1 ))

      (if prefix
           (setf symbol-type1 
                 (my-make-symbol (format nil "~A~A" prefix symbol-type1))))

      (setf array-dims1 (make-list n-dims1 :initial-element ncells1))

      (set symbol-type1 (make-array array-dims1
                          :initial-element initial-element ));; :initial-contents initial-contents))
;;mmm
      (setf array-sym-types-list (append array-sym-types-list (list symbol-type1)))

      ;;set the each symbol-type make an array
      (afout 'out (format nil "2 xxx In new-symbol-type-spec-list-of-lists, spec-list1= ~A~% array-dims1= ~A~%ncells1= ~A  n-dims1= ~a~% dims-lists1= ~a~%" spec-list1 array-dims1 ncells1  n-dims1 dims-lists1 ))
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
     (afout 'out (format nil "In ART2init,new-symbol-type-list= ~A~%  spec-lists= ~A~% new-symbol-type-spec-list-of-lists= ~A~%" new-symbol-type-list spec-lists new-symbol-type-spec-list-of-lists))

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
    ))
                             
(defun testmai ()
  (setf out nil)
  (my-ART2-init 5 3 :initial-element 0 :prefix "*") 
)
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


;;xxx --------------------------------- ART2 CALCULATION FUNCTIONS --------------

; Neuron size for plots:

(setq PEsize 18)
(setq PEsizem1 (- PEsize 1))
(setq PEsizep1 (+ PEsize 1))

; Model constants

(defparameter a 0.5 "LTM Weight parameter")
(defparameter b 0.2 "v parameter")
(defparameter c -1.0 "??" )
(defparameter d 0.4 "wUp and wdn parameter")
(defparameter e 0.04 "norm parameter?")
(defparameter theta 0.3 "f(x) Activation criteria")
(defparameter vigilance 0.94 "Sigmoid degree of match criteria, max 1.00")
(defparameter alpha 1.0 "LTM weight w parameter")

(defparameter resetThreshold 0.05 "reset threshold")

(defparameter upLR 0.12 "wUp parameter")
(defparameter downLR 0.12 "wdn parameter")

;;THE VARIABLES
(defun set-var-lists ()
  (declare (special *art2-vars *art2-cells *art2-array-names))
  (setf *art2-vars '(INPUT X-ACTIVITY V R U Q P WUP WDN Y-Output TEMP RESET-VAL RESET RESET-CNTR N-CATS TEMP2))
  ;;CELLS ETC
  (setf *art2-cells '((INPUT0 INPUT1 INPUT2 INPUT3 INPUT4) (X-ACTIVITY0 X-ACTIVITY1 X-ACTIVITY2 X-ACTIVITY3 X-ACTIVITY4) (V0 V1 V2 V3 V4) (R0 R1 R2 R3 R4) (U0 U1 U2 U3 U4) (Q0 Q1 Q2 Q3 Q4) (P0 P1 P2 P3 P4) (WUP00 WUP10 WUP20 WUP30 WUP40) (WDN01 WDN11 WDN21 WDN31 WDN41) (Y-Output0 Y-Output1 Y-Output2) (TEMP0 TEMP1 TEMP2 TEMP3 TEMP4) (RESET-VAL0) (RESET0 RESET1 RESET2) (RESET-CNTR0 RESET-CNTR1 RESET-CNTR2) (N-CATS0 N-CATS1 N-CATS2) (TEMP20 TEMP21 TEMP22)))
  ;;ARRAYS
  (setf *art2-array-names '(*INPUT *X-ACTIVITY *V *R *U *Q *P *WUP *WDN *Y-Output *TEMP *RESET-VAL *RESET *RESET-CNTR *N-CATS *TEMP2))
  )
(set-var-lists)

;;me
(afout 'out (format nil "a= ~a; b= ~a; c= ~a; d= ~a; e= ~a; theta= ~a; vigilance= ~a; alpha= ~a; ~%" a b c d e theta vigilance alpha))

; Floating point  random numbers:

(defun frandom (low high)
  (let ((range (- high low)))
    (+ (* (/ (random 1000) 1000.0) range) low)))

(defun findLargestOutput (nOutputs &aux (maxIndex 0) (mVal (aref *y-output 0)))
  (dotimes (j nOutputs)
    (if (and
         (> (aref *y-output j) mVal)
         (not (aref *reset j)))
      (setq mVal (aref *y-output j)
          maxIndex j)))
  (if (= *print-detail 2) (afout 'out (format nil "maxIndex= ~a ~%" maxIndex)))
  maxIndex)

; The following function returns d if (aref *y-output index) is the
; largest value in array *y-output AND (aref *y-output index) has not been reset:

(defun g (index nOutputs  &aux j mVal)   ;; (maxIndex (findLargestOutput)))
  (let
      ((maxIndex (findLargestOutput nOutputs))
       )
  (if (and
       (equal index maxIndex)
       (not (aref *reset maxIndex))
       (> (aref *y-output maxIndex) resetThreshold))
    d
    0.0)))

;;was
#|(defun g (index  &aux j mVal (maxIndex (findLargestOutput))) ;; nOutputs)))
  (if (and
       (equal index maxIndex)
       (not (aref *reset maxIndex))
       (> (aref *y-output maxIndex) resetThreshold))
    d
    0.0))|#


; Threshold function:

(defun sigmoid ( x-activity)
  (if (> x-activity theta)
    x-activity
    0.0))

; L2 Norm of a vector:

(defun L2NORM (v vLength &aux (sum 0.0) i j)
  (dotimes (i vLength)
    (setq sum (+ sum (* (aref *v i) (aref *v i)))))
  (+ (sqrt sum) 0.001))

; Update F1 STM arrays:

(defun F1STMcycle (nInputs nOutputs &aux i j sum norm max1 max2)
  ; Calculate p from u input and backwards feed back:
  ;;(break "Beginning of F1STMcycle")
  (afout 'out (format nil "in F1STMcycle, nInputs= ~s ~%" nInputs))
  (dotimes (i nInputs)
    (setq sum 0.0)
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "From inside dotimes(i nInputs), i= ~s, nOutputs= ~s ~%" i nOutputs)))
    (dotimes (j nOutputs)
    (if (= *print-detail 2) (afout 'out (format nil  "FOR j= ~s ~%" j)))
      (if (= *print-detail 2) (afout 'out (format nil  "From inside dotimes(j nOutputs), i= ~s, nOutputs= ~s ~%" j nOutputs)))
      (setq sum (+ sum (* (g j nOutputs) (aref *wdn j i)))))
    (if (= *print-detail 2) (afout 'out (format nil  "sum= ~s; a= ~s ~%" sum a)))
    (setf (aref *p i) (+ (aref *u i) sum))a
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *p i)= ~s ~%" (aref *p i))))
    )
  ; Update q using eq. 5
  (setq norm (+ (L2NORM p nInputs) e))
  (dotimes (i nInputs)
    (setf (aref *q i) (/ (aref *p i) norm))
    )
  ; Update u using eq. 6:
  (setq norm (L2NORM v nInputs))
  (dotimes (i nInputs)
      ;;(break "dotimes (i nInputs) in F1STMcycle")
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *u i)= ~s; norm= ~s ~%" (aref *u i) norm)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *v i)= ~s; norm= ~s ~%" (aref *v i) norm)))
    (setf (aref *u i) (/ (aref *v i) norm)))
  ; Update v using eq. 7:
  (dotimes (i nInputs)
    (setf (aref *v i) (sigmoid (+ (aref *x-activity i) (* b (sigmoid (aref *q i))))))
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *x-activity i)= ~s; b= ~s ~%" (aref *x-activity i) b)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *q i)= ~s; b= ~s ~%" (aref *q i) b)))
    )
  ; Update w using eq. 8:
  (dotimes (i nInputs)
    (setf (aref *w i) (* alpha (+ (aref *input i) (* a (sigmoid (aref *u i))))))
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *w i)= ~s; alpha= ~s ~%" (aref *w i) alpha)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *u i)= ~s; alpha= ~s ~%" (aref *u i) alpha)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *input i)= ~s; ~%" (aref *input i) )))
    )
  ; Update x-activity using eq. 9:
  (setq norm (+ (L2NORM w nInputs) e))
  (dotimes (i nInputs)
    (setf (aref *x-activity i) (/ (aref *w i) norm))
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *x-activity i)= ~s; norm= ~s ~%" (aref *x-activity i) norm)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *u i)= ~s; alpha= ~s ~%" (aref *w i) norm)))
    )

  ; Calculate reset r from eq. 20:
  (setq max1 -1000.0 max2 -1000.0)
  (dotimes (j nInputs)
    (progn
      (if (< max1 (aref *u j)) (setq max1 (aref *u j)))
      (if (< max2 (aref *p j)) (setq max2 (aref *p j)))
      )
    (if (= *print-detail 2) (afout 'out (format nil  "FOR j= ~s ~%" j)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *U j)= ~s; max1= ~s ~%" (aref *u j) max1)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *P j)= ~s; max2= ~s ~%" (aref *p j) max2)))
    )
  (setq max1 (+ max1 0.001))
  (setq max2 (+ max2 0.001))
  (dotimes (i nInputs)
    (setf
     (aref *r i)
     (- (/ (aref *u i) max1) (/ (aref *p i) max2)))
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *u i)= ~s; max1= ~s ~%" (aref *u i) max1)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *p i)= ~s; max2= ~s ~%" (aref *p i) max2)))
    (if (= *print-detail 2) (afout 'out (format nil  "r=~a i=~a; value= ~a ~%" r i (aref *r i))))
    )
  )

; Update F2 STM storage:

(defun F2STMcycle (nInputs nOutputs &aux i j sum)
  (dotimes (j nOutputs)
    (progn
      (setq sum 0.0)
      (dotimes (i nInputs)
        (setq sum (+ sum (* (aref *p i) (aref *wUp i j)))))
      (setf (aref *y-output j) sum)
      (if (aref *reset j) (setf (aref *y-output j) -0.1))
       ;;me
      (afout 'out (format nil  "y=~a j=~a; value= ~a ~%" *y-output j (aref *y-output j)))
      (aref *y-output j)      
      )    
    )  
  )

; Update weights:

(defun updateWeights (nInputs nOutputs  &aux i) ;; (j (findLargestOutput)))
  (let
      ((j (findLargestOutput nOutputs))
       )
  (if (> (g j nOutputs) 0.02)
    (dotimes (i nInputs)
      (setf
       (aref *wdn j i)
       (+ (aref *wdn j i)
          (*
           downLR
           d
           (- (aref *p i) (aref *wdn j i)))))
       ;;me
       (afout 'out (format nil  "j=~a i=~a; value= ~a ~%" j i (aref *wdn j i)))
      (setf
       (aref *wUp i j)
       (+
        (aref *wUp i j)
        (*
         upLR
         d
         (- (aref *p i) (aref *wUp i j)))))
       ;;me
      (afout 'out (format nil  "i=~a j=~a; value= ~a ~%" i j (aref *wUp i j)))   
      ))))

;;was
#|
(defun updateWeights (nInputs   &aux i (j (findLargestOutput)))
  (if (> (g j) 0.02)
    (dotimes (i nInputs)
      (setf
       (aref *wdn j i)
       (+ (aref *wdn j i)
          (*
           downLR
           d
           (- (aref *p i) (aref *wdn j i)))))
       ;;me
       (afout 'out (format nil  "j=~a i=~a; value= ~a ~%" j i (aref *wdn j i)))
      (setf
       (aref *wUp i j)
       (+
        (aref *wUp i j)
        (*
         upLR
         d
         (- (aref *p i) (aref *wUp i j)))))
       ;;me
      (afout 'out (format nil  "i=~a j=~a; value= ~a ~%" i j (aref *wUp i j)))
 
      )))
|#

; Competitive learning at slab F2:

(defun competitiveF2 (nOutputs &aux i) ;; (jj (findLargestOutput)))
  (let
      ((jj (findLargestOutput nOutputs))
       )
  (if (> (aref *y-output jj) resetThreshold)
    (dotimes (i nOutputs)
      (if (not (equal i jj))
        (setf (aref *y-output i) 0.0))))))
;;was
#|
(defun competitiveF2 (nOutputs &aux i (jj (findLargestOutput)))
  (if (> (aref *y-output jj) resetThreshold)
    (dotimes (i nOutputs)
      (if (not (equal i jj))
        (setf (aref *y-output i) 0.0)))))
|#

; Run one full cycle:

(defun OneCycle (nInputs nOutputs &aux i j)
  (F1STMcycle nInputs nOutputs) ;;xxx ;;i j) ;;me added i j bec of error
  (testReset nInputs nOutputs)
  (competitiveF2  nOutputs)
  (F2STMcycle nInputs nOutputs)
  (updateWeights nInputs nOutputs)
  (competitiveF2 nOutputs)
  ;; (updateScreen)
  )

; Check for an F2 reset condition:

(setq skipReset nil)

(defun testReset (nInputs nOutputs &aux (res 0.0) (norm1 0.0) (norm2 0.0)
                       (n1 (+ (L2NORM p nInputs) e)) n2 temp)
  (if (and
       (> n1 0.2)
       (not skipReset))
    (if (> learningCycleCounter 1)
      (if (> (aref *y-output (findLargestOutput nOutputs)) 0.25)
        (setq res (* 3.0 (L2NORM r nInputs))))  ; was 3.0
      (setq skipReset nil)))
  (setf (aref *reset-val 0) res)
  ;;(plotActivations "reset flag" 190 220 reset-val 0.0 1.0)
  (if (> res (- 1.9 vigilance))  ;; me
    (progn
      (print (list "Vigilance reset =" res "  Learning cycle ="
                   learningCycleCounter))
      (setq maxIndex (findLargestOutput nOutputs))
      (setf (aref *reset maxIndex) 1)
      (setf (aref *reset-cntr maxIndex) 80))
    (dotimes (i nOutputs)
      (setf (aref *reset-cntr i) (- (aref *reset-cntr i) 1))
      (if (< (aref *reset-cntr i) 0)
        (progn
          (if (aref *reset i)  (setq skipReset t))
          (setf (aref *reset i) nil)))))
  (setq skipReset nil)) ;; temporary

; Zero activations:
;;mmm
(defun zeroActivations (nInputs nOutputs)
  (dotimes (i nInputs)
    (afout 'out (format nil  "setting aref w-p ~a => 0 ~%" i))
  ;;is this needed??  (setf (aref *w i) 0.0) 
    (setf (aref *x-activity i) 0.0)
    (setf (aref *v i) 0.0)
    (setf (aref *r i) 0.0)
    (setf (aref *u i) 0.0)
    (setf (aref *q i) 0.0)
    (setf (aref *p i) 0.0))
  (dotimes (i nOutputs)
    (setf (aref *y-output i) 0)
   (afout 'out (format nil "setting aref ~a ~a => 0 ~%" *y-output i))
    (setf (aref *reset i) 0)
    (setf (aref *reset-cntr i) 0)))

; Set up a learning pattern in the input neurons:

(defun setPattern (pl nInputs nOutputs &aux (len (length pl)))
  (if (not (equal len nInputs))
    (print (list "Error in setPattern input:" pl))
    (progn
      (setq learningCycleCounter 0)
      (zeroActivations nInputs nOutputs)
      (dotimes (i len)
        (setf (aref *input i) (+ (pop pl) (frandom -0.08 0.08)))))))


;;XXX ------------------------------- NETWORK INITIALIZATION --------------------
;; 
; Initialize the network:

(defun initNetwork (nInputs nOutputs)
  (zeroActivations nInputs nOutputs)
  (dotimes (j nOutputs)
    (progn
      (dotimes (i nInputs)
        (setf
         (aref *wUp i j) (frandom 0.05 0.1)
         (aref *wdn j i) (frandom 0.01 0.03))
        (afout 'out (format nil "wUp ~a ~a => ~a ~%" i j (aref *wUp i j)))
        (afout 'out (format nil  "wUp ~a ~a => ~a ~%" j i (aref *wdn j i)))        
        )
      (setf (aref *n-cats j) 0))))


;; XXX ---------------- LEARN PATTERNS MANAGER FUNCTION ---------------

; Cycle through all training patterns once:

(defun learnPatterns (nInputs nOutputs testPatterns &optional (numCycles 10)) ;; Watson uses num = 50 when calls in original (ART2) was this orig args? (i j) 
    (afout 'out (format nil "In learnPatterns, testPatterns= ~a ~%" testPatterns))

  (setq  learningcyclecounter 0);;2013-11 added global var here

  (dolist (p testPatterns)
     (afout 'out (format nil "Inputs= ~A~%" p))
    (setPattern p nInputs nOutputs)
    (dotimes (i numCycles)
      (setq learningCycleCounter (1+ learningCycleCounter))
      (OneCycle ninputs noutputs)  ;; temp quoted out i j bec of error
      ;;(updateScreen))
    (setq *learned-categories*
          (cons (list p (findLargestOutput nOutputs))
                *learned-categories*)))
    (afout 'out (format nil "At END of learnPatterns, *learned-categories* = ~s ~%"
             *learned-categories*))
    )
  )
   
    







;;********************* OLDER MISC *************************
;;
#| compare to original initialization
    (setq input (make-array (list nInputs)))  ;; inputs (n=5)
        (setq w (make-array (list nInputs)))      ;; w weights (n)
        (setq x (make-array (list nInputs)))      ;;x activity level (n)
        (setq v (make-array (list nInputs)))     ;;
        (setq r (make-array (list nInputs)))
        (setq u (make-array (list nInputs)))
        (setq q (make-array (list nInputs)))
        (setq p (make-array (list nInputs)))
        (setq temp (make-array (list nInputs)))
        (setq reset-val (make-array (list 1)))
        (setq y (make-array (list nOutputs)))     ;;y outputs (3??)
        ;;me
        (afout 'out (format nil "nOutputs= ~a ~%" nOutputs))
        (setq reset (make-array (list nOutputs)))
        (setq resetCounter (make-array (list nOutputs)))
        (setq nCategories (make-array (list nOutputs)))   ;;categories (3)
        (setq temp2 (make-array (list nOutputs)))
        (setq wUp (make-array (list nInputs nOutputs)))    ;;wUp   (??)
        (setq wDown (make-array (list nOutputs nInputs)))   ;;wDown  (??)
        ; Global variable to remember input patterns and
        ; their associated output category code for plotting
        ; by function ART2-Postprocess:
        (setq *learned-categories* nil))))) ;;learned categories
|#