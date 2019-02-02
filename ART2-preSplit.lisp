;;
 ;        Adaptive Resonance 2 Model in Common Lisp.
 ;        Copyright 1990 by Mark Watson.
 ;
 ;        Based on the Adaptive Resonance Theory
 ;        by Gail Carpenter and Stephen Grossberg.
;;
;;syntax error in (DOLIST VALUE-LIST (LIST *INPUTS* *F1* *F2*

(my-config-editor-after-start)

;;TO RUN THIS ART2 PROJECT (From M Watson, Common LISP Modules CH-7 ART2: pp 77ff;
;; TO RUN INSTRUCTIONS p94: (In Listener Window at prompt, type:
;; 1-(ART2-init 5 3); then 2- (ART2) or (art2 :plot? t) then 3- (ART2-postprocess)

(defun load-ART2-files ()
  (load "C:\\TOM\\LISP PROJECTS TS\\MyUtilities\\U-Arrays.lisp")
  (load "C:\\TOM\\LISP PROJECTS TS\\MyUtilities\\U-function-plotter.lisp")
  (load "C:\\TOM\\LISP PROJECTS TS\\ART-LW\\ART2-multipane-interrface.lisp")
  ;;  (load "C:\\TOM\\LISP PROJECTS TS\\ART-LW\\ 
  )
;;USING FIND-SYMBOL MUST TYPE ALL CAPS FOR SYMBOL NAME
;;(find-symbol "ART2-MULTIPANE-INTERFACE" 'common-lisp-user)=> WORKS
;;LOAD ART2 FILES (unless already loaded)

;;SSS PROBLEM LOADING -- GETS NONSENSE ERROR MESSAGES
;;(unless (find-symbol "ART2-MULTIPANE-INTERFACE" 'common-lisp-user)
;;  (load-ART2-files))


(defun testart ()
  (setf out nil)
  (ART2-init 5 3)
  (ART2)
  (fout out)
  )

;;
 ; Define a separate name space package for this software module:
 ;;

;;(make-package :art2)   ;; optional
;;(export '(ART2 ART2-postprocess ART2-init)  :cg-user)  ;; optional

(defparameter *print-detail 1)
(defparameter *learnPatterns-cycles 10)

(setf nInputs 5 nOutputs 3)

; Define some test patterns:

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
      

(setq testPatterns testPatterns6)

;;SSS  CHECK CODE BELOW FOR REDUNDANT--UNEEDED OPERATIONS
;;   THAT WERE DONE AT LOWER LEVEL -- ALREADY ELIMINATED SOME
;;
(defun my-ART2-init (nInputs nOutputs 
                             &key not-return-arrays  no-cell-type-arrays 
                             make-lower-cell-arrays initial-element prefix)
  (setf out nil)
  (let 
      ((symbol-spec-lists
        `((,nInputs ("Inputs" ((0 1)) "" ""))
          (,nInputs ("X-Activity" ((0 1)) "" ""))
          (,nInputs ("V" ((0 1)) "" ""))
          (,nInputs ("R" ((0 1)) "" ""))
          (,nInputs ("U" ((0 1)) "" ""))
          (,nInputs ("Q" ((0 1)) "" ""))
          (,nInputs ("P" ((0 1)) "" ""))
          (,nInputs ("Wup" ((0 1)(0 0)) "" "")) 
          (,nInputs ("Wdn" ((0 1)(1 0)) "" "")) 
          (,nOutputs ("Y-Outputs" ((0 1)) "" ""))
          ;;others
          (,nInputs ("Temp" ((0 1)) "" ""))
          (1 ("reset-Val" ((0 1)) "" ""))
          (,nOutputs ("reset" ((0 1)) "" ""))
          (,nOutputs ("reset-cnt" ((0 1)) "" ""))
          (,nOutputs ("n-cats" ((0 1)) "" ""))
          (,nOutputs ("Temp2" ((0 1)) "" ""))))
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

;;SSS START DEBUGGING HERE -- ALSO &KEY FOR CREATING 
;; LOWER LEVEL ARRAYS
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


      (afout 'out (format nil " (length spec-list1)= ~a (second (car spec-list1))= ~a (length dims-lists1)= ~a~%  "  (length spec-list1) (second (car spec-list1))   (length dims-lists1)  ))
      (afout 'out (format nil "1 PRExxx In new-symbol-type-spec-list-of-lists, spec-list1= ~A~% array-dims1= ~A~%ncells1= ~A  n-dims1= ~a~% dims-lists1= ~a~%" spec-list1 array-dims1 ncells1  n-dims1 dims-lists1 ))

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

;;SSS START HERE  DEBUG MAKE ARRAY FOR EACH TYPE, AND PUT &KEY FOR MAKING FOR EACH CELL
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
(INPUTS X-ACTIVITY V R U Q P WUP WDN Y-OUTPUTS TEMP RESET-VAL RESET RESET-CNT N-CATS TEMP2)
((INPUTS0 INPUTS1 INPUTS2 INPUTS3 INPUTS4) (X-ACTIVITY0 X-ACTIVITY1 X-ACTIVITY2 X-ACTIVITY3 X-ACTIVITY4) (V0 V1 V2 V3 V4) (R0 R1 R2 R3 R4) (U0 U1 U2 U3 U4) (Q0 Q1 Q2 Q3 Q4) (P0 P1 P2 P3 P4) (WUP00 WUP10 WUP20 WUP30 WUP40) (WDN01 WDN11 WDN21 WDN31 WDN41) (Y-OUTPUTS0 Y-OUTPUTS1 Y-OUTPUTS2) (TEMP0 TEMP1 TEMP2 TEMP3 TEMP4) (RESET-VAL0) (RESET0 RESET1 RESET2) (RESET-CNT0 RESET-CNT1 RESET-CNT2) (N-CATS0 N-CATS1 N-CATS2) (TEMP20 TEMP21 TEMP22))
(*INPUTS *X-ACTIVITY *V *R *U *Q *P *WUP *WDN *Y-OUTPUTS *TEMP *RESET-VAL *RESET *RESET-CNT *N-CATS *TEMP2)
((("Inputs" ((0 1)) "" "") ("Inputs" ((1 1)) "" "") ("Inputs" ((2 1)) "" "") ("Inputs" ((3 1)) "" "") ("Inputs" ((4 1)) "" "")) (("X-Activity" ((0 1)) "" "") ("X-Activity" ((1 1)) "" "") ("X-Activity" ((2 1)) "" "") ("X-Activity" ((3 1)) "" "") ("X-Activity" ((4 1)) "" "")) (("V" ((0 1)) "" "") ("V" ((1 1)) "" "") ("V" ((2 1)) "" "") ("V" ((3 1)) "" "") ("V" ((4 1)) "" "")) (("R" ((0 1)) "" "") ("R" ((1 1)) "" "") ("R" ((2 1)) "" "") ("R" ((3 1)) "" "") ("R" ((4 1)) "" "")) (("U" ((0 1)) "" "") ("U" ((1 1)) "" "") ("U" ((2 1)) "" "") ("U" ((3 1)) "" "") ("U" ((4 1)) "" "")) (("Q" ((0 1)) "" "") ("Q" ((1 1)) "" "") ("Q" ((2 1)) "" "") ("Q" ((3 1)) "" "") ("Q" ((4 1)) "" "")) (("P" ((0 1)) "" "") ("P" ((1 1)) "" "") ("P" ((2 1)) "" "") ("P" ((3 1)) "" "") ("P" ((4 1)) "" "")) (("Wup" ((0 1) (0 0)) "" "") ("Wup" ((1 1) (0 0)) "" "") ("Wup" ((2 1) (0 0)) "" "") ("Wup" ((3 1) (0 0)) "" "") ("Wup" ((4 1) (0 0)) "" "")) (("Wdn" ((0 1) (1 0)) "" "") ("Wdn" ((1 1) (1 0)) "" "") ("Wdn" ((2 1) (1 0)) "" "") ("Wdn" ((3 1) (1 0)) "" "") ("Wdn" ((4 1) (1 0)) "" "")) (("Y-Outputs" ((0 1)) "" "") ("Y-Outputs" ((1 1)) "" "") ("Y-Outputs" ((2 1)) "" "")) (("Temp" ((0 1)) "" "") ("Temp" ((1 1)) "" "") ("Temp" ((2 1)) "" "") ("Temp" ((3 1)) "" "") ("Temp" ((4 1)) "" "")) (("reset-Val" ((0 1)) "" "")) (("reset" ((0 1)) "" "") ("reset" ((1 1)) "" "") ("reset" ((2 1)) "" "")) (("reset-cnt" ((0 1)) "" "") ("reset-cnt" ((1 1)) "" "") ("reset-cnt" ((2 1)) "" "")) (("n-cats" ((0 1)) "" "") ("n-cats" ((1 1)) "" "") ("n-cats" ((2 1)) "" "")) (("Temp2" ((0 1)) "" "") ("Temp2" ((1 1)) "" "") ("Temp2" ((2 1)) "" "")))
(("Inputs0" "Inputs1" "Inputs2" "Inputs3" "Inputs4") ("X-Activity0" "X-Activity1" "X-Activity2" "X-Activity3" "X-Activity4") ("V0" "V1" "V2" "V3" "V4") ("R0" "R1" "R2" "R3" "R4") ("U0" "U1" "U2" "U3" "U4") ("Q0" "Q1" "Q2" "Q3" "Q4") ("P0" "P1" "P2" "P3" "P4") ("Wup00" "Wup10" "Wup20" "Wup30" "Wup40") ("Wdn01" "Wdn11" "Wdn21" "Wdn31" "Wdn41") ("Y-Outputs0" "Y-Outputs1" "Y-Outputs2") ("Temp0" "Temp1" "Temp2" "Temp3" "Temp4") ("reset-Val0") ("reset0" "reset1" "reset2") ("reset-cnt0" "reset-cnt1" "reset-cnt2") ("n-cats0" "n-cats1" "n-cats2") ("Temp20" "Temp21" "Temp22"))  NIL NIL
CL-USER 2 > inputs
(INPUTS0 INPUTS1 INPUTS2 INPUTS3 INPUTS4)
CL-USER 3 > *inputs
#(0 0 0 0 0)
|#


   
;;OUTPUT FROM MAKE-ARRAYS IN U-ARRAYS.LISP
;; use for writing functions to initialize ART2
#|
((INPUTS0 INPUTS1 INPUTS2 INPUTS3 INPUTS4) (X-ACTIVITY0 X-ACTIVITY1 X-ACTIVITY2 X-ACTIVITY3 X-ACTIVITY4) (V0 V1 V2 V3 V4) (R0 R1 R2 R3 R4) (U0 U1 U2 U3 U4) (Q0 Q1 Q2 Q3 Q4) (P0 P1 P2 P3 P4) (WUP00 WUP11 WUP22 WUP33 WUP44) (WDN00 WDN11 WDN22 WDN33 WDN44) (Y-OUTPUTS0 Y-OUTPUTS1 Y-OUTPUTS2) (TEMP0 TEMP1 TEMP2 TEMP3 TEMP4) (RESET-VAL0) (RESET0 RESET1 RESET2) (RESET-CNT0 RESET-CNT1 RESET-CNT2) (N-CATS0 N-CATS1 N-CATS2) (TEMP20 TEMP21 TEMP22))
((("Inputs" ((0 1)) "" "") ("Inputs" ((1 1)) "" "") ("Inputs" ((2 1)) "" "") ("Inputs" ((3 1)) "" "") ("Inputs" ((4 1)) "" "")) (("X-Activity" ((0 1)) "" "") ("X-Activity" ((1 1)) "" "") ("X-Activity" ((2 1)) "" "") ("X-Activity" ((3 1)) "" "") ("X-Activity" ((4 1)) "" "")) (("V" ((0 1)) "" "") ("V" ((1 1)) "" "") ("V" ((2 1)) "" "") ("V" ((3 1)) "" "") ("V" ((4 1)) "" "")) (("R" ((0 1)) "" "") ("R" ((1 1)) "" "") ("R" ((2 1)) "" "") ("R" ((3 1)) "" "") ("R" ((4 1)) "" "")) (("U" ((0 1)) "" "") ("U" ((1 1)) "" "") ("U" ((2 1)) "" "") ("U" ((3 1)) "" "") ("U" ((4 1)) "" "")) (("Q" ((0 1)) "" "") ("Q" ((1 1)) "" "") ("Q" ((2 1)) "" "") ("Q" ((3 1)) "" "") ("Q" ((4 1)) "" "")) (("P" ((0 1)) "" "") ("P" ((1 1)) "" "") ("P" ((2 1)) "" "") ("P" ((3 1)) "" "") ("P" ((4 1)) "" "")) (("Wup" ((0 1) (0 1)) "" "") ("Wup" ((1 1) (1 1)) "" "") ("Wup" ((2 1) (2 1)) "" "") ("Wup" ((3 1) (3 1)) "" "") ("Wup" ((4 1) (4 1)) "" "")) (("Wdn" ((0 1) (0 1)) "" "") ("Wdn" ((1 1) (1 1)) "" "") ("Wdn" ((2 1) (2 1)) "" "") ("Wdn" ((3 1) (3 1)) "" "") ("Wdn" ((4 1) (4 1)) "" "")) (("Y-Outputs" ((0 1)) "" "") ("Y-Outputs" ((1 1)) "" "") ("Y-Outputs" ((2 1)) "" "")) (("Temp" ((0 1)) "" "") ("Temp" ((1 1)) "" "") ("Temp" ((2 1)) "" "") ("Temp" ((3 1)) "" "") ("Temp" ((4 1)) "" "")) (("reset-Val" ((0 1)) "" "")) (("reset" ((0 1)) "" "") ("reset" ((1 1)) "" "") ("reset" ((2 1)) "" "")) (("reset-cnt" ((0 1)) "" "") ("reset-cnt" ((1 1)) "" "") ("reset-cnt" ((2 1)) "" "")) (("n-cats" ((0 1)) "" "") ("n-cats" ((1 1)) "" "") ("n-cats" ((2 1)) "" "")) (("Temp2" ((0 1)) "" "") ("Temp2" ((1 1)) "" "") ("Temp2" ((2 1)) "" "")))
(("Inputs0" "Inputs1" "Inputs2" "Inputs3" "Inputs4") ("X-Activity0" "X-Activity1" "X-Activity2" "X-Activity3" "X-Activity4") ("V0" "V1" "V2" "V3" "V4") ("R0" "R1" "R2" "R3" "R4") ("U0" "U1" "U2" "U3" "U4") ("Q0" "Q1" "Q2" "Q3" "Q4") ("P0" "P1" "P2" "P3" "P4") ("Wup00" "Wup11" "Wup22" "Wup33" "Wup44") ("Wdn00" "Wdn11" "Wdn22" "Wdn33" "Wdn44") ("Y-Outputs0" "Y-Outputs1" "Y-Outputs2") ("Temp0" "Temp1" "Temp2" "Temp3" "Temp4") ("reset-Val0") ("reset0" "reset1" "reset2") ("reset-cnt0" "reset-cnt1" "reset-cnt2") ("n-cats0" "n-cats1" "n-cats2") ("Temp20" "Temp21" "Temp22"))
|#
;;-------------------------------------------- older init -------------------------------------

;;mmm
(defun ART2-init (numInputs numOutputs &optional trainingPatterns)
  ; Check for specified training patterns:
  ; me added bec of errors
  (setf NumInputs numInputs NumOutputs numOutputs)
  (afout 'out (format nil "NumInputs= ~a NumOutputs= ~% " NumInputs NumOutputs))
  (if trainingPatterns
    ; Make sure the number of input neurons agrees with
    ; the size of the training patterns:
    (if (equal (length (car trainingPatterns)) numInputs)
      (setq testPatterns trainingPatterns)
      (print
       (list
        "~aERROR: bad input to ART2-init. numInputs should have been"
        (length (car trainingPatterns)))))
    ; No specified training patterns: use the default set
    ; defined in this package:
    (if (not (equal (length (car testPatterns)) numInputs))
      (print
       (list
        "ERROR: bad input to ART2-init. numInputs should have been"
        (length (car testPatterns))))
      ; Specified number of input neurons agrees with
      ; the size of the default training patterns defined
      ; in this package; proceed with defining network data:
      (progn
        ; Define the network size:
        (setq nInputs numInputs)
        (setq nOutputs numOutputs)
        ; Array storage allocation:
        ; added
        (afout 'out (format nil "nInputs= ~a ~%" nInputs))
        (setq input (make-array (list nInputs)))  ;; inputs (n=5)
        (setq w (make-array (list nInputs)))      ;; w weights (n)
        (setq x (make-array (list nInputs)))      ;;x activity level (n)
        (setq v (make-array (list nInputs)))     ;;
        (setq r (make-array (list nInputs)))
        (setq u (make-array (list nInputs)))
        (setq q (make-array (list nInputs)))
        (setq p (make-array (list nInputs)))
        (setq temp (make-array (list nInputs)))
        (setq resetVal (make-array (list 1)))
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
        (setq *learned-categories* nil)))))  ;;learned categories

; Neuron size for plots:

(setq PEsize 18)
(setq PEsizem1 (- PEsize 1))
(setq PEsizep1 (+ PEsize 1))

; Model constants

(defparameter a 0.5 "LTM Weight parameter")
(defparameter b 0.2 "v parameter")
(defparameter c -1.0 "??" )
(defparameter d 0.4 "wUp and wDown parameter")
(defparameter e 0.04 "norm parameter?")
(defparameter theta 0.3 "f(x) Activation criteria")
(defparameter vigilance 0.94 "Sigmoid degree of match criteria, max 1.00")
(defparameter alpha 1.0 "LTM weight w parameter")

(defparameter resetThreshold 0.05 "reset threshold")

(defparameter upLR 0.12 "wUp parameter")
(defparameter downLR 0.12 "wDown parameter")
;;me
(afout 'out (format nil "a= ~a; b= ~a; c= ~a; d= ~a; e= ~a; theta= ~a; vigilance= ~a; alpha= ~a; ~%" a b c d e theta vigilance alpha))

; Floating point  random numbers:

(defun frandom (low high)
  (let ((range (- high low)))
    (+ (* (/ (random 1000) 1000.0) range) low)))

(defun findLargestOutput (&aux (maxIndex 0) (mVal (aref y 0)))
  (dotimes (j nOutputs)
    (if (and
         (> (aref y j) mVal)
         (not (aref reset j)))
      (setq mVal (aref y j)
          maxIndex j)))
  (if (= *print-detail 2) (afout 'out (format nil "maxIndex= ~a ~%" maxIndex)))
  maxIndex)

; The following function returns d if (aref y index) is the
; largest value in array y AND (aref y index) has not been reset:

(defun g (index &aux j mVal (maxIndex (findLargestOutput)))
  (if (and
       (equal index maxIndex)
       (not (aref reset maxIndex))
       (> (aref y maxIndex) resetThreshold))
    d
    0.0))

; Threshold function:

(defun sigmoid (x)
  (if (> x theta)
    x
    0.0))

; L2 Norm of a vector:

(defun L2NORM (v vLength &aux (sum 0.0) i j)
  (dotimes (i vLength)
    (setq sum (+ sum (* (aref v i) (aref v i)))))
  (+ (sqrt sum) 0.001))

; Update F1 STM arrays:

(defun F1STMcycle (&aux i j sum norm max1 max2)
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
      (setq sum (+ sum (* (g j) (aref wDown j i)))))
    (if (= *print-detail 2) (afout 'out (format nil  "sum= ~s; a= ~s ~%" sum a)))
    (setf (aref p i) (+ (aref u i) sum))a
    (if (= *print-detail 2) (afout 'out (format nil  "(aref p i)= ~s ~%" (aref p i))))
    )
  ; Update q using eq. 5
  (setq norm (+ (L2NORM p nInputs) e))
  (dotimes (i nInputs)
    (setf (aref q i) (/ (aref p i) norm))
    )
  ; Update u using eq. 6:
  (setq norm (L2NORM v nInputs))
  (dotimes (i nInputs)
      ;;(break "dotimes (i nInputs) in F1STMcycle")
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref u i)= ~s; norm= ~s ~%" (aref u i) norm)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref v i)= ~s; norm= ~s ~%" (aref v i) norm)))
    (setf (aref u i) (/ (aref v i) norm)))
  ; Update v using eq. 7:
  (dotimes (i nInputs)
    (setf (aref v i) (sigmoid (+ (aref x i) (* b (sigmoid (aref q i))))))
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref x i)= ~s; b= ~s ~%" (aref x i) b)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref q i)= ~s; b= ~s ~%" (aref q i) b)))
    )
  ; Update w using eq. 8:
  (dotimes (i nInputs)
    (setf (aref w i) (* alpha (+ (aref input i) (* a (sigmoid (aref u i))))))
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref w i)= ~s; alpha= ~s ~%" (aref w i) alpha)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref u i)= ~s; alpha= ~s ~%" (aref u i) alpha)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref input i)= ~s; ~%" (aref input i) )))
    )
  ; Update x using eq. 9:
  (setq norm (+ (L2NORM w nInputs) e))
  (dotimes (i nInputs)
    (setf (aref x i) (/ (aref w i) norm))
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref x i)= ~s; norm= ~s ~%" (aref x i) norm)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref u i)= ~s; alpha= ~s ~%" (aref w i) norm)))
    )

  ; Calculate reset r from eq. 20:
  (setq max1 -1000.0 max2 -1000.0)
  (dotimes (j nInputs)
    (progn
      (if (< max1 (aref u j)) (setq max1 (aref u j)))
      (if (< max2 (aref p j)) (setq max2 (aref p j)))
      )
    (if (= *print-detail 2) (afout 'out (format nil  "FOR j= ~s ~%" j)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref U j)= ~s; max1= ~s ~%" (aref u j) max1)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref P j)= ~s; max2= ~s ~%" (aref p j) max2)))
    )
  (setq max1 (+ max1 0.001))
  (setq max2 (+ max2 0.001))
  (dotimes (i nInputs)
    (setf
     (aref r i)
     (- (/ (aref u i) max1) (/ (aref p i) max2)))
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref u i)= ~s; max1= ~s ~%" (aref u i) max1)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref p i)= ~s; max2= ~s ~%" (aref p i) max2)))
    (if (= *print-detail 2) (afout 'out (format nil  "r=~a i=~a; value= ~a ~%" r i (aref r i))))
    )
  )

; Update F2 STM storage:

(defun F2STMcycle (&aux i j sum)
  (dotimes (j nOutputs)
    (progn
      (setq sum 0.0)
      (dotimes (i nInputs)
        (setq sum (+ sum (* (aref p i) (aref wUp i j)))))
      (setf (aref y j) sum)
      (if (aref reset j) (setf (aref y j) -0.1))
       ;;me
      (afout 'out (format nil  "y=~a j=~a; value= ~a ~%" y j (aref y j)))
      (aref y j)      
      )    
    )  
  )

; Update weights:

(defun updateWeights (&aux i (j (findLargestOutput)))
  (if (> (g j) 0.02)
    (dotimes (i nInputs)
      (setf
       (aref wDown j i)
       (+ (aref wDown j i)
          (*
           downLR
           d
           (- (aref p i) (aref wDown j i)))))
       ;;me
       (afout 'out (format nil  "j=~a i=~a; value= ~a ~%" j i (aref wDown j i)))
      (setf
       (aref wUp i j)
       (+
        (aref wUp i j)
        (*
         upLR
         d
         (- (aref p i) (aref wUp i j)))))
       ;;me
      (afout 'out (format nil  "i=~a j=~a; value= ~a ~%" i j (aref wUp i j)))
      
      )))

; Competitive learning at slab F2:

(defun competitiveF2 (&aux i (jj (findLargestOutput)))
  (if (> (aref y jj) resetThreshold)
    (dotimes (i nOutputs)
      (if (not (equal i jj))
        (setf (aref y i) 0.0)))))

; Run one full cycle:

(defun OneCycle (&aux i j)
  (F1STMcycle) ;;xxx ;;i j) ;;me added i j bec of error
  (testReset)
  (competitiveF2)
  (F2STMcycle)
  (updateWeights)
  (competitiveF2)
  ;; (updateScreen)
  )

; Check for an F2 reset condition:

(setq skipReset nil)

(defun testReset (&aux (res 0.0) (norm1 0.0) (norm2 0.0)
                       (n1 (+ (L2NORM p nInputs) e)) n2 temp)
  (if (and
       (> n1 0.2)
       (not skipReset))
    (if (> learningCycleCounter 1)
      (if (> (aref y (findLargestOutput)) 0.25)
        (setq res (* 3.0 (L2NORM r nInputs))))  ; was 3.0
      (setq skipReset nil)))
  (setf (aref resetVal 0) res)
  ;;(plotActivations "reset flag" 190 220 resetVal 0.0 1.0)
  (if (> res (- 1.9 vigilance))  ;; me
    (progn
      (print (list "Vigilance reset =" res "  Learning cycle ="
                   learningCycleCounter))
      (setq maxIndex (findLargestOutput))
      (setf (aref reset maxIndex) 1)
      (setf (aref resetCounter maxIndex) 80))
    (dotimes (i nOutputs)
      (setf (aref resetCounter i) (- (aref resetCounter i) 1))
      (if (< (aref resetCounter i) 0)
        (progn
          (if (aref reset i)  (setq skipReset t))
          (setf (aref reset i) nil)))))
  (setq skipReset nil)) ;; temporary

; Zero activations:

(defun zeroActivations ()
  (dotimes (i nInputs)
    (afout 'out (format nil  "setting aref w-p ~a => 0 ~%" i))
    (setf (aref w i) 0.0)
    (setf (aref x i) 0.0)
    (setf (aref v i) 0.0)
    (setf (aref r i) 0.0)
    (setf (aref u i) 0.0)
    (setf (aref q i) 0.0)
    (setf (aref p i) 0.0))
  (dotimes (i nOutputs)
    (setf (aref y i) 0)
   (afout 'out (format nil "setting aref ~a ~a => 0 ~%" y i))
    (setf (aref reset i) 0)
    (setf (aref resetCounter i) 0)))

; Set up a learning pattern in the input neurons:

(defun setPattern (pl &aux (len (length pl)))
  (if (not (equal len nInputs))
    (print (list "Error in setPattern input:" pl))
    (progn
      (setq learningCycleCounter 0)
      (zeroActivations)
      (dotimes (i len)
        (setf (aref input i) (+ (pop pl) (frandom -0.08 0.08)))))))

; Initialize the network:

(defun initNetwork ()
  (zeroActivations)
  (dotimes (j nOutputs)
    (progn
      (dotimes (i nInputs)
        (setf
         (aref wUp i j) (frandom 0.05 0.1)
         (aref wDown j i) (frandom 0.01 0.03))
        (afout 'out (format nil "wUp ~a ~a => ~a ~%" i j (aref wUp i j)))
        (afout 'out (format nil  "wUp ~a ~a => ~a ~%" j i (aref wDown j i)))        
        )
      (setf (aref nCategories j) 0))))


; Cycle through all training patterns once:

(defun learnPatterns (&optional (numCycles 10)) ;; Watson uses num = 50 when calls in original (ART2) was this orig args? (i j) 
    (afout 'out (format nil "In learnPatterns, testPatterns= ~a ~%" testPatterns))
  (dolist (p testPatterns)
     (afout 'out (format nil "Inputs= ~A~%" p))
    (setPattern p)
    (dotimes (i numCycles)
      (setq learningCycleCounter (1+ learningCycleCounter))
      (OneCycle)  ;; temp quoted out i j bec of error
      ;;(updateScreen))
    (setq *learned-categories*
          (cons (list p (findLargestOutput))
                *learned-categories*)))
    (afout 'out (format nil "At END of learnPatterns, *learned-categories* = ~s ~%"
             *learned-categories*))
    )
  )
   
    



#|(defun convert-array-statesXX ()
  "In ART2.lisp, converts array data to formated list data for use in graphing, etc"
  (let
      ((x)
       )
        (setq input (make-array (list nInputs)))  ;; inputs (n=5)
        (setq w (make-array (list nInputs)))      ;; w weights (n)
        (setq x (make-array (list nInputs)))      ;;x activity level (n)
        (setq v (make-array (list nInputs)))
        (setq r (make-array (list nInputs)))
        (setq u (make-array (list nInputs)))
        (setq q (make-array (list nInputs)))
        (setq p (make-array (list nInputs)))
        (setq temp (make-array (list nInputs)))
        (setq resetVal (make-array (list 1)))
        (setq y (make-array (list nOutputs)))     ;;y outputs (3??)
        ;;me
        (afout 'out (format nil "nOutputs= ~a ~%" nOutputs))
        (setq reset (make-array (list nOutputs)))
        (setq resetCounter (make-array (list nOutputs)))
        (setq nCategories (make-array (list nOutputs)))   ;;categories (3)
        (setq temp2 (make-array (list nOutputs)))
        (setq wUp (make-array (list nInputs nOutputs)))    ;;wUp   (??)
        (setq wDown (make-array (list nOutputs nInputs))) 
))|#


;ART-MANAGER FUNCTION
;;
;;sss mmm
;;ddd
(defun art-manager (n-input-cells n-output-cells  initial-x incr-x)
  (setf out nil)

  ;;these may have been set to other values--this initializes them
  (setf *I-points nil *x-points nil *wup0-points nil *wup1-points nil *wdn0-points nil *wdn1-points nil *y-points nil)
  (let
      ((symbol-list '(*I-points *x-points (*wup0-points *wup1-points)( *wdn0-points *wdn1-points) *y-points))
   (*x-list '(x0 x1 x2 x3 x4))
   (*wup0-list  '(wu00 wu01 wu02 wu03 wu04))
   (*wup1-list '(wu10 wu11 wu12 wu13 wu14))
   (*wdn0-list '(wd00 wd01 wd02 wd03 wd04))
   (*wdn1-list '(wd10 wd11 wd12 wd13 wd14))
   (*Y-cell-list '(y1 y2 y3))
   (array-list)
;;doesn't work, arrays not set yet??   (array-list '(input  x wUp wDown y))
   (initial-x 20)
   (incr-x 20)   
   )
    ;;INITIALIZE ART2 -- NOTE REDUNDANCY IN DEF OF SOME VARS
    (ART2-init 5 3)

    ;;RUN ART2
    (ART2)
    ;;now make the array list
    (setf array-list `(,input  ,x  ,wUp ,wDown ,y))
   ;;convert the ART2 array values to x, y coordinates in lists
   (convert-ART2-state array-list symbol-list  initial-x incr-x)
   ))

;;following works, see below
#|(defun testcas ()
  (setf out nil)
  ;;these may have been set to other values--this initializes them
  (setf *I-points nil *x-points nil *wup0-points nil *wup1-points nil *wdn0-points nil *wdn1-points nil *y-points nil)
  (let
      ((symbol-list '(*I-points *x-points (*wup0-points *wup1-points)( *wdn0-points *wdn1-points) *y-points))
   (*x-list '(x0 x1 x2 x3 x4))
   (*wup0-list  '(wu00 wu01 wu02 wu03 wu04))
   (*wup1-list '(wu10 wu11 wu12 wu13 wu14))
   (*wdn0-list '(wd00 wd01 wd02 wd03 wd04))
   (*wdn1-list '(wd10 wd11 wd12 wd13 wd14))
   (*Y-cell-list '(y1 y2 y3))
   (array-list)
;;doesn't work, arrays not set yet??   (array-list '(input  x wUp wDown y))
   (initial-x 20)
   (incr-x 20)   
   )
    ;;INITIALIZE ART2 -- NOTE REDUNDANCY IN DEF OF SOME VARS
    (ART2-init 5 3)

    ;;RUN ART2
    (ART2)
    ;;now make the array list
    (setf array-list `(,input  ,x  ,wUp ,wDown ,y))
   ;;convert the ART2 array values to x, y coordinates in lists
   (convert-ART2-state array-list symbol-list  initial-x incr-x)
   ))|#
;;works returns=>
;;(((0 (20 -0.04624)) (1 (40 0.16976)) (2 (60 0.90496)) (3 (80 0.45296)) (4 (100 0.060640008))) ((0 (20 -0.029565735)) (1 (40 0.108544104)) (2 (60 0.8645607)) (3 (80 0.43200946)) (4 (100 0.038773064))) ((0 (20 0.027807182)) (1 (40 0.12025459)) (2 (60 0.68849814)) (3 (80 0.3401771)) (4 (100 0.030526527))) ((0 (20 0.6114355)) (1 (40 0.17182535)) (2 (60 0.026882045)) (3 (80 0.026259305)) (4 (100 0.3709189))) ((0 (20 0.008810705)) (1 (40 0.5814226)) (2 (60 0.019639999))) ((0 (20 0.108088605)) (1 (40 0.14640773)) (2 (60 0.02894))) ((0 (20 0.95934576)) (1 (40 0.0)) (2 (60 0.0))))
;;(*I-POINTS *X-POINTS *WUP0-POINTS *WUP1-POINTS *WDN0-POINTS *WDN1-POINTS *Y-POINTS)


;;mmm

;;CONVERT-ART2-STATE
;;
;;ddd
(defun convert-ART2-state (array-list symbol-list  initial-x incr-x)
  "In ART2.lisp, converts ART2 state into lists etc that can be easily processed by my graphing etc functions. NOTE:  initial-x and incr-x can be a value OR a list of values--single value used for all, list values must match num cells. Takes 1dim or 2dim arrays--symbols of 2D arrays in nested list."
  (let
      ( (num-cells 0)
        (initial-x-coord 0)
        (incr-x-coord 20)
        (symbol 'unknown)
        (converted-array-list)
        (converted-arrays-lists)
        (num-cells 0)
        (array-dimensions)
        (new-symbol)
        (new-symbols-list)
        )
    (afout 'out (format nil "In convert-ART2-state, array-list= ~A~% symbol-list= ~A~%(eval input)= ~A" array-list symbol-list (eval (car array-list))))

    (loop
     for array in array-list 
     for i from 0 to (length array-list)     

     do
     (setf array-dimensions (array-dimensions array)
           n-dims (length array-dimensions)
           num-cells (car array-dimensions))
     (afout 'out (format nil "In  array-dimensions= ~A n-dims= ~A  num-cells= ~A~% initial-x= ~A " array-dimensions n-dims num-cells initial-x))
     
         (if (listp initial-x)
             (setf initial-x-coord (nth i initial-x))
           (setf initial-x-coord initial-x))
         (if (listp incr-x)
             (setf incr-x-coord (nth i incr-x))
           (setf incr-x-coord incr-x))
         ;;find the symbol (could be a nested list)
         (setf symbol (nth i symbol-list))

       (dotimes ( j-dim n-dims)  ;;j-dim becomes 0 or 1
         ;;apply the conversion function
         (multiple-value-setq (converted-arrays-list new-symbol)
               (convert-ART2-arrays-to-lists array j-dim  num-cells symbol initial-x-coord incr-x-coord))
         ;;modify the summated lists
         (setq converted-arrays-lists (append converted-arrays-lists (list converted-arrays-list))
               new-symbols-list (append new-symbols-list (list new-symbol)))
         ;;end loop and dotimes
         ))      
    (values  converted-arrays-lists new-symbols-list)))



;;CONVERT-ART2-ARRAYS-TO-LISTS
;;
;;ddd
(defun convert-ART2-arrays-to-lists (array array-dim num-cells list-symbol initial-x incr-x)
  "In ART2.lisp, converts one array to one graph-data-list-structure"
  (let
      ((num-dims (length (array-dimensions array)))
       (array-value)
       (item-list)
       (x-val initial-x)
       (all-values-list)
       (symbol)
       )

    (loop
     for cell-i from 0 to  (- num-cells 1)
     do
     (cond
      ((= num-dims 1)
       (setf  value (aref array cell-i)
              symbol list-symbol))
      ((= num-dims 2)
       (setf  value (aref array cell-i array-dim)
              symbol (nth array-dim list-symbol))))

      ;;append the graph all-values-list (if a 2D array, add to flattened--not nested--list
     (setf item-list `(,cell-i (,x-val  ,value))
            all-values-list (append all-values-list (list item-list)))
     (setf x-val (+ x-val incr-x))
     ;;end loop
     )
    (set symbol all-values-list)
    (values all-values-list symbol)))



#|(defun testcaa1 ()
  (let
     ((array1 (make-array '(4) :initial-contents '(2 4 6 8)))
      (result)
      )
  (setf result  (convert-ART2-arrays-to-lists array1 0 4 'testx1 20  20))
  (values result testx1)))|#
;;works if 1 D array,  returns=>
;;((0 (20 2)) (1 (40 4)) (2 (60 6)) (3 (80 8)))
;;((0 (20 2)) (1 (40 4)) (2 (60 6)) (3 (80 8)))

#|(defun testcaa2 ()
  (let
     ((array1 (make-array '(4 2) :initial-contents '((1 2)(3 4)(5 6)(7 8))))
      (result)
      (testx1)
      (testx2)
      (symbol)
      )
  (multiple-value-setq (result symbol)
     ;;for array-dim 0 (convert-ART2-arrays-to-lists array1 0 4 '(testx1 testx2)  20  20)
     ;;for array-dim 1
      (convert-ART2-arrays-to-lists array1 1 4 '(testx1 testx2)  20  20)
    )
  (values result symbol)))    |#
;;for a 2D array for array-dim 0 above -- works =>
;;((0 (20 1)) (1 (40 3)) (2 (60 5)) (3 (80 7)))
;;TESTX1
;;results for a 2D array for array-dim 1 =>
;;((0 (20 2)) (1 (40 4)) (2 (60 6)) (3 (80 8)))
;;TESTX2
     
  

;;test
;;(concatenate 'list "abc" '(x y z)) => (#\a #\b #\c X Y Z)


 ;;;(defparameter *ART2-values*
;;;  '(((Cell 0 1) :Input1 0.9 :Input2 0.7  :Input3 0.2 :Input4 0.9)
;;;    ((Cell 0 2) :Input1 0.1 :Input2 0.04 :Input3 0.2 :Input4 0.2)
;;;    ((Cell 0 3) :Input1 0.5 :Input2 0.6  :Input3 0.6 :Input4 0.6)
;;;    ((Cell 0 4) :Input1 0.7 :Input2 0.3  :Input3 0.5 :Input4 0.5)
  ;;;    ((Cell 0 5) :Input1 0.2 :Input2 0.05 :Input3 0.1 :Input4 0.3 )
  ;;;   ((Activity x x)  :Input1 0.2 :Input2 0.05 :Input3 0.1 :Input4 0.3)
;;;    ((Output fx y) :Input1 0.5 :Input2 0.6  :Input3 0.6 :Input4 0.6)
;;;     )) 

;;;  '(((Cell 0 1) :Input1 (aref input  :Input2 0.7  :Input3 0.2 :Input4 0.9)



;;short version??
;;
(defun ART2 (&key (plot? nil))   
  (setf *learned-categories* nil)
  ;;;  (if plot?
  ;;;      (init-plot "Adaptive Resonance Theory" 250 290))
  (initNetwork)
  (learnPatterns *learnPatterns-cycles)
)

#|
 ;;Main test program:
(defun ART2 (&key (plot? nil))   
  (setf *learned-categories* nil)
  ;;;  (if plot?
  ;;;      (init-plot "Adaptive Resonance Theory" 250 290))
  (initNetwork)
  (learnPatterns *learnPatterns-cycles)

;;(setf value-list1 (list *inputs* *F1* *F2* *last-learned-categories*))
  ;;my added
  (setf 
    *inputs* '(.1 .9 .1 .9 .1) ;;change
    *F1*     '(.2 .7 .2 .8 .0) ;;change
    *F2*     '(.15 .8 .15 .7 .2) ;;change
    *last-learned-categories* (caar (last *learned-categories*)) ;; '(1 1 1 1 1) 
   )
  (setf label-list1 '(:Output-fx :wUp :wDown)
        input-label-list ;;  "something missingin code  here")
  (dotimes (n NumInputs)
    (let* ((cell (list 'Cell 0 n))
           (cell-list (list cell))
           )

      (dolist value-list (list *inputs* *F1* *F2* *last-learned-categories*)
        (let ((value (nth n value-list))
          (cell-list (append cell-list value)
))
    (setf cell-list (append cell-list  '("xxx someting missing in code here")))
  
         

  (setf *ART2-values* '("xxx someting missing in code here")



    ) 
  (afout 'out (format nil "*ART2-values* = ~A ~%" *ART2-values*))
  (if plot?
      (init-plot "ART2 Last Learned Categories" 700 700)  
    )
  )

;;added to balance parens
)))))
|#

;;;original follows ----------------------------------------------------------

#|
; Cycle through all training patterns once:

;;;(defun learnPatterns (num)
;;;  (dolist (p testPatterns)
;;;    (print (list "Inputs:" p))
;;;    (setPattern p)
;;;    (dotimes (i num)
;;;      (setq learningCycleCounter (1+ learningCycleCounter))
;;;      (OneCycle)
;;;      (updateScreen))
;;;    (setq *learned-categories*
;;;          (cons (list p (findLargestOutput))
;;;                *learned-categories*))))

 ; Graphics support:

(defun plotActivations (title x y data dmin dmax)
  (let ((size (array-dimension data 0)) (ypos 0) (xpos x))
    (plot-string-italic (- x 47) (+ y 7) title)
    (dotimes (i size)
      (if (< size 20)
        (setq ypos y xpos (+ x (* i PEsizep1)))
        (if (< i (/ size 2))
          (setq ypos (- y 7) xpos (+ x (* i PEsizep1)))
          (setq ypos (+ y 2)  xpos (+ x (* (- i (/ size 2)) PEsizep1)))))
      (plot-size-rect
       xpos ypos PEsizem1 PEsizem1
       (max
        1
        (truncate (* (/  (- (aref data i) dmin) (- dmax dmin)) PEsize))))
      (plot-frame-rect xpos ypos PEsize PEsize))))
  
(defun updateScreen ()
  (plotActivations "inputs" 50 10 input 0.0 1.0)
  (plotActivations "w" 50 30 w 0.0 1.0)
  (plotActivations "x" 50 50 x 0.0 1.0)
  (plotActivations "v" 50 70 v 0.0 1.0)
  (plotActivations "r" 50 90 r 0.0 1.0)
  (plotActivations "u" 50 110 u 0.0 1.0)
  (plotActivations "q" 50 130 q 0.0 1.0)
  (plotActivations "p" 50 150 p 0.0 1.0)
  (plotActivations "outputs" 50 200 y 0.0 1.0)
  (dotimes (i nOutputs)
    (if (aref reset i)
      (setf (aref temp2 i) 1.0)
      (setf (aref temp2 i) 0.0)))
  (plotActivations "reset" 50 220 temp2 0.0 1.0))

 ; Main test program:

(defun ART2 ()
  (setq *learned-categories* nil)
  (my-init-plot "Adaptive Resonance Theory" 250 290)
  (initNetwork)
  (learnPatterns 50))

;;
 ;         Standalone program to plot the input
 ;         patterns and associated output category
 ;         codes produced by executing program ART2:
 ;;

(defun ART2-Postprocess (&aux (width 130) category-counts)

  (defun plot-input (x-org y-org values)
    (let ((delta-x (/ (- width 20) (length values))))
      (plot-line x-org y-org x-org (- y-org 20))
      (plot-line x-org y-org (+ x-org (- width 40)) y-org)
      (dotimes (n (- (length values) 1))
        (plot-line
         (+ x-org (* n delta-x))
         (- y-org (* 20 (nth n values)))
         (+ x-org (* (1+ n) delta-x))
         (- y-org (* 20 (nth (1+ n) values)))))))

  (setq category-counts (copy-tree '(0 0 0)))
  (init-plot "ART2: Category codes for input patterns" 382 270)
  (plot-string-italic 20 15 "Category 1")
  (plot-string-italic (+ 20 width) 15 "Category 2")
  (plot-string-italic (+ 20 width width) 15 "Category 3")
  (dolist (p *learned-categories*)
    (print (list "inputs:" (car p) "category:" (cadr p)))
    (let ((pattern (car p))
          (category-code (cadr p)))
      (plot-input
       (+ 20 (* category-code width)) ;; x coord of plot origin
       (+ 40 (* (nth category-code category-counts) 40)) ;; y coord
       pattern)
      (setf (nth category-code category-counts)
        (1+ (nth category-code category-counts))))))

;;TESTING      
 ;;works (setf xy (make-list 3 :initial-element 5)) returns (5 5 5)
;;works  (setf ar99 (make-array xy :initial-element 0)) returns #3A(((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)) ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)) ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)) ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)) ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)))
|#