;;************************* ART2-array-data-analysis.lisp **********************
;;
;;Functions for converting from Arrays to lists for  use in data analysis and graphing
;;
;;

;;ART POST-RUNART  DATA ANALYSIS LIST STRUCTURES -------------------
;;
;; 1. EACH CELL-SYM (eg. input1) IS SET TO A LIST = (celln (x-axis-pixel value) "label")
;;CL-USER 61 > input1
;;(1 (80 0.040319994))
;;
;; 2. EACH CELL-TYPE (eg. input) IS SET TO A LIST OF ALL CELLS OF THAT EXACT TYPE (eg. (INPUT0 INPUT1 INPUT2 INPUT3 INPUT4 INPUT5 INPUT6 INPUT7 INPUT8)) -- returned as second value of (make-points-list input)
;; 
;; 3. (make-points-list input) RETURNS VALUES 
#|CL-USER 60 > (make-points-list input)
((0 (40 -0.07712)) (1 (80 0.040319994)) (2 (120 0.063999996)) (3 (160 1.0504)) (4 (200 0.92752)) (5 (240 0.92656)) (6 (280 0.04)) (7 (320 -0.052)) (8 (360 0.021280006)))
(INPUT0 INPUT1 INPUT2 INPUT3 INPUT4 INPUT5 INPUT6 INPUT7 INPUT8)
|#

;; -------------- POST-RUNART DATA-ANALYSIS FUNCTIONS ------------------



;; (testami)

;;; --NEXT MAKE SO WILL REDRAW WHEN ICONIFY MAXIMIZED
;;(setf  *art2-instance (make-instance 'ART2-multipane-interface))
(defun initialize-art2-data ()
  (setf 
   ;;THE POINTS LISTS
   *I-points (make-points-list input)
   ;;was'((0 (40 0) "0") (1 (80 0.4) "1") (2 (120 0.9) "2") (3 (160 0.4) "3") (4 (200 0) "4"))
   *x-points (make-points-list x-activity)
   ;;was'((0 (40 0) "0") (1 (80 0.4) "1") (2 (120 0.7) "2") (3 (160 0.4) "3") (4 (200 0.4) "4"))
#| doesn't work because  double-indexes  *wup-points (make-points-list wup)
   ;;was'((0 (40 0) "0") (1 (80 0.2) "1") (2 (120 0.3) "2") (3 (160 0.2) "3") (4 (200 0.1) "4"))
   *wdn-points (make-points-list wdn)
   ;;was'((0 (40 0) "0") (1 (80 0.0) "1") (2 (120 0.1) "2") (3 (160 0.4) "3") (4 (200 0.2) "4"))|#
   *reset-points (make-points-list reset)
   *y-points (make-points-list y-output)
   ;;was'((0 (40 0) "0")  (1 (80 1.0) "1")  (2 (120 0) "2"))
   ;;mmm  ;;To make graph of ALL reset vals in each frame

   ;;THE CELL NAMES
   *I-cell-list input
   ;;was'(In0 In1 In2 In3 In4)
   *x-list x-activity
   ;;was'(x0 x1 x2 x3 x4)
   *wup-list wup
   ;;was'(wu0 wu1 wu2 wu3 wu4)
   *wdn-list wdn
   ;;was'(wd0 wd1 wd2 wd3 wd4)
   *Y-cell-list  y-output
   ;;was'(y1 y2 y3))
   *reset-list  reset
  ))


;; (make-points-list input)
(defun make-points-list (cell-type)
  "In ART2-multipane-interface.lisp, makes a graph points list from eg. input, x-activity"
  (let
      ((points-list)
       )
    (dolist (cell cell-type)
      (setf points-list (append points-list (list (eval cell))))
      )
    (values points-list cell-type)
    ))
;;TEST
;;works
#|CL-USER 21 > (make-points-list input)
((0 (20 0.076000005)) (1 (40 0.23568)) (2 (60 0.91568)) (3 (80 0.37008)) (4 (100 -0.07216)))
(INPUT0 INPUT1 INPUT2 INPUT3 INPUT4)
|#




;;CONVERT-ART2-ARRAYS-TO-LISTS
;;
;;ddd
(defun convert-ART2-arrays-values-to-lists (array-list symbol-root-list 
                                            &key   (initial-x 0) (incr-x 40)
                                            (from-field-n 1)(from-cells :all)
                                            (to-field-n  2) (to-cells :all)
                                            (from-index0 1)(to-index0 1)
                                            add-label-p)
  "In ART2.lisp, converts a list of arrays to  into lists etc that can be easily processed by my graphing etc functions. NOTE:  initial-x and incr-x can be a value OR a list of values--single value used for all, list values must match num cells. Takes 1dim or 2dim arrays--symbols of 2D arrays in nested list eg. ((sym01 sym02...)(sym11 sym12...)).  RETURNS (values  converted-arrays-lists new-symbols-list)  CONVERTED-ARRAYS-LISTS eg. (PUT EG HERE SSS  )."
  (let
      ( (num-cells 0)
        (initial-x-coord 0)
        (incr-x-coord 40)
        (symbol 'unknown)
        (converted-array-list)
        (converted-arrays-lists)
        (num-cells 0)
        (array-dimensions)
        (symbol-root)
        (new-symbol)
        (new-symbols-lists)
        )
    ;;  (afout 'out (format nil "In convert-ART2-state, symbol-root-list= ~A~% graph-symbol-list= ~A~%(eval input)= ~A" symbol-root-list graph-symbol-list (eval (car symbol-root-list))))
     (loop
      for array in array-list ;;eg '(input x-activity ...)
      for i from 0 to (- (length array-list) 1)    
      do
      (afout 'out (format nil "AAA 1 array= ~A~%" array))
      ;;array may be a symbol set to an array
      (unless (arrayp array)
        (setf array (eval array)))  ;;eg eval *input to get input array

      (setf array-dimensions (array-dimensions array)  ;;produces a list of array dims
            n-dims (length array-dimensions)
            num-cells (car (last array-dimensions))) ;;num cells is always first dim
      (afout 'out (format nil "InAAA 2 array= ~A~%  array-dimensions= ~A n-dims= ~A  num-cells= ~A~% initial-x= ~A " array array-dimensions n-dims num-cells initial-x))
     
      ;;IF INITIAL-X IS A LIST THEN IT SPECIFIES VARYING PARAMETERS
      (if (listp initial-x)
          (setf initial-x-coord (nth i initial-x))
        (setf initial-x-coord initial-x))
      (if (listp incr-x)
          (setf incr-x-coord (nth i incr-x))
        (setf incr-x-coord incr-x))
      ;;find the symbol (could be a nested list)  ;;nnn
      (setf symbol-root (nth i symbol-root-list))

      ;;MAKE THE CELL/VAR GRAPHING LISTS (includes x, y coordinates)
      (dotimes ( j-dim n-dims)  ;;j-dim becomes 1 or 2
        ;;apply the conversion function
        (multiple-value-setq (converted-arrays-list new-symbols-list) ;was new-symbol
            (convert-ART2-array-values-to-list  array  
                                                :symbol-root symbol-root
                                                :initial-x initial-x-coord :incr-x incr-x-coord
                                                :from-field-n from-field-n :to-field-n to-field-n
                                                :from-cells from-cells  :to-cells to-cells
                                                :from-index0 from-index0 :to-index0 to-index0
                                                :add-label-p add-label-p))

        ;;;SSS ADDS ACTUAL ARRAY NOT ARRAY NAME TO THE LIST, FIX IT.

        ;;modify the summated lists
        (setq converted-arrays-lists
              (append converted-arrays-lists (list converted-arrays-list))
              new-symbols-lists
              (append new-symbols-lists (list new-symbols-list)))
        ;;(afout 'out (format nil "AAA 3 j-dim= ~A converted-arrays-lists= ~A~%new-symbols-lists= ~A~%"j-dim converted-arrays-lists new-symbols-lists))
        ;;was   new-symbols-list (append new-symbols-list (list new-symbol)))
        ;;end inner and outer loops and dotimes
        ))      
    (values  converted-arrays-lists new-symbols-lists)))
;;TEST  TTT
;; (testca)
;;mmm2
(defun testca ()
      (let*
          ((symbol-root-list '(input x-activity wUp wDn y-output))
         ;;  (symbol-list-of-lists `(,input ,x-activity  ,wUp  ,wdn  ,y-output))
           (array-list '(*input  *x-activity  *wUp *wdn *y-output))           
          ;; (array-list-of-lists `(,*input  ,*x-activity  ,*wUp ,*wdn ,*y-output))
          (initial-x 20)
          (incr-x 20)
          (add-label-p T)
            )             
        (convert-ART2-arrays-values-to-lists array-list symbol-root-list
        ;;array-graph-sym-types-list  graph-symbol-lists
                                             ;;  :symbol-root symbol
                                                  :initial-x initial-x :incr-x incr-x
                                               ;;   :from-field-n from-field-n :to-field-n to-field-n
                                               ;;   :from-cells from-cells  :to-cells to-cells
                                               ;;   :from-index0 from-index0 :to-index0 to-index0
                                                  :add-label-p add-label-p)
      ))
;; REPLACE WITH REAL OUTPUT FROM ART2?
;;RETURNS= (((1 (20 0) "INPUT1") (2 (40 0) "INPUT2") (3 (60 0) "INPUT3") (4 (80 0) "INPUT4") (5 (100 0) "INPUT5")) ((1 (20 0) "X-ACTIVITY1") (2 (40 0) "X-ACTIVITY2") (3 (60 0) "X-ACTIVITY3") (4 (80 0) "X-ACTIVITY4") (5 (100 0) "X-ACTIVITY5")) (((1 (1 (20 0)) WUP11) (1 (2 (40 0)) WUP21) (1 (3 (60 0)) WUP31) (1 (4 (80 0)) WUP41) (1 (5 (100 0)) WUP51)) ((2 (1 (20 0)) WUP12) (2 (2 (40 0)) WUP22) (2 (3 (60 0)) WUP32) (2 (4 (80 0)) WUP42) (2 (5 (100 0)) WUP52)) ((3 (1 (20 0)) WUP13) (3 (2 (40 0)) WUP23) (3 (3 (60 0)) WUP33) (3 (4 (80 0)) WUP43) (3 (5 (100 0)) WUP53)) ((4 (1 (20 0)) WUP14) (4 (2 (40 0)) WUP24) (4 (3 (60 0)) WUP34) (4 (4 (80 0)) WUP44) (4 (5 (100 0)) WUP54)) ((5 (1 (20 0)) WUP15) (5 (2 (40 0)) WUP25) (5 (3 (60 0)) WUP35) (5 (4 (80 0)) WUP45) (5 (5 (100 0)) WUP55))) (((1 (1 (20 0)) WUP11) (1 (2 (40 0)) WUP21) (1 (3 (60 0)) WUP31) (1 (4 (80 0)) WUP41) (1 (5 (100 0)) WUP51)) ((2 (1 (20 0)) WUP12) (2 (2 (40 0)) WUP22) (2 (3 (60 0)) WUP32) (2 (4 (80 0)) WUP42) (2 (5 (100 0)) WUP52)) ((3 (1 (20 0)) WUP13) (3 (2 (40 0)) WUP23) (3 (3 (60 0)) WUP33) (3 (4 (80 0)) WUP43) (3 (5 (100 0)) WUP53)) ((4 (1 (20 0)) WUP14) (4 (2 (40 0)) WUP24) (4 (3 (60 0)) WUP34) (4 (4 (80 0)) WUP44) (4 (5 (100 0)) WUP54)) ((5 (1 (20 0)) WUP15) (5 (2 (40 0)) WUP25) (5 (3 (60 0)) WUP35) (5 (4 (80 0)) WUP45) (5 (5 (100 0)) WUP55))) (((1 (1 (20 0)) WDN11) ETC WDN..... ((1 (20 0) "Y-OUTPUT1") (2 (40 0) "Y-OUTPUT2") (3 (60 0) "Y-OUTPUT3")))
;;((INPUT1 INPUT2 INPUT3 INPUT4 INPUT5) (X-ACTIVITY1 X-ACTIVITY2 X-ACTIVITY3 X-ACTIVITY4 X-ACTIVITY5) (WUP11 WUP21 WUP31 WUP41 WUP51 WUP12 WUP22 WUP32 WUP42 WUP52 WUP13 WUP23 WUP33 WUP43 WUP53 WUP14 WUP24 WUP34 WUP44 WUP54 WUP15 WUP25 WUP35 WUP45 WUP55) (WUP11 WUP21 WUP31 WUP41 WUP51 WUP12 WUP22 WUP32 WUP42 WUP52 WUP13 WUP23 WUP33 WUP43 WUP53 WUP14 WUP24 WUP34 WUP44 WUP54 WUP15 WUP25 WUP35 WUP45 WUP55) (WDN11 WDN21 WDN31 WDN41 WDN51 WDN12 WDN22 WDN32 WDN42 WDN52 WDN13 WDN23 WDN33 WDN43 WDN53 WDN14 WDN24 WDN34 WDN44 WDN54 WDN15 WDN25 WDN35 WDN45 WDN55) (WDN11 WDN21 WDN31 WDN41 WDN51 WDN12 WDN22 WDN32 WDN42 WDN52 WDN13 WDN23 WDN33 WDN43 WDN53 WDN14 WDN24 WDN34 WDN44 WDN54 WDN15 WDN25 WDN35 WDN45 WDN55) (Y-OUTPUT1 Y-OUTPUT2 Y-OUTPUT3))




;;CONVERT-ART2-ARRAY-TO-LIST
;;
;;ddd
(defun convert-ART2-array-values-to-list (array 
                                          &key (symbol-root 'cell) (initial-x 0) (incr-x 40)
                                          (from-field-n 1)(from-cells :all)
                                          (to-field-n  2) (to-cells :all)
                                          (from-index0 1)(to-index0 1)
                                          add-label-p)
  "In ART2.lisp, converts array-dim (if an integer) or all dims of 1 array FROM 1 field to to-field-n-list  into 1 graph-data-list-structure. array-dim = array-dim to convert symbol-root= symbol for list and root for sublists.  NOTE:  initial-x and incr-x can be a value OR a list of values--single value used for all, list values must match num cells. Takes N dim arrays RETURNS (values  converted-arrays-lists new-symbols-list)  CONVERTED-ARRAYS-LISTS eg. (PUT EG HERE SSS  )."
  (let*
      ((array-dims (array-dimensions array))
       (num-dims (length array-dims ))
       (num-from-field-cells (car (last array-dims)))   
       (num-to-field-cells  (car array-dims))
       (to-field-cell-nums)
       (array-value)
       (item-list)
       (dim-item-list)
       (x-val initial-x)
       (converted-arrays-list)
       (symbol-list)
       (cell-i)
       (cell-j)  
       (cell-array-i-diff  from-index0)  ;;dif between cell-i and array-index-i (eg 1  0) = 1
       (cell-array-j-diff  to-index0)  ;;dif between cell-i and array-index-i (eg 1  0) = 1
       (array-i)
       (array-j)
       )
    ;;SET FROM AND TO CELL NUMBERS LISTS
    (cond
     ((equal from-cells :all)
      (setf from-field-cell-nums (my-make-list num-from-field-cells 
                                               :initial-element from-index0
                                               :delta-element-function  '+ :fun-rest-args '(1) )))
     (t (setf from-field-cell-nums from-cells)))
    (cond
     ((equal to-cells :all)
      (setf to-field-cell-nums (my-make-list num-to-field-cells
                                             :initial-element to-index0
                                             :delta-element-function  '+ :fun-rest-args '(1) )))
     (t (setf to-field-cell-nums  to-cells)))

    ;;IS INDEX TYPE SINGLE-INDEX OR DOUBLE-INDEX
    (cond
     ;;single index
     ((= num-dims 1)
      (loop
       for cell-i  in  from-field-cell-nums
       ;;causes termination to fast for cell-j in  to-field-cell-nums
       do
       (setf  array-i (- cell-i cell-array-i-diff)
        value (aref array array-i))
       (if (numberp value)
           (setf symbol  symbol-root))
       ;;append the graph converted-arrays-list (if a 2D array, add to flattened--not nested--list
       (setf item-list `(,cell-i (,x-val  ,value)))
       (if add-label-p 
           (setf item-list (append item-list (list (format nil "~A~A" symbol-root cell-i)))))
       (setf converted-arrays-list (append converted-arrays-list (list item-list)))
       (if (numberp value)
            (setf symbol (my-make-symbol (format nil "~A~A" symbol-root cell-i))
                  symbol-list (append symbol-list (list symbol))))
       (set symbol item-list)
       (setf x-val (+ x-val incr-x))
       ;;end loop
       )
      ;;end num-dims 1
      )
     ;;DOUBLE INDEX
     (t 
      (loop
       for cell-j in  to-field-cell-nums
       do
       (loop
        for cell-i  in  from-field-cell-nums
        do
        (setf  array-i (- cell-i cell-array-i-diff)
               array-j (- cell-j cell-array-j-diff)
               value (aref  array array-j array-i))
        (if (numberp value)
            (setf symbol (my-make-symbol (format nil "~A~A~A" symbol-root cell-i cell-j))
                  symbol-list (append symbol-list (list symbol ))))

        ;;make the item-list for each cell/var
        (setf item-list `(,cell-j (,cell-i (,x-val  ,value))))
        (if add-label-p 
            (setf  item-list (append item-list (list symbol))))
        (setf dim-item-list (append dim-item-list (list item-list)))        
        (set symbol item-list)
        (setf x-val (+ x-val incr-x))
        ;;end inner-loop
        )
       (setf converted-arrays-list (append converted-arrays-list (list dim-item-list))
             dim-item-list nil
             x-val initial-x)

       ;;end outer-loop, t, cond
       )))
    (values converted-arrays-list symbol-list) 
    ;;end let, convert-ART2-array-values-to-list
    ))
;;TEST 
(defun testca1 ()
  (let*
     ((array1 (make-array '(8) :initial-contents '(0 1 2 3 4 5 6 7)))
      (result)
      (num-dims (length (array-dimensions array1))) ;;= (3 4 2)
      (all-values-list)
      (symbol 'input)
      (add-label-p T)
      )
  (afout 'out (format nil "array1= ~A~% num-dims= ~A "  array1 num-dims))
  (multiple-value-bind (converted-arrays-list symbol)
      (convert-ART2-array-values-to-list  array1 3 :symbol-root symbol :add-label-p add-label-p)   
  (values converted-arrays-list symbol))))
;;THE TEST
;; (testca1)  RESULTS=
;;((1 (0 0)) (2 (40 1)) (3 (80 2)) (4 (120 3)) (5 (160 4)) (6 (200 5)) (7 (240 6)) (8 (280 7)))
;;(CELL1 CELL2 CELL3 CELL4 CELL5 CELL6 CELL7 CELL8)
;;  CELL4 = (4 (120 3))
;;  IF :add-label-p T, RESULT= ((1 (0 0) "INPUT1") (2 (40 1) "INPUT2") (3 (80 2) "INPUT3") (4 (120 3) "INPUT4") (5 (160 4) "INPUT5") (6 (200 5) "INPUT6") (7 (240 6) "INPUT7") (8 (280 7) "INPUT8"))
;;(INPUT1 INPUT2 INPUT3 INPUT4 INPUT5 INPUT6 INPUT7 INPUT8)
;;
;;TEST FOR DOUBLE-INDEX VARIABLES/CELLS
(defun testca2 ()
  (let*
     ((array1 (make-array '(3 8) :initial-contents '((0 .1 .2 .3 .4 .5 .6 .7)(0 .21 .22 .23 .24 .25 .26 .27 )(0 .31 .32 .33 .34 .35 .36 .37))))
      (result)
      (num-dims (length (array-dimensions array1))) ;;= (3 4 2)
      (all-values-list)
      (symbol 'wup)
      (add-label-p T)
      )
  (afout 'out (format nil "array1= ~A~% num-dims= ~A "  array1 num-dims))
  (multiple-value-bind (converted-arrays-list symbol-list)
      (convert-ART2-array-values-to-list  array1  :symbol-root symbol :add-label-p add-label-p)   
    ;;(pprint converted-arrays-list)
  (values converted-arrays-list  symbol-list))))
;;TEST RESULTS
;; (testca2)
;;RESULTS = (((1 (1 (0 0)) WUP11) (1 (2 (40 0.1)) WUP21) (1 (3 (80 0.2)) WUP31) (1 (4 (120 0.3)) WUP41) (1 (5 (160 0.4)) WUP51) (1 (6 (200 0.5)) WUP61) (1 (7 (240 0.6)) WUP71) (1 (8 (280 0.7)) WUP81)) ((2 (1 (0 0)) WUP12) (2 (2 (40 0.21)) WUP22) (2 (3 (80 0.22)) WUP32) (2 (4 (120 0.23)) WUP42) (2 (5 (160 0.24)) WUP52) (2 (6 (200 0.25)) WUP62) (2 (7 (240 0.26)) WUP72) (2 (8 (280 0.27)) WUP82)) ((3 (1 (0 0)) WUP13) (3 (2 (40 0.31)) WUP23) (3 (3 (80 0.32)) WUP33) (3 (4 (120 0.33)) WUP43) (3 (5 (160 0.34)) WUP53) (3 (6 (200 0.35)) WUP63) (3 (7 (240 0.36)) WUP73) (3 (8 (280 0.37)) WUP83)))
;;(WUP11 WUP21 WUP31 WUP41 WUP51 WUP61 WUP71 WUP81 WUP12 WUP22 WUP32 WUP42 WUP52 WUP62 WUP72 WUP82 WUP13 WUP23 WUP33 WUP43 WUP53 WUP63 WUP73 WUP83)













;; END -------------- POST-RUNART DATA-ANALYSIS FUNCTIONS ------------------



;;TTT --------------------------------- HELP TESTING AREA -------------------------------
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
#|(defun ART2 (&key (plot? nil))   
  (setf *learned-categories* nil)
  ;;;  (if plot?
  ;;;      (init-plot "Adaptive Resonance Theory" 250 290))
  (initNetwork)
  (learnPatterns *nInputs *nOutputs *testPatterns *learnPatterns-cycles)
)|#

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
;;end --------------------------------- HELP TESTING AREA -------------------------------



;;************* THIS WORKS WHEN USE CELL SUBTYPE ARRAYS **********
;; RECONCILE WITH NEWER VERSION ABOVE THAT OPERATES ON
;;   ONLY CELL TYPE ARRAYS
;;


;;END OF WORKING FUNCTION ----------------------



;;--------------  OLD  DELETE?? ------------------------

;;SSS  FIX DATA-ANAL BASED UPON BELOW (CREATED IN INIT)
;; 3. ALL VARIABLES LIST (If variable = a cell (celln fieldn) only item in spec list)
;; 1-SINGLE-CELL/INDEX ARRAYS (1D array = celln or valueN)
;; (eg= (("X-Activity" ((0 1)) "" "") ("X-Activity" ((1 1)) "" "") ("X-Activity" ((2 1)) "" "") ("X-Activity" ((3 1)) "" "") ("X-Activity" ((4 1)) "" ""))
;;  (if variable is a weight, etc involving 2 or more cells, then a list  FROM-CELL = (celln fieldn)  TO-CELL (celln fieldn) 
;; 2-FROM-TO-CELL/DOUBLE-INDEX ARRAYS (Multi-DIM arrays:
;;    Number of  FROM-CELLS= N of first dimension. 
;;  First index=Dim1=FROM-cell; Second index=Dim 2toN= TO-cell
;;  Number of DIMS= Number of cells in TO-FIELD
;;  (eg.  (make-array '(8 8 8 8 8)) => Num FROM-cells = 8; Num TO-cells = 5)
;; (eg1= (("Wup" ((0 1) (0 0)) "" "") ("Wup" ((1 1) (0 0)) "" "") ("Wup" ((2 1) (0 0)) "" "") ("Wup" ((3 1) (0 0)) "" "") ("Wup" ((4 1) (0 0)) "" ""))
;; (eg2 (("Wdn" ((0 1) (1 0)) "" "") ("Wdn" ((1 1) (1 0)) "" "") ("Wdn" ((2 1) (1 0)) "" "") ("Wdn" ((3 1) (1 0)) "" "") ("Wdn" ((4 1) (1 0)) "" "")) 
