;;****************************** ART2.lisp **************************
;;
;;MAIN FUNCTIONS for ART2 
;;
 ;        Adaptive Resonance 2 Model in Common Lisp.
 ; 
 ;;      Copyright 1990 by Mark Watson.
 ;        Based on the Adaptive Resonance Theory
 ;        by Gail Carpenter and Stephen Grossberg.
;; Modified by Tom G. Stevens PhD
;;
;;SSS START HERE
;;TO RUN ART2 --THIS REPLACED MyART2.lisp
;; 1- ADJUST PARAMETERS IN RUNART, INIT-ART2, AND ART2-MANAGER ==>>  GO TO config-ART2.lisp
;; 2-(RUNART)

;; *load-art2-files-p
;;  
(defparameter *load-art2-files-p T "Only used when loading config-art2.lisp. May be reset in RUNART.")

;;RUNART
;;
;;ddd
(defun runart (&optional load-config-p &key (load-art2-files-p T))
  "In ART2.lisp, Main test function. Set some init variables here--esp graphing. Then runs ART2-manager. Also contains some key run parameters to set."
  ;;LOAD ART2 CONFIG??
  (setf *load-art2-files-p load-art2-files-p)
  (if  load-config-p
      (load "C:\\TOM\\LISP PROJECTS TS\\ART-LW\\config-ART2.lisp"))
  (unless (find-symbol "ART2-MULTIPANE-INTERFACE" 'common-lisp-user)
       (load "C:\\TOM\\LISP PROJECTS TS\\ART-LW\\config-ART2.lisp"))

  (declare (special out *overall-cycle-n *display-graphs-on-reset *graph-every-x-cycle-list))
  
  (setf out nil)
  (setf *overall-cycle-n 1
        *reset-x-gap 10)
  ;;define the test-patterns
  (define-current-test-patterns)

  ;;SET THE MAIN VARIABLES HERE AND IN INIT-ART2
  (let*
      ((num-cycles  *num-cycles)
       (ninputs  *n-inputs)
       (noutputs *n-outputs)
       (test-pattern-lists *current-test-pattern-list) ;;testPatterns15
       (graphing-symbols-list)  ;;find it below after set
       (graphing-array-sym-list  '(*input  *x-activity  *wUp *wdn *y-output *reset-val))
       (graphing-array-list) ;;find below after set was  `(,*input  ,*x-activity  ,*wUp ,*wdn ,*y-output))
       ;;find the graphing-symbols-list
       (graphing-symbols-list '(input x-activity wUp wDn y-output reset-val)) ;;find below after set `(,input ,x-activity  ,wUp ,wdn ,y-output))
       (graph-every-x-cycle *graph-every-x-cycle)
       (graph-last-cycle  T)
       (initial-x 40)
       (incr-x 40)
       )
     ;;for graphing
      (setf *display-graphs-on-reset t
             ;;determines times graph pops up
            *graph-every-x-cycle-list '(1   5  10)) 
        ;;was    *graph-every-x-cycle-list '(1 2 3 4 5 6 7 8 9 10)) 

    ;;RUN ART2-MANAGER--the main exec function
    (ART2-manager num-cycles ninputs noutputs test-pattern-lists 
                  graphing-array-sym-list  graphing-symbols-list 
                  graph-every-x-cycle graph-last-cycle initial-x incr-x)

    (afout 'out (format nil "AT END OF RUNART, *converted-arrays-list-of-lists= ~A~%" *converted-arrays-list-of-lists))
    (fout out)

    ;;RUN THE TEST CYCLE
#|        (ART2-manager *num-test-cycles ninputs noutputs  test-pattern-lists 
                  graphing-array-sym-list  graphing-symbols-list 
                  *graph-every-test-cycle  *graph-last-test-cycle initial-x incr-x)|#
  ;;   (afout 'out (format nil "AT END OF TEST tRUNART, *converted-arrays-list-of-lists= ~A~%" *converted-arrays-list-of-lists))
  ;;   (fout out)   

   (values *converted-arrays-list-of-lists  *converted-arrays-lists)
    ))


;;ART2-MANAGER
;;mmm
;;ddd  
(defun ART2-manager (num-cycles ninputs noutputs test-pattern-lists 
                                graphing-array-sym-list  graphing-symbols-list 
                                graph-every-x-cycle  graph-last-cycle  initial-x incr-x )
  "In ART2.lisp, manages full ART2 execution, initializes, calculates, simple data analysis, and graphing. Set run and model parameters elsewhere."
  (declare (special *learned-categories*  *converted-arrays-lists *converted-arrays-list-of-lists *new-symbols-list *my-reset-broadcast *display-graphs-on-reset))
  (setf *learned-categories* nil *converted-arrays-lists nil *converted-arrays-list-of-lists nil *new-symbols-list nil *my-reset-broadcast nil)
  (let
      ((graphing-array-list)
       (new-symbol-type-list)
       (new-symbols-type-list-of-lists)
       (array-sym-types-list)
       (new-symbol-type-spec-list-of-lists)
       (new-symbol-type-symbol-string-list-of-lists)
       (array-symbol-list-of-lists all-arrays-list)
       (interface-title)
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
    ;;is this done elsewhere too??
    ( initNetwork nInputs nOutputs)

    ;;STEP 4: RUN ART2 -- CALCULATE THE VALUES FOR EACH CYCLE 
    ;; ON EACH FIELD AND CELL    
    (setf *learned-patterns
          (learnPatterns nInputs nOutputs test-pattern-lists num-Cycles 
                         graph-every-x-cycle  initial-x incr-x))
    ;;Keep track of cycle num (can't find another--is this redundant????)
    (incf  *overall-cycle-n)

    ;;OLD VERSION USING MY GRAPHING -- WORKED
    ;;RUN ART2
   ;; (ART2) replaced by initNetwork plus setf *learned-patterns ... above

   ;;STEP 5: CONVERT ART2 ARRAY VALUES TO X, Y COORDS IN LISTS
   ;; and  STEP 6:  GRAPH THE TERMINAL CELL VALUES 
   ;;if only print graphs for last cycle  
   (if graph-last-cycle
       (progn
         ;;now make the graphing  list of lists
         (setf graphing-array-sym-list '(*input  *x-activity  *wUp *wdn *y-output *reset)
               graphing-array-list `(,*input  ,*x-activity  ,*wUp ,*wdn ,*y-output ,*reset-val))
         ;;find the graphing-symbols-list
         (setf  graphing-symbols-list `(,input ,x-activity  ,wUp ,wdn ,y-output ,reset))
        
         (afout 'out (format nil "In ART2-manager, graphing-array-list= ~A~%graphing-symbols-list= ~% "  graphing-array-list graphing-symbols-list ))

         ;;create the new lists
         (multiple-value-setq (*converted-arrays-lists *new-symbols-list)
             (convert-ART2-arrays-to-lists  graphing-array-sym-list  graphing-symbols-list
                                            initial-x incr-x))
         (setf *converted-arrays-list-of-lists (append *converted-arrays-list-of-lists 
                                                       (list *converted-arrays-lists)))

         ;;create the interfaces and graphs
         (graph-ART2 " ART2 LAST Cycle" )
         ))
;;convert-ART2-state (array-list symbol-list  initial-x incr-x)
   ;;end ART2-manager
   (values *converted-arrays-list-of-lists  *converted-arrays-lists)
    ))


;;TESTING ART2 with (runart) --------------------------------------------
;;  ((num-cycles 5) (ninputs 5)(noutputs 3) (test-pattern-lists testPatterns6) returns(((0 (40 0.0051200018)) (1 (80 0.22896)) (2 (120 0.88416)) (3 (160 0.39984)) (4 (200 -0.007360004))) ((0 (40 2.1019472E-4)) (1 (80 0.009399642)) (2 (120 0.03629799)) (3 (160 0.016414889)) (4 (200 -3.0215495E-4))) ((0 (40 0.0933)) (1 (80 0.0579)) (2 (120 0.0521)) (3 (160 0.068)) (4 (200 0.0961))) ((0 (40 0.09065001)) (1 (80 0.09895)) (2 (120 0.0658)) (3 (160 0.0694)) (4 (200 0.07865))) ((0 (40 0.01506)) (1 (80 0.011299999)) (2 (120 0.01677424)) (3 (160 0)) (4 (200 0))) ((0 (40 0.0155)) (1 (80 0.01418)) (2 (120 0.029842116)) (3 (160 0)) (4 (200 0))) ((0 (40 0.0)) (1 (80 0.0)) (2 (120 0.0))))



;;xxx --------------------------------- ART2 CALCULATION FUNCTIONS --------------



;; XXX ---------------- LEARN PATTERNS MANAGER FUNCTION ---------------

;;LEARNPATTERNS
;;
;;ddd
(defun learnPatterns (nInputs nOutputs testPatterns num-Cycles
                              graph-every-x-cycle  initial-x incr-x 
                              &key (run-random-test-p  NIL) ;;problems with T
                              ignore-pattern-num-cycles-p)
                            ; Watson uses num = 50 when calls in original (ART2) was this orig args? (i j) 
  "In ART2.lisp, Main ART2 Calculation Manager that cycles through all training patterns"
  (afout 'out (format nil "In learnPatterns, testPatterns= ~a ~%" testPatterns))

  (setq  learningcyclecounter 0);;2013-11 added global var here

  (let
      ((random-testpatterns)
       (pattern-num-cycles)
       (p)
       )

    (loop
     for p-list in testPatterns
     do
     (cond
      (ignore-pattern-num-cycles-p NIL)
      ((listp (car p-list))
       (setf pattern-num-cycles 0
             p ( car p-list)))
      (t (setf pattern-num-cycles (car p-list)
               num-Cycles pattern-num-cycles  ;;REPLACES THE DEFAULT numCycles
               p (second p-list))))

     (afout 'out (format nil "Inputs= ~A~%" p))
     (setPattern p nInputs nOutputs)
     (dotimes (i num-Cycles)
       (setq learningCycleCounter (1+ learningCycleCounter))
       (OneCycle ninputs noutputs)  ;; temp quoted out i j bec of error
       ;;(updateScreen))
       (setq *learned-categories*
             ;;SSS  START HERE what is p? it is an &aux var in findLargestOutput
             ;;  what is happening, how to change to my normal coding 
             ;;NOTE: In Watson, p is a vector
             ;; pi = ui + SUMj g(yj) * wDownji (in 7.3)
             (cons (list p (findLargestOutput nOutputs))
                   *learned-categories*))
       ;;sss
       ;;was-- do I need to copy to lists??   (copy-data-to-cumulative-arrays *cum-array-list)
       (cond
        ((or (and (my-floor graph-every-x-cycle :floor 1)
                  (member  (/ i graph-every-x-cycle) *graph-every-x-cycle-list :test  '=))
             ;;added next 2013-11-29
             (and  *my-reset-broadcast *display-graphs-on-reset))
         ;;reset it to nil after this is triggered
         (setf *my-reset-broadcast nil)
      
         ;;now make the graphing  list of lists
         (setf graphing-array-sym-list '(*input  *x-activity  *wUp *wdn  *y-output *reset)
               graphing-array-list `(,*input  ,*x-activity  ,*wUp ,*wdn ,*y-output ,*reset))
         ;;find the graphing-symbols-list
         (setf  graphing-symbols-list `(,input ,x-activity  ,wUp ,wdn ,y-output  ,reset))

         (afout 'out (format nil "In ART2-manager, graphing-array-list= ~A~%graphing-symbols-list= ~%graphing-symbols-list= ~A "  graphing-array-list graphing-symbols-list ))

         ;;create the new lists
         (multiple-value-setq (*converted-arrays-lists *new-symbols-list)
             (convert-ART2-arrays-to-lists  graphing-array-sym-list  graphing-symbols-list
                                            initial-x incr-x))
         ;;accumulate the lists
         (setf *converted-arrays-list-of-lists (append   *converted-arrays-list-of-lists
                                                         (list *converted-arrays-lists)))
         (afout 'out (format nil "IN LEARNPATTERNS, *converted-arrays-lists= ~a~%" *converted-arrays-lists))
         ;;create the interfaces and graphs 
         (setf  interface-title (format nil "ART2 Cycle Number= ~A Reset= ~A" i *my-reset-broadcast))
         (graph-ART2 interface-title)
         (sleep 3)
         )
        (t nil))

       ;;end dotimes, loop
       ))
    (afout 'out (format nil "At END of learnPatterns, *learned-categories* = ~s ~%"
                        *learned-categories*))
    (when  run-random-test-p
      (setf random-testpatterns (my-randomize-list testpatterns))
      (learnPatterns ninputs noutputs random-testpatterns 1 1 initial-x incr-x
                     :run-random-test-p nil :ignore-pattern-num-cycles-p T)
      (afout 'out (format nil "At END of RANDOM TEST CYCLES of learnPatterns, *learned-categories* = ~s ~%" *learned-categories*))
      ;;end when
      )
    ;;end let LearnPatterns
    ))
 
;;xxx -------------------------------------- SUB-CALCULATION FUNCTIONS --------------------------

;;FRANDOM
; Floating point  random numbers:
;;ddd
(defun frandom (low high)
  (let ((range (- high low)))
    (+ (* (/ (random 1000) 1000.0) range) low)))

;;findLargestOutput
;;ddd
(defun findLargestOutput (nOutputs) ;;was &aux (maxIndex 0) (maxVal (aref *y-output 0)))
  "In ART2, returns (values maxIndex maxVal) "
  (let*
      ((maxIndex 0)
       (maxVal (aref *y-output 0))
       )
  (dotimes (j nOutputs)
    (if (and
         (> (aref *y-output j) maxVal)
         (not (aref *reset j)))
      (setq maxVal (aref *y-output j)
          maxIndex j)))
  (if (= *print-detail 2) (afout 'out (format nil "maxIndex= ~a ~%" maxIndex)))
  (values maxIndex maxVal)
  ))
;;TEST
;; (findLargestOutput 4) = 0 (when *y-output= #(0.4372523 0.0 0.0 0.0 0.0); note that &aux sets mVal to 0.437 which is greater than any other value, but is not returned.
; The following function returns d if (aref *y-output index) is the
; largest value in array *y-output AND (aref *y-output index) has not been reset:

;;G
;;
;;ddd
(defun g (index nOutputs) ;;was  &aux j mVal)   ;; (maxIndex (findLargestOutput)))
  (let*
      ((maxIndex (findLargestOutput nOutputs))
     ;;  (maxVal)
     ;;  (j)
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


;;SIGMOID
; Threshold function:
;;ddd
(defun sigmoid ( x-activity)
  "In ART2.lisp, threshold function."
  (if (> x-activity theta)
    x-activity
    0.0))

;;L2NORM
; L2 Norm of a vector:
;;ddd
(defun L2NORM (v vLength) ;; &aux (sum 0.0) i j)
  "In ART2.lisp, Norm of a vector"
    (let*
        ((l2norm 0)
         (sum 0.0)
         (i 0)
         (j 0)
         (*vi-val (aref *v i))
         )
  (dotimes (i vLength)
    (setq sum (+ sum (* *vi-val *vi-val)))
    ;;end let, dotimes
    )
  (setf l2norm (+ (sqrt sum) 0.001))
  ;;end let, L2NORM
  ))
;;TEST
;;   (L2NORM  *input 9) = 61.491974  (where *input = #(-0.079519995 -0.007360004 -0.04432 1.00784 1.02688 1.0753601 -0.016960003 -0.05472 0.058239997))

;;F1STMcycle
; Update F1 STM arrays:
;;
(defun F1STMcycle (nInputs nOutputs &aux i j sum norm max1 max2)
  "In ART2.lisp, Updates F1 STM arrays"
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
    ;;end dotimes
    )
  ; Update q using eq. 5
  (setq norm (my-floor (+ (L2NORM p nInputs) e) :floor .0001))
  (dotimes (i nInputs)
    (setf (aref *q i) (/ (aref *p i) norm))
    ;;end dotimes
    )
  ; Update u using eq. 6:
  (setq norm (L2NORM v nInputs))
  (dotimes (i nInputs)
      ;;(break "dotimes (i nInputs) in F1STMcycle")
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *u i)= ~s; norm= ~s ~%" (aref *u i) norm)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *v i)= ~s; norm= ~s ~%" (aref *v i) norm)))
    (setf (aref *u i) (/ (aref *v i) norm))
    ;;end dotimes
    )
  ; Update v using eq. 7:
  (dotimes (i nInputs)
    (setf (aref *v i) (sigmoid (+ (aref *x-activity i) (* b (sigmoid (aref *q i))))))
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *x-activity i)= ~s; b= ~s ~%" (aref *x-activity i) b)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *q i)= ~s; b= ~s ~%" (aref *q i) b)))
    ;;end dotimes
    )
  ; Update w using eq. 8:
  (dotimes (i nInputs)
    (setf (aref *w i) (* alpha (+ (aref *input i) (* a (sigmoid (aref *u i))))))
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *w i)= ~s; alpha= ~s ~%" (aref *w i) alpha)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *u i)= ~s; alpha= ~s ~%" (aref *u i) alpha)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *input i)= ~s; ~%" (aref *input i) )))
    ;;end dotimes
    )
  ; Update x-activity using eq. 9:
  (setq norm (my-floor (+ (L2NORM w nInputs) e) :floor .0001))
  (dotimes (i nInputs)
    (setf (aref *x-activity i) (/ (aref *w i) norm ))
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *x-activity i)= ~s; norm= ~s ~%" (aref *x-activity i) norm)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *u i)= ~s; alpha= ~s ~%" (aref *w i) norm)))
    ;;end dotimes
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
    ;;end dotimes
    )
  (setq max1 (my-floor (+ max1 0.001) :floor .00001))
  (setq max2 (my-floor (+ max2 0.001) :floor .00001))
  (dotimes (i nInputs)
    (setf (aref *r i) (my-floor (- (/ (aref *u i) max1)  (/ (aref *p i) max2))))  
    (if (= *print-detail 2) (afout 'out (format nil  "FOR i= ~s ~%" i)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *u i)= ~s; max1= ~s ~%" (aref *u i) max1)))
    (if (= *print-detail 2) (afout 'out (format nil  "(aref *p i)= ~s; max2= ~s ~%" (aref *p i) max2)))
    (if (= *print-detail 2) (afout 'out (format nil  "r=~a i=~a; value= ~a ~%" r i (aref *r i))))
    ;;end dotimes
    )
  )

;;F2STMcycle
; Update F2 STM storage:
;;ddd
(defun F2STMcycle (nInputs nOutputs &aux i j sum)
  "In ART2.lisp,Updates F2 STM storage"
  (dotimes (j nOutputs)
    (progn
      (setq sum 0.0)
      (dotimes (i nInputs)
        (setq sum (+ sum (* (aref *p i) (aref *wUp i j)))))
      (setf (aref *y-output j) sum)
      (if (aref *reset j) (setf (aref *y-output j) -0.1))
       ;;me
       (if (> *print-detail 1) (afout 'out (format nil  "y=~a j=~a; value= ~a ~%" *y-output j (aref *y-output j))))
      (aref *y-output j)      
      )    
    )  
  )


;;UPDATEWEIGHTS
;;
;;ddd
(defun updateWeights (nInputs nOutputs  &aux i) ;; (j (findLargestOutput)))
    "In ART2.lisp, Updates weights for LTM model."
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
           d (my-floor (- (aref *p i) (aref *wdn j i)) :floor 0.00001    ))))
       ;;me
     (if (> *print-detail 1)  (afout 'out (format nil  "j=~a i=~a; value= ~a ~%" j i (aref *wdn j i))))
      (setf 
         (aref *wUp i j)
       (+
        (aref *wUp i j)
        (*
         upLR
         d  (my-floor (- (aref *p i) (aref *wUp i j))   :floor 0.00001 ))))
       ;;me
       (if (> *print-detail 1)(afout 'out (format nil  "i=~a j=~a; value= ~a ~%" i j (aref *wUp i j)))   
      )))))

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


;;COMPETITIVEF2
; Competitive learning at slab F2:
;;ddd
(defun competitiveF2 (nOutputs &aux i) ;; (jj (findLargestOutput)))
  "In ART2.lisp, Calculates competitive learning at slab F2"
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

;;ONECYCLE
;;
;;ddd
(defun OneCycle (nInputs nOutputs &aux i j)
  "In ART2.lisp,  Runs one full ART2 cycle"
  (F1STMcycle nInputs nOutputs) ;;xxx ;;i j) ;;me added i j bec of error
  (testReset nInputs nOutputs)
  (competitiveF2  nOutputs)
  (F2STMcycle nInputs nOutputs)
  (updateWeights nInputs nOutputs)
  (competitiveF2 nOutputs)
  ;; (updateScreen)
  )



;;SSS START HERE -- NEED TO UNDERSTAND RESET BETTER, STUDY
;;
;;TESTRESET
; Check for an F2 reset condition:
(setq skipReset nil)
;;ddd
(defun testReset (nInputs nOutputs)  ;;was, replace in let* &aux (res 0.0) (norm1 0.0) (norm2 0.0)  (n1 (+ (L2NORM p nInputs) e)) n2 temp) uses &aux instead of let??
  "In ART2.lisp, Used in OneCycle.  Checks for an F2 reset condition. Tracks reset values by appending *reset-vals  *reset-values-list. [default e= 0.04? n2=nil temp=eg.(TEMP0 TEMP1 TEMP2 TEMP3 TEMP4 TEMP5 TEMP6 TEMP7 TEMP8)"
;;Steele-Nothing can bedone with &aux variables that cannot be done with the special form let*:
  (let*
      ((res 0.0)
       (norm1 0.0)
       (norm2 0.0)
       (l2norm-value (L2NORM p nInputs))  ;;eg p=(P0 P1 P2 P3 P4)
       (n1 (+ l2norm-value e))
       (n2)
       (temp)
       (was-reset)
       (reset-x-coord)       
       )
    ;;   (afout 'out (format nil "In testReset,learningCycleCounter= ~A~%   n1= ~A  res= ~A~%" learningCycleCounter n1 res))
    (cond
     ((and (> n1  0.2)
           (not skipReset))
      (cond
       ((> learningCycleCounter 1)
        (setf  l2norm-value (L2NORM r nInputs))
        (when (> (aref  *y-output (findLargestOutput nOutputs)) *reset-y-criteria ) ;;;was 0.25)
          (setq res (* 3.0  l2norm-value)
                was-reset t)))  ; was 3.0
       (t nil))
      (setq skipReset nil)
      ;;end and > n1
      )
     (t (setf (aref *reset 0) res)))

    ;;not inside a cond
    (setf reset-x-coord  (* *overall-cycle-n *reset-x-gap)
          *reset-vals (append *reset-vals (list (list *overall-cycle-n (list reset-x-coord  res)))))
    (afout 'out (format nil "In testReset,  learningCycleCounter = ~A n1= ~A   *y-output= ~A l2norm-value= ~A~% res= ~A *reset-val= ~A skipReset= ~A  was-reset= ~A~%" learningCycleCounter n1    learningCycleCounter *y-output l2norm-value res *reset-val skipReset was-reset))

    ;;DOES THIS BROADCAST THE RESET CHANGES?
    ;;(plotActivations "reset flag" 190 220 reset-val 0.0 1.0)
    (cond
     ((> res (- 1.9 vigilance))  ;; me
      (print (list "Vigilance reset =" res "  Learning cycle ="
                   learningCycleCounter))
      (setq maxIndex (findLargestOutput nOutputs))
      ;;added next 2013-11-29 to trigger graph display if appropriate
      (setf *my-reset-broadcast t)
      (setf (aref *reset maxIndex) 1)
      (setf (aref *reset-cntr maxIndex) 80))
     (t
      (dotimes (i nOutputs)
        (setf (aref *reset-cntr i) (- (aref *reset-cntr i) 1))
        (when (< (aref *reset-cntr i) 0)
          (if (= (aref *reset i) 1)  (setq skipReset t))     ;;was if T
          (setf (aref *reset i) 0))))) ;;end cond  ;;was set to nil

    (setq skipReset nil);; temporary
  
    ;;ADD INFO TO *reset-values-list and *reset-vals RRR
    (setf  *reset-values-list (append *reset-values-list (list (format nil "In testReset, n1= ~A  skipReset= ~A  learningCycleCounter = ~A~% *y-output= ~A res= ~A *reset-val= ~A was-reset= ~A~%*reset= ~A~%" n1  skipReset  learningCycleCounter *y-output res *reset-val was-reset *reset)))
           ;;next 2 follow format of other cells
           reset-val0 reset-val
           *reset-val '((reset-val0))
           ;;creates a list of just reset values for making graph of ALL reset values
           ;;no puts in bad vals  *reset-vals (append *reset-vals (list reset-val0))
           )
    ;;end *let, testReset
    ))



; ZEROACTIVATIONS:
;;
;;ddd
(defun zeroActivations (nInputs nOutputs)
  "In ART2.lisp, sets activation levels to zero."
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

;;SETPATTERN
; Set up a learning pattern in the input neurons:
;;ddd
(defun setPattern (pl nInputs nOutputs &aux (len (length pl)))
  "In ART2.lisp, Sets up a learning pattern in the input neurons"
  (if (not (equal len nInputs))
    (print (list "Error in setPattern input:" pl))
    (progn
      (setq learningCycleCounter 0)
      (zeroActivations nInputs nOutputs)
      (dotimes (i len)
        (setf (aref *input i) (+ (pop pl) (frandom -0.08 0.08)))))))


;;XXX ------------------------------- NETWORK INITIALIZATION --------------------
;; 

;;INITNETWORK
; Initialize the network:
;;ddd
(defun initNetwork (nInputs nOutputs)
  "In ART2.lisp, Original ART2 function that initializes the network computational values"
  (zeroActivations nInputs nOutputs)
  (dotimes (j nOutputs)
    (progn
      (dotimes (i nInputs)
        (setf
         (aref *wUp i j) (frandom *wUpInitLo *wUpInitHi)      ;;were 0.05 0.1)  ;;THESE MUST BE SMALL or too manyF2 nodes activated initially
         (aref *wdn j i) (frandom  *wDnInitLo  *wDnInitHi))     ;;were 0.01 0.03))  ;;THESE MUST BE LARGE or no learning
        (afout 'out (format nil "wUp ~a ~a => ~a " i j (aref *wUp i j)))
        (afout 'out (format nil  "wDn ~a ~a => ~a " j i (aref *wdn j i)))        
        )
            (setf (aref *n-cats j) 0)))
  (afout 'out (format nil "*wUp Array= ~a~%*wDn Array= ~a" *wUp *wDn ))
  )


;;
;;ART2-PATTERN-GENERATOR
;; (5((< 5)(>.5)(< .5)(> .5)))
;;mmm11
;;ddd
(defun ART2-pattern-generator (n-inputs pattern-list pattern-type 
                                        &key randomize num-cyclesp)
  "In ART2.lisp, generates patterns for inputs. pattern-list is form of ((num-cycles (pattern)).. ).  pattern-type is either 'equal or 'formula. If 'equal, then patterns identical to the one provided are generated. If 'formula, the either a value or a list (operator value), operators = < > are used. < and > generate random numbers below or above the value. If :num-cyclesp is T, then the :num-cycles is included in each output list. num-cyclesp nil causes generation of separate patterns with no num-cycles in list (use if want to randomize all patterns).  :randomize T causes the final list-of-lists to be randomized."

  (let*
      ((num-patterns (list-length pattern-list))
       (new-pattern-list) ;;overall list of lists
       )
    (loop
     for sublist in pattern-list
     for n from 0 to num-patterns
     with num-cycles
     with pattern ;; can be list of values or (operator value) lists which reduce to a single value
     with new-pattern ;;a list of values
     with label ;;usually the S or s-r label
     do
     (setf num-cycles (car sublist)
           pattern (second sublist)
           label (third sublist))
           
     (afout 'out (format nil "In ART2-pattern-generator, num-cycles= ~A pattern-type= ~A pattern= ~A~%" num-cycles pattern-type pattern))
     (cond
      ;;alike-repeat use only if all values in a pattern are identical to repeats of same pattern
      ((equal pattern-type 'equal)
       (cond
        ((null num-cyclesp)
         (dotimes (cycle-n num-cycles)
           (setf new-pattern-list (append new-pattern-list (list (list pattern label))))
           (afout 'out (format nil "new-pattern-list= ~A~%"  new-pattern-list))))
        (t 
         (setf new-pattern-list (append new-pattern-list (list sublist)))))
       )
      ((equal pattern-type 'formula)
       ;;work on the pattern list elements
       (loop
        for element in pattern
        with operator
        with value
        with new-value
        do
        (cond
         ((listp element)
          (setf operator (car element)
                value (second element))
          (cond
           ((equal operator '=)
            (setf new-value value))
           ((equal operator '<)
            (setf new-value (random value)))
           ((equal operator '>)
            (setf new-value (+ value (random (- 1 value)))))
           (t nil))
          (setf new-pattern (append new-pattern (list new-value)))
          ;;end listp
          )
         (t "Element is not a list"))
        ;;end loop
        )
       ;;make copies of it for num-cycles it is used
       (cond
        ((null num-cyclesp)
         (dotimes (cycle-n num-cycles)
           (setf new-pattern-list (append new-pattern-list (list (list new-pattern label)))))
         )
        (t
         (setf new-pattern-list 
               (append new-pattern-list (list (list  num-cycles new-pattern label))))))
       ;;end pattern = random
       )
     (t nil))
     ;;reset new-pattern
      (setf new-pattern nil)

    ;;randomize new-pattern-list??
    (cond
     ((equal randomize t)
      (setf new-pattern-list (organize-sublists new-pattern-list :randomize t)))
     (t nil))
    ;;end loop
    )
    (afout 'out (format nil "new-pattern-list= ~A" new-pattern-list))
  new-pattern-list
  ))


;;   (testpg)
#|
(defun testpg ()
  (setf out nil)
  (let*
      ((n-inputs 3)
       (pattern-list1 '((3 (1 0 1) "S1")(2(0 1 0) "S2"))) 
       (pattern-type1 'equal)
       ;;works, returns (((1 0 1) "S1") ((1 0 1) "S1") ((1 0 1) "S1") ((0 1 0) "S2") ((0 1 0) "S2"))
       ;;with randomize t returns (((1 0 1) "S1") ((1 0 1) "S1") ((0 1 0) "S2") ((1 0 1) "S1") ((0 1 0) "S2"))
       ;; with num-cyclesp t returns ((3 (1 0 1) "S1") (2 (0 1 0) "S2"))
       (pattern-list2 '((2 ((> .7)(< 0.3)(> 0.6))  "S1")(3 ((= 0.5)(> 0.8)(< 0.6))"S2" )))
       ;; w/or randomize works, returns (((0.92205954 0.16652395 0.84966165) "S1") ((0.92205954 0.16652395 0.84966165) "S1") ((0.5 0.9885243 0.2871696) "S2") ((0.5 0.9885243 0.2871696) "S2") ((0.5 0.9885243 0.2871696) "S2"))
       ;;w randomize t works, returns (((0.796799 0.16920913 0.96542407) "S1") ((0.5 0.97509367 0.1302892) "S2") ((0.796799 0.16920913 0.96542407) "S1") ((0.5 0.97509367 0.1302892) "S2") ((0.5 0.97509367 0.1302892) "S2"))
       ;;with num-cyclesp t works, returns ((2 (0.96480257 0.11676551 0.88210607) "S1") (3 (0.5 0.97711325 0.0067082406) "S2"))
       (pattern-type2 'formula)
       (result)
       )
  ;;  (setf result (ART2-pattern-generator n-inputs pattern-list1 pattern-type1 :randomize t));; :num-cyclesp t))
    ;; works returns  ((1 0 1) (1 0 1) (1 0 1) (0 1 0) (0 1 0))
    (setf result (ART2-pattern-generator n-inputs pattern-list2 pattern-type2 :num-cyclesp t))   ;; :randomize t))
   ;; (afout 'out (format nil "testpg result= ~A~%" result))
    (fout out)
    result
    ))
|#


 #|    (loop 
       for n from 1 to 10
       with x
       with y = '(99 98)
       do
       (setf x  (list 'a n))
       collect n
      ;; append y
       )|#
    






;;xxx --------------------------------------------- END MATERIAL -----------------------------------------
;;











;;pre-testPattern including num-cycles version
#|
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
                *learned-categories*))
    (copy-data-to-cumulative-arrays *cum-array-list)
    
    ;;end dotimes
    )
    (afout 'out (format nil "At END of learnPatterns, *learned-categories* = ~s ~%"
             *learned-categories*))
    )
  )
|#

;;works, but not using this cummulateive array approach--using lists instead
#|(defun initialize-cum-arrays (num-cycles testPatterns)
  "In ART2.lisp, Makes and initializes with 0s the cummulative arrays for adding
  single-cycle array data from each cycle.  The *cum-array-list can be changed to
  include whatever arrays that writer wants to accumulate data from."
  (declare 
   (special *total-num-cycles *cum-array-list *cum-inputs *cum-x-activity *cum-wdn *cum-wup  *cum-y-output))
  ;;the cummulative list
  (setf *total-num-cycles 0
   *cum-array-list  '((*input *cum-inputs)(*x-activity *cum-x-activity)(*wdn *cum-wdn)(*wup *cum-wup)(*y-output *cum-y-output)))


  ;;find the actual total number of cycles by substituting the special num-cycles 
  ;;  within individual patterns for the default num-cycles for all patterns.
  (loop
   for testpattern in testPatterns
   with testpattern-num-cycles = num-cycles
   do
   (if (numberp (car testpattern))
     (setf testpattern-num-cycles (car testpattern)))

     (setf *total-num-cycles (+ *total-num-cycles   testpattern-num-cycles))
     ;;end loop to find *total-num-cycles for all testPatterns
     )

  (loop
   for array-list in *cum-array-list
   for n from 0 to (length *cum-array-list)
   with array-type ;; = (car array-list)
   with dim-list ;;= (array-dimensions array-type)
   with dims ;;= (length dim-list)
   with array

   do
   (setf  array-type (car array-list)
          array (eval array-type)
          dim-list (array-dimensions array)
          dims  (length dim-list))
   (set (second array-list) (make-array  (append (list *total-num-cycles) dim-list) :initial-element 0))
   ;;end loop, defun
   ))|#


#|(defun testica ()
 (setf out nil)
  (initialize-cum-arrays 5 testPatterns6)
)|#
;;works
#|
CL-USER 23 > *cum-x-activity
#2A((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
CL-USER 22 > *cum-wdn
#3A(((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)) ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)) ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)) ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)) ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)))
|#

;;
;;
;;NOT WORKING--NOT USING ARRAYS TO COPY CUM DATA TO
#|
(defun copy-data-to-cumulative-arrays (cycle-num cum-array-list )
  "In ART2.lisp, copies data from arrays in current-cycle-array-list to arrays which include the first
  dimension = cycle-num that accumulate data from ALL ART2 cycles. The number of array dimensions in the current arrays MUST be 3 or less for this to work. "
  (let
      ((current-cycle-array-list)
       )
    (loop
     for cum-array-sublist in cum-array-list
     with current-cycle-array
     with dim-list 
     with dims 
     do
     ;;find the current array symbols and evaluate to get the actual arrays
     (setf current-array-sym (car cum-array-sublist)
           current-cum-array-sym (second cum-array-sublist)
           current-array (eval current-array-sym)
           current-cum-array (eval current-cum-array-sym))
     ;;find the array dimensions
     (setf dim-list  (array-dimensions current-array)
           num-dims  (length dim-list))
     (
     (loop
      for dim in dim-list
      with dim-n
      with current-value
      with cum-value
      do
      (setf current-value (my-aref current-array 
      

#|     (loop 
      for dim in dim-list
      for cell from

   *cum-array-list  
        '((*input *cum-inputs)(*x-activity *cum-x-activity)(*wdn *cum-wdn)(*wup *cum-wup)(*y-output *cum-y-output)))
|#
#|(defun initialize-cum-arrays (num-cycles)
  (declare (special  *cum-inputs *cum-x-activity *cum-wdn *cum-wup
                     *cum-y-output))
  (setf cum-array-list  '((*input *cum-inputs)(*x-activity *cum-x-activity)(*wdn *cum-wdn)(*wup *cum-wup)(*y-output *cum-y-output)))
|#


  )))))))
|#
;;was
#|
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
                *learned-categories*))
    )
    (afout 'out (format nil "At END of learnPatterns, *learned-categories* = ~s ~%"
             *learned-categories*))
    )
  )
|#







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

;;OLD
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



;;TESTING ART2
;;ttt
#|(defun testart ()
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
|#
#|(defun test& ()
  (declare (special xnn))
  (setf xnn 44)
)
(test&)|#

|#


#| ORIGINAL VERSION
(defun testReset (nInputs nOutputs &aux (res 0.0) (norm1 0.0) (norm2 0.0)
                       (n1 (+ (L2NORM p nInputs) e)) n2 temp)
  "In ART2.lisp, Used in OneCycle.  Checks for an F2 reset condition. Tracks reset values by appending *reset-vals  *reset-values-list."
  (let*
      ((was-reset)
       (reset-x-coord)
       )
    (afout 'out (format nil "In testReset,learningCycleCounter= ~A~%   n1= ~A  res= ~A~%" learningCycleCounter n1 res))
    (cond
     ((and (> n1  0.2)
       (not skipReset))
      (cond
       ((> learningCycleCounter 1)
        (if (> (aref  *y-output (findLargestOutput nOutputs)) *reset-y-criteria ) ;;;was 0.25)
            (setq res (* 3.0 (L2NORM r nInputs))
                  was-reset t)))  ; was 3.0
       (setq skipReset nil))
      ;;end and clause
      )
     (t 
      (setf (aref *reset 0) res)
      ;;RRR ???
      (setf reset-x-coord  (* *overall-cycle-n *reset-x-gap)
            *reset-vals (append *reset-vals (list (list *overall-cycle-n (list reset-x-coord  res)))))))
  (afout 'out (format nil "In testReset, n1= ~A  skipReset= ~A  learningCycleCounter = ~A~% *y-output= ~A res= ~A *reset-val= ~A was-reset= ~A~%" n1  skipReset  learningCycleCounter *y-output res *reset-val was-reset))

  ;;DOES THIS BROADCAST THE RESET CHANGES?
  ;;(plotActivations "reset flag" 190 220 reset-val 0.0 1.0)
  (if (> res (- 1.9 vigilance))  ;; me
    (progn
      (print (list "Vigilance reset =" res "  Learning cycle ="
                   learningCycleCounter))
      (setq maxIndex (findLargestOutput nOutputs))
      ;;added next 2013-11-29 to trigger graph display if appropriate
      (setf *my-reset-broadcast t)
      (setf (aref *reset maxIndex) 1)
      (setf (aref *reset-cntr maxIndex) 80))
    (dotimes (i nOutputs)
      (setf (aref *reset-cntr i) (- (aref *reset-cntr i) 1))
      (if (< (aref *reset-cntr i) 0)
        (progn
          (if (aref *reset i)  (setq skipReset t))
          (setf (aref *reset i) nil)))))
  (setq skipReset nil);; temporary
  
  ;;ADD INFO TO *reset-values-list and *reset-vals RRR
  (setf  *reset-values-list (append *reset-values-list (list (format nil "In testReset, n1= ~A  skipReset= ~A  learningCycleCounter = ~A~% *y-output= ~A res= ~A *reset-val= ~A was-reset= ~A~%" n1  skipReset  learningCycleCounter *y-output res *reset-val was-reset)))
         ;;next 2 follow format of other cells
         reset-val0 reset-val
         *reset-val '((reset-val0))
         ;;creates a list of just reset values for making graph of ALL reset values
       ;;no puts in bad vals  *reset-vals (append *reset-vals (list reset-val0))
         )
  ;;end *let, testReset
  ))

;;OLD VERSION
(defun L2NORM (v vLength &aux (sum 0.0) i j)
  "In ART2.lisp, Norm of a vector"
  (dotimes (i vLength)
    (setq sum (+ sum (* (aref *v i) (aref *v i)))))
  (+ (sqrt sum) 0.001))


|#

