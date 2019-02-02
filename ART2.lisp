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
;;SSS START HERE TO RUN ART2
;;
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
      (load "C:\\3-TS\\LISP PROJECTS TS\\ART-LW\\config-ART2.lisp"))
  (unless (find-symbol "ART2-MULTIPANE-INTERFACE" 'common-lisp-user)
       (load "C:\\3-TS\\LISP PROJECTS TS\\ART-LW\\config-ART2.lisp"))

;;done in config  (declare (special out *overall-cycle-n *display-graphs-on-reset *graph-every-x-cycle-list))
  
  (setf out nil 
        reset-info nil
        *out-icon nil
        *all-reset-info-list nil)

  (setf *overall-cycle-n 1
        *reset-x-gap 10)
  ;;define the test-patterns
  (define-current-test-patterns)

  ;;SET THE MAIN VARIABLES HERE AND IN INIT-ART2
  (let*
      ((num-cycles  *num-cycles) ;;10
       (ninputs  *n-inputs)
       (noutputs *n-outputs)
       (test-pattern-lists  *current-test-pattern-list)  ;;= *pattern1
  ;;*pattern1=(((0 0 0 1 0 0 0 1 1) "D") ((0 0 0 1 0 0 0 0 0) "A") ((0 0 0 1 1 1 0 0 0) "X"))
  ;;
       ;;find the graphing-symbols-list
       (graphing-symbols-list *graphing-symbols-list)
       ;; '(input v-activity wUp wDn y-output reset-val)) ;;find below after set `(,input ,v-activity  ,wUp ,wdn ,y-output))
       (graphing-sym-points-list *graphing-sym-points-list)
       ;;'(*input *v-activity *wUp *wDn *y-output  *reset-val))
       (graph-every-x-cycle *graph-every-x-cycle)
       (graph-last-cycle  *graph-last-cycle)
       (end-test-all-patterns-n  *end-test-all-patterns-n)
       (graph-every-x-test-cycle *graph-every-x-test-cycle)
       (graph-end-tests-p *graph-end-tests-p)
       (initial-x-pix *initial-x-pix)
       (incr-x-pix  *incr-x-pix)
       )
     ;;for graphing
      (setf *display-graphs-on-reset t
             ;;determines times graph pops up
            *graph-every-x-cycle-list '(1   5  10)) 
        ;;was    *graph-every-x-cycle-list '(1 2 3 4 5 6 7 8 9 10)) 

    ;;RUN ART2-MANAGER--the main exec function
    (ART2-manager num-cycles ninputs noutputs test-pattern-lists 
                  ;;graphing-array-sym-list 
                  graphing-symbols-list 
                  graph-every-x-cycle graph-last-cycle
                  end-test-all-patterns-n graph-end-tests-p graph-every-x-test-cycle
                  initial-x-pix incr-x-pix)

    (afout 'out (format nil "AT END OF RUNART, *converted-arrays-list-of-lists= ~A" *converted-arrays-list-of-lists))
    ;;(fout out)
    (if (> *print-detail 1)(fout out))

   (values *converted-arrays-list-of-lists  *converted-arrays-lists)
   ;;end let, runart
    ))
;;TEST
;;  (RUNART)


;;ART2-MANAGER
;;mmm
;;ddd  
(defun ART2-manager (num-cycles ninputs noutputs test-pattern-lists 
                                ;;graphing-array-sym-list  
                                graphing-symbols-list 
                                graph-every-x-cycle  graph-last-cycle 
                                end-test-all-patterns-n graph-end-tests-p graph-every-x-test-cycle
                                initial-x-pix incr-x-pix )
  "In ART2.lisp, manages full ART2 execution, initializes, calculates, simple data analysis, and graphing. Set run and model parameters elsewhere."
  (declare (special *learned-categories*  *converted-arrays-lists *converted-arrays-list-of-lists *new-symbols-list *my-reset-broadcast *display-graphs-on-reset))
  (setf *learned-categories* nil *converted-arrays-lists nil *converted-arrays-list-of-lists nil *new-symbols-list nil *my-reset-broadcast nil)
  (setf *art-data-text NIL)
  (let
      ((graphing-array-list)
       (new-symbol-type-list)
       (new-symbols-type-list-of-lists)
       (array-sym-types-list)
       (new-symbol-type-spec-list-of-lists)
       (new-symbol-type-symbol-string-list-of-lists)
       (array-symbol-list-of-lists all-arrays-list)
       (interface-title)
       (date-time (my-get-date-time))
       (converted-symvals-lists)
       (symbols-list)
       (learned-test-patterns)
       )

    ;;STEP 1: INITIALIZE THE ART2 VARIABLES
    (multiple-value-setq (new-symbol-type-list  new-symbols-type-list-of-lists  new-symbol-type-spec-list-of-lists  new-root-list  new-symbol-type-symbol-string-list-of-lists) 
        (my-ART2-init nInputs nOutputs))
    ;;;;was( new-symbol-type-list  new-symbols-type-list-of-lists                                        array-sym-types-list    new-symbol-type-spec-list-of-lists     new-symbol-type-symbol-string-list-of-lists   array-symbol-list-of-lists all-arrays-list) 

    ;;(afout 'out (format nil "array-sym-types-list= ~A" array-sym-types-list))
    (setf *art-data-text (list (format nil ">>>>>>>> NEW ART2 OUTPUT >>>   ~A~%
CONSTANTS:~%a= ~A; b= ~A; c= ~A; d= ~A; e= ~A; theta= ~A; vigilance= ~A *vigilence-multiplier= ~A ~%alpha= ~A  *onCenterThreshold= ~A; *min-input-criteria= ~A; ~%*min-g= ~A;*reset-y-criteria= ~A; *show-last-reset-vals-n= ~A~%Weights: upLR= ~A downLR= ~A; *wUpInitLo= ~A, *wUpInitHi= ~A~%*wDnInitLo= ~A *wDnInitHi= ~A " date-time  a b c d e theta vigilance *vigilence-multiplier  alpha  *onCenterThreshold *min-input-criteria  *min-g *reset-y-criteria *show-last-reset-vals-n  upLR downLR *wUpInitLo *wUpInitHi *wDnInitLo *wDnInitHi)))

    ;;STEP 2: INITIALIZE THE ART2 CONSTANTS
    ;;done in calculation function area

    ;;STEP 3: INITIALIZE THE NETWORK (ART2 VARIABLES-in arrays)
    ;;is this done elsewhere too??
    (initNetwork nInputs nOutputs) 

    ;;STEP 4: RUN ART2 -- CALCULATE THE VALUES FOR EACH CYCLE 
    ;; ON EACH FIELD AND CELL    
    (setf *learned-patterns
          (learnPatterns nInputs nOutputs test-pattern-lists num-Cycles 
                         graph-every-x-cycle  initial-x-pix incr-x-pix))

    ;;OLD VERSION USING MY GRAPHING -- WORKED
    ;;RUN ART2
    ;; (ART2) replaced by initNetwork plus setf *learned-patterns ... above

    ;;STEP 5: CONVERT ART2 ARRAY VALUES TO X, Y COORDS IN LISTS
    ;; and  STEP 6:  GRAPH THE TERMINAL CELL VALUES 
    ;;if only print graphs for last cycle  
    #|FOR ARRAYS (if graph-last-cycle
       (progn
         ;;now make the graphing  list of lists
         (setf graphing-array-sym-list '(*input  *v-activity  *wUp *wdn *y-output *reset)
               graphing-array-list `(,*input  ,*v-activity  ,*wUp ,*wdn ,*y-output ,*reset-val))
         ;;find the graphing-symbols-list
         (setf  graphing-symbols-list `(,input ,v-activity  ,wUp ,wdn ,y-output ,reset))
        
         (afout 'out (format nil "In ART2-manager, graphing-array-list= ~Agraphing-symbols-list= ~% "  graphing-array-list graphing-symbols-list ))

         ;;create the new lists
         (multiple-value-setq (*converted-arrays-lists *new-symbols-list)
             (convert-ART2-arrays-to-lists  graphing-array-sym-list  graphing-symbols-list
                                            initial-x-pix incr-x-pix))
         (setf *converted-arrays-list-of-lists (append *converted-arrays-list-of-lists 
                                                       (list *converted-arrays-lists)))

         ;;create the interfaces and graphs
         (graph-ART2 " ART2 LAST Cycle" )
         ))|#
    (when (and *graph-last-cycle (=  end-test-all-patterns-n 0))
      ;;for keeping track of pane text and graph info
      ;;*art-inst-text-info-list
      ;;*art-inst-graph-info-list            
      (graph-ART2 (format nil "ART2 LAST Cycle N= ~A Resets= ~A "*overall-cycle-n  (find-art-above-min) )))

    (when (> end-test-all-patterns-n 0)
      (setf learned-test-patterns
            (learnPatterns nInputs nOutputs test-pattern-lists end-test-all-patterns-n 
                           graph-every-x-test-cycle   initial-x-pix incr-x-pix))
      ;;graph the end-test patterns
      (when   graph-end-tests-p
        (graph-ART2 (format nil "ART2 TEST Cycle N= ~A Resets= ~A " *overall-cycle-n  (find-art-above-min) )))        
      ;;end outer end-test when
      )

    (if *make-art-data-text-p
        (pprint *art-data-text))

    ;;end ART2-manager
    (values converted-symvals-lists  symbols-list)
    ))


;;TESTING ART2 with (runart) --------------------------------------------
;;  ((num-cycles 5) (ninputs 5)(noutputs 3) (test-pattern-lists testPatterns6) returns(((0 (40 0.0051200018)) (1 (80 0.22896)) (2 (120 0.88416)) (3 (160 0.39984)) (4 (200 -0.007360004))) ((0 (40 2.1019472E-4)) (1 (80 0.009399642)) (2 (120 0.03629799)) (3 (160 0.016414889)) (4 (200 -3.0215495E-4))) ((0 (40 0.0933)) (1 (80 0.0579)) (2 (120 0.0521)) (3 (160 0.068)) (4 (200 0.0961))) ((0 (40 0.09065001)) (1 (80 0.09895)) (2 (120 0.0658)) (3 (160 0.0694)) (4 (200 0.07865))) ((0 (40 0.01506)) (1 (80 0.011299999)) (2 (120 0.01677424)) (3 (160 0)) (4 (200 0))) ((0 (40 0.0155)) (1 (80 0.01418)) (2 (120 0.029842116)) (3 (160 0)) (4 (200 0))) ((0 (40 0.0)) (1 (80 0.0)) (2 (120 0.0))))



;;xxx --------------------------------- ART2 CALCULATION FUNCTIONS --------------



;; XXX ---------------- LEARN PATTERNS MANAGER FUNCTION ---------------

;;LEARNPATTERNS
;;
;;ddd
(defun learnPatterns (nInputs nOutputs testPatterns num-Cycles
                              graph-every-x-cycle  initial-x-pix incr-x-pix 
                              &key (run-random-test-p  NIL) ;;problems with T
                              ignore-pattern-num-cycles-p
                              (graphing-symbols-list  *graphing-symbols-list))
                            ; Watson uses num = 50 when calls in original (ART2) was this orig args? (i j) 
  "In ART2.lisp, Main ART2 Calculation Manager that cycles through all training patterns"
  (afout 'out (format nil ">>>> IN LEARNPATTERNS, TESTPATTERNS= ~a ~%" testPatterns))

  (declare (special learningcyclecounter *all-reset-info-list))

  (setf  learningcyclecounter 0  ;;2013-11 added global var here
         *all-reset-info-list nil)
  

  (let
      ((random-testpatterns)
       (pattern-num-cycles)
       (setsym-2dim-nested-lists-p *setsym-2dim-nested-lists-p)       
       (p)
       (reset-p)
       )
    ;;FOR THE PATTERN LIST, RUN EACH PATTERN
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

     ;;added 2015-08-07, reset all activations to 0, or new pattern uses previous activation patterns
     (zeroactivations nInputs nOutputs)

     (afout 'out (format nil ">>>> INPUTS= ~A <<<<" p))

     ;;SET ALL RESET VALUES TO 0 with each new INPUT (not each cycle, causes flip-flop betw previous and new y's)
     (loop
      for n from 1 to nOutputs
      do
      (setsymval 'reset (list n) 0)
      )

     ;;FOR REPEAT EACH PATTERN FOR NUM-CYCLES
     (setPattern p nInputs nOutputs)
     (loop
      for i from 1 to num-Cycles
      do
      ;;Keep track of cycle num (can't find another--is this redundant????)
      (incf  *overall-cycle-n)
      ;;was(dotimes (n num-Cycles)
      (setq learningCycleCounter (1+ learningCycleCounter))
     (afout 'out (format nil ">>>>   learningCycleCounter= ~A"  learningCycleCounter))
      ;;Formerly set all reset values = 0 here, but caused flipflop betw cycles; Grossberg says reset valid for one INPUT not one cycle.

      (OneCycle ninputs noutputs)  ;; temp quoted out i j bec of error
      ;;(updateScreen))
      (setq *learned-categories*
            ;;SSS  START HERE what is p? it is an &aux var in findLargestNonResetOutput
            ;;  what is happening, how to change to my normal coding 
            ;;NOTE: In Watson, p is a vector
            ;; pi = ui + SUMj g(yj) * wDownji (in 7.3)
            (cons (list p (findLargestOutput nOutputs))
                  *learned-categories*))
      ;;sss
      ;;was-- do I need to copy to lists??   (copy-data-to-cumulative-arrays *cum-array-list)
      (when *make-art-graphs-p
      (cond
       ((or 
         ;;graph every x cycles
         (and (my-floor graph-every-x-cycle :floor 1)
              (member  (/ i  graph-every-x-cycle) *graph-every-x-cycle-list :test  '=))
         ;;or if reset and *display-graphs-on-reset
         (and  *my-reset-broadcast  *display-graphs-on-reset))
        ;;reset it to nil after this is triggered
        (setf *my-reset-broadcast NIL)
               
        ;;FIND THE GRAPHING-SYMBOLS-LIST -- GRAPHING DONE HERE
        ;; *graphing-symbols-list=  '(input v-activity wUp wDn y-output reset-val) ;;find below after set `(,input ,v-activity  ,wUp ,wdn ,y-output))
        ;; *graphing-sym-points-list '(*input *v-activity *wUp *wDn *y-output  *reset-val))
        ;;NOW PUT VALUES IN THE GRAPHING POINTS-LISTS
        ;;create the interfaces and graphs 
        (setf  resets (find-art-above-min )  ;;(find-largest-ART-ValueIndex 'reset)
               interface-title (format nil "ART2 Cycle= ~A Resets= ~A"
                                       learningCycleCounter resets ))
        ;;SSS INPUT AND V-Activity GRAPHS; SOMETIMES IDENTICAL ON ALL INPUTS, SOMETIMES OK, SOMETIMES SOME VALS DON'T PLOT RIGHT DEBUG
        ;;GRAPH ART
        ;;make the point lists (*input-points *v-activity-points *p-points *wup-points *wdn-points *y-output-points *reset-points)

        (multiple-value-setq (converted-symvals-lists symbols-list)
            (make-graph-points-lists '(input v-activity p wup wdn reset y-output)))
        
        ;;graph the points lists
        ;;aaaa
        (graph-ART2  interface-title) 
        (sleep 3)
        )
       (t nil))
      ;;end when *make-art-graphs-p
      )

    ;;MAKE DATA TEXT?
    (if *make-art-data-text-p
        (setf *art-data-text (append *art-data-text  (list (make-art-cycle-data-text  '(input v-activity p u r  reset wup wdn y-output))))))

      ;;end inner loop, loop
      ))
    (afout 'out (format nil "At END of learnPatterns, *learned-categories* = ~s "
                        *learned-categories*))
    
    ;;FOR RANDOM PATTERNS
    (when  run-random-test-p
      (setf random-testpatterns (my-randomize-list testpatterns))
      (learnPatterns ninputs noutputs random-testpatterns 1 1 initial-x-pix incr-x-pix
                     :run-random-test-p nil :ignore-pattern-num-cycles-p T)
      (afout 'out (format nil "At END of RANDOM TEST CYCLES of learnPatterns, *learned-categories* = ~s " *learned-categories*))
      ;;end when
      )
    ;;NEW set *art2-instance nil to avoid contamination??? SSS
    ;;causes error--missing class?? (setf *art2-instance NIL)

    ;;end let LearnPatterns
    ))

;;NOTE:   (/ 1.9696045E10  10000000000) = 1.9696044
 
;;xxx -------------------------------------- SUB-CALCULATION FUNCTIONS --------------------------

;;FRANDOM
; Floating point  random numbers:
;;ddd
(defun frandom (low high)
  (let ((range (- high low)))
    (+ (* (/ (random 1000) 1000.0) range) low)))


;;FINDLARGESTOUTPUT
;;ddd
(defun findLargestOutput (&optional nOutputs) ;;was &aux (maxIndex 0) (maxVal (aref *y-output 0)))
  "In ART2, returns (values  maxIndex maxVal) nOutputs not needed, outdated."
  (find-largest-ART-ValueIndex 'y-output)
  )
;;TEST
;; (findLargestOutput) =   1  -0.1 


;;FINDLARGEST-NONRESET-OUTPUT
;;ddd
(defun findLargest-NonReset-Output (&optional nOutputs) ;;was &aux (maxIndex 0) (maxVal (aref *y-output 0)))
  "In ART2, returns (values  maxIndex maxVal) nOutputs not needed, outdated."
  (find-largest-ART-ValueIndex 'y-output  :avoid-reset-indexes-p t)
  )
;;TEST
;; (findLargest-NonReset-Output 5) =   1  -0.1 


;;ARRAY-VERSION
#|(defun findLargestOutput (nOutputs) ;;was &aux (maxIndex 0) (maxVal (aref *y-output 0)))
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
  (if (= *print-detail 2) (afout 'out (format nil "maxIndex= ~a " maxIndex)))
  (values maxIndex maxVal)
  ))|#
;;TEST
;; (findLargestOutput 4) = 0 (when *y-output= #(0.4372523 0.0 0.0 0.0 0.0); note that &aux sets mVal to 0.437 which is greater than any other value, but is not returned.
; The following function returns d if (aref *y-output index) is the
; largest value in array *y-output AND (aref *y-output index) has not been reset:

;;G
;;
;;ddd
(defun g (index &optional nOutputs) ;;was  &aux j mVal)   ;; (maxIndex (findLargestOutput)))
  "In ART2, If index = max y-output index AND that y NOT reset AND the y-output for that index > *onCenterThreshold, then RETURNS constant d, otherwise returns 0.00001"
  (multiple-value-bind (maxIndex maxVal)
      (findLargest-NonReset-Output) ;; largest NOT been reset
  (cond
   ((and
       (equal index maxIndex)
       (> maxVal *onCenterThreshold)) ;;was resetThreshold
    d)       ;;d=  0.4?
   (t  0.0001))  ;;prevent divide by 0??
  ;;end let, g
  ))


;;ARRAY VERSION
#|(defun g (index nOutputs) ;;was  &aux j mVal)   ;; (maxIndex (findLargestOutput)))
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
    0.0)))|#

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
(defun sigmoid (x-activity)
  "In ART2.lisp, threshold function."
  (cond
   ((> x-activity theta)   ;;theta 0.3
    x-activity)
   (t  0.0001)) ;;was 0, avoid div by 0
  )

;;L2NORM
; L2 Norm of a vector/list of values:
;;ddd
(defun L2NORM (var  vLength) ;; &aux (sum 0.0) i j)
  "In ART2.lisp, Norm of a vector/list of values. Sqrt of sum of squares. Note: In seq of similar nums, l2norm about (* 0.5 simple-sum), but if one num dominate, close to that number."
    (let*
        ((l2norm 0.0001) ;;was 0
         (sum 0.0001) ;;was 0
         (i)
         (vi-value)  ;;error was (getsymval v (list i)))
         )
   (loop
    for i from 1 to vLength
    do
    (setf  vi-value (getsymval var  (list i))
          sum (+ sum (* vi-value vi-value)))
    ;;end let, loop
    )
  (setf l2norm (+ (sqrt sum) 0.0001)) ;;0.001prevents l2norm=0, for division later
  ;;end let, L2NORM
  ))
;; (L2NORM  'v-activity 9) = 17.822482
;;
;;Note: To get an idea of how values are affected
;;  (my-normalize '(1 1 1 1.0)) = 2.0 4.0 4.0  (0.5 0.5 0.5 0.5)  2.0
;;  (my-normalize '(.1 .1 .1 .1)) = 0.2  0.040000003  0.4  (0.5 0.5 0.5 0.5)  2.0
;;  (my-normalize '(1 2 3 4 5)) = 7.4161983 55.0  15.0  (0.13483998 0.26967996 0.40451992 0.5393599 0.6741999)  2.0225997
;;  (my-normalize '(11 12 13 14)) = 25.0998  630.0  50.0  (0.4382505 0.47809145 0.5179324 0.55777336)  1.9920478
;;  (my-normalize '(1 -2  3 -4 5)) = 7.4161983 55.0  3.0  (0.13483998 -0.26967996 0.40451992 -0.5393599 0.6741999)  0.4045199
;;  (my-normalize '(0.1 0.2 0.3 0.4 0.5)) = 0.7416199 0.55 1.5 (0.13483997 0.26967993 0.40451992 0.53935987 0.6741998)  2.0225995
;;  (my-normalize '(0.01 0.02 0.03 0.04 0.05)) = 0.074161984
;;  (my-normalize '(1 2 3 4 1000)) = 1000.015 1000030.0  1010.0  (9.99985E-4 0.00199997 0.0029999549 0.00399994 0.999985)  1.0099849
;;  (my-normalize '(1 1 1  .0001)) = 1.7320508  3.0  3.0001  (0.57735026 0.57735026 0.57735026 5.7735026E-5)  1.7321085

;;ARRAY VERSION
#|(defun L2NORM (v vLength) ;; &aux (sum 0.0) i j)
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
  ))|#
;;TEST
;;   (L2NORM  *input 9) = 61.491974  (where *input = #(-0.079519995 -0.007360004 -0.04432 1.00784 1.02688 1.0753601 -0.016960003 -0.05472 0.058239997))

;;F1STMcycle
; Update F1 STM arrays:
;
(defun F1STMcycle (nInputs nOutputs &aux i j sum norm max1 max2)
  "In ART2.lisp, Updates F1 STM arrays"
  ; Calculate p from u input and backwards feed back:
  ;;(break "Beginning of F1STMcycle")
  (let
      ((input)
       (sum-F2dn)
       (q-activity)
       (v-activity)
       (v-activity0)
       (u)
       (r)
       (sigmoid-u)
       (p)
       (L2norm-p)
       (L2norm-p-e)
       (L2norm-v)
       (L2norm-w)
       (L2norm-w-e)
       (p0)
       (w)
       (wdn)
       (x-activity)
       (max1u)
       (max2p)
       )
    (afout 'out (format nil ">>>>> IN  F1STMcycle, nInputs= ~s " nInputs))

;;=================== original middle ====================
  ;; New Step 1:
  ;; Step 5: Update w using u sigmoid-u w alpha input   (Watson eq. 8)
    (loop
     for i from 1 to nInputs
     do
     (setf input (getsymval 'input (list i))
           u (getsymval 'u (list i))
           sigmoid-u (sigmoid u)
           w (* alpha (+ input  (* a  sigmoid-u))))
     (setsymval 'w (list i)  w)
     ;;was (setf (aref *w i) (* alpha (+ (aref *input i) (* a (sigmoid (aref *u i))))))

     (if (= *print-detail 3) (afout 'out (format nil  "Step 5: FIND w, FOR i= ~A~%  w=(* alpha (+ input  (* a  (sigmoid u))))= ~A;  alpha= ~A input= ~A  a=~A sigmoid-u= ~A  u= ~A " i  w alpha input  a sigmoid-u u  )))
     ;;end loop
     )
  ;; New Step 2:
  ; ; Step 6: Update x-activity using w, L2norm-w (Watson eq. 9:)
    (setq L2norm-w (L2NORM 'w nInputs) 
         L2norm-w-e (+ L2norm-w e))
    ;;(setq norm (+ (L2NORM 'w nInputs) e)) ;;causes div by 0 sometimes
     (if (> *print-detail 1) (afout 'out (format nil "Step 6: FIND x-activity,~%Step 6.1 UPDATE L2norm-w-e=  ~A after w loop:~%; L2norm-w= ~A " L2norm-w-e L2norm-w)))
    (loop
     for i from 1 to nInputs
     do
     ;;setting x-activity
     (setf w  (getsymval 'w (list  i))
           x-activity 
           (my-floor-ceiling (/ w  L2norm-w-e ) :floor *x-activity-floor :ceiling *x-activity-ceiling))
     (setsymval 'x-activity (list  i) x-activity)
     ;;was (setf (aref *x-activity i) (/ (aref *w i) norm ))
          (if (= *print-detail 3) (afout 'out (format nil  "Step 6.2 FIND x-activity, FOR i= ~s~% x-activity i=(my-floor-ceiling (/ w  L2norm-w-e ) :floor *x-activity-floor :ceiling *x-activity-ceiling)= ~s; w= ~A   L2norm-w-e= ~s " i x-activity w  L2norm-w-e)))
     ;;end loop
     )

;;============ original beginning =================
  ;; New Step 3:
    ;;Step 1: find total sum =  (+ sum (* g wdn)) for each input/output
    (loop
     for i from 1 to nInputs
     do
     (setf sum-F2dn  0.0)                  ;;try 0.0   0.00001)
    ;;Step 1.1  Find sum-F2dn  of all F2 wdn signals
     (loop
      for j from 1 to nOutputs
      do
      (setf g (g  j nOutputs)
            wdn (getsymval 'wdn (list j i)))
      (setf sum-F2dn 
            (my-floor-ceiling (+ sum-F2dn  (* g wdn)) :floor *wt-floor :ceiling *wt-ceiling)) ;;was  (aref *wdn j i))));  *wt-floor = 0+ *wt-ceiling= 1.0
      (if (= *print-detail 3) (afout 'out (format nil  "Step 1.1: FIND sum-F2dn,  Inside loop (j to nOutputs), FOR j=~A i= ~s, nOutputs= ~s ~%,  sum-F2dn= (+ sum-F2dn (* g wdn))= ~A; g= ~A wdn= ~A" j i nOutputs sum-F2dn g wdn)))
      ;;end loop
      )
  ;; New Step 4:
  ;;Step 3: Update u using v-activity and norm-v (Watson eq. 6:)
    (setf  L2norm-v (L2NORM  'v-activity  nInputs))
    (loop
     for i from 1 to nInputs
     do
     (setf v-activity0 (getsymval 'v-activity (list i))
           u   (/ v-activity0  L2norm-v))
     (setsymval 'u (list  i) u)  ;;was (setf (aref *u i) (/ (aref *v-activity i) norm))
     (if (= *print-detail 3) (afout 'out (format nil  "Step 3: FIND u, FOR i= ~A, ~& u= (/ v-activity0i L2norm-v)= ~A;  v-activity0= ~A  L2norm-v= ~A" i  u  v-activity0 L2norm-v)))
     ;;end loop
     )
  ;; New Step 5:
     ;;Step 1.2  finds pi based upon ui  + sum-F2dn 
     (setf  u  (getsymval 'u (list i))
            p (+ u  sum-F2dn))
     (setsymval 'p (list i) p) ;; a  ;;was(setf (aref *p i) (+ (aref *u i) sum))a
     (if (>  *print-detail 1) (afout 'out (format nil  "Step 1.2 FIND p FOR i= ~A, ~%p=(+ u  sum-F2dn)= ~A; u0= ~A sum-F2dn= ~A" i p u sum-F2dn)))
     ;;end loop
     )
    ;; New Step 6:
    ;;Step 2:  For each input, finds q-activity based upon p and norm 
    ;; Update q-activity using eq. 5
    (setf  L2norm-p  (L2NORM 'p nInputs);;was(my-floor (L2NORM 'p nInputs) :floor .0001)
          L2norm-p-e  (+ L2norm-p  e))  ;;e= 0.04?
    (loop
     for i from 1 to nInputs
     do
     (setf p (getsymval 'p (list  i))
           q-activity (/  p  L2norm-p-e))   ;;chk
     (setsymval 'q-activity (list  i) q-activity) ;;was(setf (aref *q-activity i) (/ (aref *p i) norm))
     (if (= *print-detail 3) (afout 'out (format nil "Step 2: FIND q-activity, FOR i= ~A, q-activity= (/ pi  norm-p-e)= ~A;  pi= ~A   L2norm-p-e=(+ (L2NORM 'p nInputs) e)= ~A" i q-activity p L2norm-p)))
     ;;end loop
     )

  ;; New Step 7:
  ; Step 4: Update v-activity using Watson eq. 7:
    (loop
     for i from 1 to nInputs
     do
     (setf x-activity  (getsymval 'x-activity(list i))
           q-activity (sigmoid (getsymval 'q-activity (list i)))
           v-activity (sigmoid (+ x-activity (* b q-activity))))
     (setsymval 'v-activity (list i) v-activity)
     ;;was (setf (aref *v-activity i) (sigmoid (+ (aref *x-activ-activityity i) (* b (sigmoid (aref *q-activity i))))))
     (if (= *print-detail 3) (afout 'out (format nil  "Step 4: FIND v-activity, FOR i= ~A~%v-activity=(sigmoid (+ x-activity (* b q-activity)))= ~A;  x-activity= ~A b= ~A q-activity= ~A%" i v-activity x-activity b q-activity)))
     ;;end loop
     )

;;=========== original end =================
  ;;??? HOW DOES THIS WORK WITH TESTRESET???  
  ;;New Step 8:
  ;; Step 7.1: Calculate  r   using u p    (Watson eq. 20)
  ;;  7.1: Find max1u  (of all ui)  and max2u (of all pi)
    (setf  max1u -1000.0  max2p  -1000.0)
    (loop
     for i from 1 to nInputs  ;;use i or j?? 
     do
       (setf u (getsymval 'u (list  i)))
       (when (< max1u  u) (setf max1u u))
       ;;was (if (< max1 (aref *u j)) (setq max1 (aref *u j)))
       (setf p (getsymval 'p (list i)))
       (when (< max2p  p) (setq max2p  p))
       ;;was(if (< max2 (aref *p j)) (setq max2 (aref *p j)))

      (if (= *print-detail 3) (afout 'out (format nil  "Step 7: FIND  r,  vector components betw u and p??~% Step 7.1 Find max1u  (of all ui)  and max2u (of all pi)  FOR i= ~A, ~%  u i= ~s;  max1u= ~s ~%Pj= ~A  max2p= ~A" j u max1u  p max2p)))
     ;;end loop
     )
    ;;Prevent div by 0? not needed?
    ;;(setq max1u (my-floor (+ max1u 0.001) :floor .001))
    ;;(setq max2p (my-floor (+ max2p 0.001) :floor .001))
   ; l2-norm-r  low value prevents reset. 
   ;;New Step 9:
   ;;Step 7.2 FIND  ri
    (loop
     for i from 1 to nInputs
     do
     ;;do these reflect the cosines of  u, p and r the angle between?
     (setf u (getsymval 'u (list i))
           p (getsymval 'p (list  i))
           r  (-  (/  u   max1u)  (/ p  max2p)) ;;was (my-floor (-  (/  u   max1u)  (/ p  max2p)) :floor .0001)
           )
     (setsymval 'r (list  i)  r)
     ;;was(setf (aref *r i) (my-floor (- (/ (aref *u i) max1)  (/ (aref *p i) max2))))
     (if (= *print-detail 3) (afout 'out (format nil  "Step 7.2: FIND  r  FOR i= ~A~%  r=(my-floor (- (/  u  max1u)  (/ p  max2p)) :floor .0001)= ~a;   u= ~A max1u= ~A,  p= ~A  max2p= ~A" i  r u max1u p max2p  )))
     ;;end loop
     )
    (afout 'out (format nil "--------------- END  F1STMcycle ------------------%" ))
    ;;end let, F1STMcycle
    ))


;;OLD-WATSON ORIGINAL VERSION--BACKWARDS ORDER??
#|Me: It appears that Watson implemented the equations in backwards order. He started with p and q, then v and x and finally inputs.  Seems to me should be in opposite direction.  His led to weird results of inputs firing expectations that were from the previous input.
(defun F1STMcycle (nInputs nOutputs &aux i j sum norm max1 max2)
  "In ART2.lisp, Updates F1 STM arrays"
  ; Calculate p from u input and backwards feed back:
  ;;(break "Beginning of F1STMcycle")
  (let
      ((input)
       (sum-F2dn)
       (q-activity)
       (v-activity)
       (v-activity0)
       (u)
       (r)
       (sigmoid-u)
       (p)
       (L2norm-p)
       (L2norm-p-e)
       (L2norm-v)
       (L2norm-w)
       (L2norm-w-e)
       (p0)
       (w)
       (wdn)
       (x-activity)
       (max1u)
       (max2p)
       )
    (afout 'out (format nil ">>>>> IN  F1STMcycle, nInputs= ~s " nInputs))
    ;;Step 1: find total sum =  (+ sum (* g wdn)) for each input/output
    (loop
     for i from 1 to nInputs
     do
     (setf sum-F2dn  0.0)                  ;;try 0.0   0.00001)
    ;;Step 1.1  Find sum-F2dn  of all F2 wdn signals
     (loop
      for j from 1 to nOutputs
      do
      (setf g (g  j nOutputs)
            wdn (getsymval 'wdn (list j i)))
      (setf sum-F2dn 
            (my-floor-ceiling (+ sum-F2dn  (* g wdn)) :floor *wt-floor :ceiling *wt-ceiling)) ;;was  (aref *wdn j i))));  *wt-floor = 0+ *wt-ceiling= 1.0
      (if (= *print-detail 3) (afout 'out (format nil  "Step 1.1: FIND sum-F2dn,  Inside loop (j to nOutputs), FOR j=~A i= ~s, nOutputs= ~s ~%,  sum-F2dn= (+ sum-F2dn (* g wdn))= ~A; g= ~A wdn= ~A" j i nOutputs sum-F2dn g wdn)))
      ;;end loop
      )
     ;;Step 1.2  finds pi based upon ui  + sum-F2dn 
     (setf  u  (getsymval 'u (list i))
            p (+ u  sum-F2dn))
     (setsymval 'p (list i) p) ;; a  ;;was(setf (aref *p i) (+ (aref *u i) sum))a
     (if (>  *print-detail 1) (afout 'out (format nil  "Step 1.2 FIND p FOR i= ~A, ~%p=(+ u  sum-F2dn)= ~A; u0= ~A sum-F2dn= ~A" i p u sum-F2dn)))
     ;;end loop
     )
    ;;Step 2:  For each input, finds q-activity based upon p and norm 
    ;; Update q-activity using eq. 5
    (setf  L2norm-p  (L2NORM 'p nInputs);;was(my-floor (L2NORM 'p nInputs) :floor .0001)
          L2norm-p-e  (+ L2norm-p  e))  ;;e= 0.04?
    (loop
     for i from 1 to nInputs
     do
     (setf p (getsymval 'p (list  i))
           q-activity (/  p  L2norm-p-e))   ;;chk
     (setsymval 'q-activity (list  i) q-activity) ;;was(setf (aref *q-activity i) (/ (aref *p i) norm))
     (if (= *print-detail 3) (afout 'out (format nil "Step 2: FIND q-activity, FOR i= ~A, q-activity= (/ pi  norm-p-e)= ~A;  pi= ~A   L2norm-p-e=(+ (L2NORM 'p nInputs) e)= ~A" i q-activity p L2norm-p)))
     ;;end loop
     )
  ;;Step 3: Update u using v-activity and norm-v (Watson eq. 6:)
    (setf  L2norm-v (L2NORM  'v-activity  nInputs))
    (loop
     for i from 1 to nInputs
     do
     (setf v-activity0 (getsymval 'v-activity (list i))
           u   (/ v-activity0  L2norm-v))
     (setsymval 'u (list  i) u)  ;;was (setf (aref *u i) (/ (aref *v-activity i) norm))
     (if (= *print-detail 3) (afout 'out (format nil  "Step 3: FIND u, FOR i= ~A, ~& u= (/ v-activity0i L2norm-v)= ~A;  v-activity0= ~A  L2norm-v= ~A" i  u  v-activity0 L2norm-v)))
     ;;end loop
     )
  ; Step 4: Update v-activity using Watson eq. 7:
    (loop
     for i from 1 to nInputs
     do
     (setf x-activity  (getsymval 'x-activity(list i))
           q-activity (sigmoid (getsymval 'q-activity (list i)))
           v-activity (sigmoid (+ x-activity (* b q-activity))))
     (setsymval 'v-activity (list i) v-activity)
     ;;was (setf (aref *v-activity i) (sigmoid (+ (aref *x-activ-activityity i) (* b (sigmoid (aref *q-activity i))))))
     (if (= *print-detail 3) (afout 'out (format nil  "Step 4: FIND v-activity, FOR i= ~A~%v-activity=(sigmoid (+ x-activity (* b q-activity)))= ~A;  x-activity= ~A b= ~A q-activity= ~A%" i v-activity x-activity b q-activity)))
     ;;end loop
     )
  ;; Step 5: Update w using u sigmoid-u w alpha input   (Watson eq. 8)
    (loop
     for i from 1 to nInputs
     do
     (setf input (getsymval 'input (list i))
           u (getsymval 'u (list i))
           sigmoid-u (sigmoid u)
           w (* alpha (+ input  (* a  sigmoid-u))))
     (setsymval 'w (list i)  w)
     ;;was (setf (aref *w i) (* alpha (+ (aref *input i) (* a (sigmoid (aref *u i))))))

     (if (= *print-detail 3) (afout 'out (format nil  "Step 5: FIND w, FOR i= ~A~%  w=(* alpha (+ input  (* a  (sigmoid u))))= ~A;  alpha= ~A input= ~A  a=~A sigmoid-u= ~A  u= ~A " i  w alpha input  a sigmoid-u u  )))
     ;;end loop
     )
  ; ; Step 6: Update x-activity using w, L2norm-w (Watson eq. 9:)
    (setq L2norm-w (L2NORM 'w nInputs) 
         L2norm-w-e (+ L2norm-w e))
    ;;(setq norm (+ (L2NORM 'w nInputs) e)) ;;causes div by 0 sometimes
     (if (> *print-detail 1) (afout 'out (format nil "Step 6: FIND x-activity,~%Step 6.1 UPDATE L2norm-w-e=  ~A after w loop:~%; L2norm-w= ~A " L2norm-w-e L2norm-w)))
    (loop
     for i from 1 to nInputs
     do
     ;;setting x-activity
     (setf w  (getsymval 'w (list  i))
           x-activity 
           (my-floor-ceiling (/ w  L2norm-w-e ) :floor *x-activity-floor :ceiling *x-activity-ceiling))
     (setsymval 'x-activity (list  i) x-activity)
     ;;was (setf (aref *x-activity i) (/ (aref *w i) norm ))
          (if (= *print-detail 3) (afout 'out (format nil  "Step 6.2 FIND x-activity, FOR i= ~s~% x-activity i=(my-floor-ceiling (/ w  L2norm-w-e ) :floor *x-activity-floor :ceiling *x-activity-ceiling)= ~s; w= ~A   L2norm-w-e= ~s " i x-activity w  L2norm-w-e)))
     ;;end loop
     )

  ;;??? HOW DOES THIS WORK WITH TESTRESET???  
  ;; Step 7. 1: Calculate  r   using u p    (Watson eq. 20)
  ;;  7.1: Find max1u  (of all ui)  and max2u (of all pi)
    (setf  max1u -1000.0  max2p  -1000.0)
    (loop
     for i from 1 to nInputs  ;;use i or j?? 
     do
       (setf u (getsymval 'u (list  i)))
       (when (< max1u  u) (setf max1u u))
       ;;was (if (< max1 (aref *u j)) (setq max1 (aref *u j)))
       (setf p (getsymval 'p (list i)))
       (when (< max2p  p) (setq max2p  p))
       ;;was(if (< max2 (aref *p j)) (setq max2 (aref *p j)))

      (if (= *print-detail 3) (afout 'out (format nil  "Step 7: FIND  r,  vector components betw u and p??~% Step 7.1 Find max1u  (of all ui)  and max2u (of all pi)  FOR i= ~A, ~%  u i= ~s;  max1u= ~s ~%Pj= ~A  max2p= ~A" j u max1u  p max2p)))
     ;;end loop
     )
    ;;Prevent div by 0? not needed?
    ;;(setq max1u (my-floor (+ max1u 0.001) :floor .001))
    ;;(setq max2p (my-floor (+ max2p 0.001) :floor .001))
   ; l2-norm-r  low value prevents reset. 
   ;;Step 7.2 FIND  ri
    (loop
     for i from 1 to nInputs
     do
     ;;do these reflect the cosines of  u, p and r the angle between?
     (setf u (getsymval 'u (list i))
           p (getsymval 'p (list  i))
           r  (-  (/  u   max1u)  (/ p  max2p)) ;;was (my-floor (-  (/  u   max1u)  (/ p  max2p)) :floor .0001)
           )
     (setsymval 'r (list  i)  r)
     ;;was(setf (aref *r i) (my-floor (- (/ (aref *u i) max1)  (/ (aref *p i) max2))))
     (if (= *print-detail 3) (afout 'out (format nil  "Step 7.2: FIND  r  FOR i= ~A~%  r=(my-floor (- (/  u  max1u)  (/ p  max2p)) :floor .0001)= ~a;   u= ~A max1u= ~A,  p= ~A  max2p= ~A" i  r u max1u p max2p  )))
     ;;end loop
     )
    (afout 'out (format nil "--------------- END  F1STMcycle ------------------%" ))
    ;;end let, F1STMcycle
    ))|#


#|Done in config-art 
(defparameter *wt-floor  0.00)
(defparameter *wt-ceiling 1.00)
(defparameter *x-activity-floor -100)
(defparameter *x-activity-ceiling 1000)
(defparameter *y-output-floor -100)
(defparameter *y-output-ceiling 1000)|#
;;
;;F2STMcycle
; Update F2 STM storage:
;;ddd
(defun F2STMcycle (nInputs nOutputs) ;;&aux i j sum)
  "In ART2.lisp,Updates F2 STM storage"
  (let
      ((i)
       (j)
       (sum-p-wup)
       (p)
       (wup)
       (y-output)
       (reset) ;;this--current
      ;; (r)  ;;or this??
       )
   (afout 'out (format nil ">>>>> IN F2STMcycle: learningCycleCounter= ~A <<<<<<" learningCycleCounter))
   ;;F2 Step 1: Find pre reset  y-output= 
  (loop 
   for j from 1 to nOutputs
   do
     (setq sum-p-up 0.0)
     ;;1.1 
     (loop
      for i from 1 to nInputs
      do
      (setf p  (getsymval 'p (list i))
            wUp (getsymval 'wUp (list  i j))
            sum-p-up  (+ sum-p-up (* p  wUp )))
     (if (= *print-detail  3) (afout 'out (format nil "Step 1:FIND sum-p-up, For j= ~A, i= ~A;~%sum-p-up=(+ sum-p-up (* p  wUp ))= ~A, p= ~A wUp= ~A" j i sum-p-up p wup)))
       ;;end inner loop
       )
     ;;Step 2: Set y-output to sum if  no reset, limit with my-floor-ceiling
     (setf y-output
           (my-floor-ceiling  sum-p-up :floor *y-output-floor :ceiling *y-output-ceiling))  ;;-100 to 1000
     (setf  reset (getsymval 'reset (list j)))
     ;;SSS START HERE  SHOULD THIS BE  R OR RESET????
     ;;original Watson (if (aref reset j) (setf (aref y j) -0.1))
     (if (> reset 0) (setsymval 'y-output (list  j)  0.0001)
       (setsymval 'y-output (list  j) y-output))
     (if (> *print-detail  1) (afout 'out (format nil  "In F2STMcycle, Step 2: FIND y-output FOR j= ~A,  y-output=~a ; reset= ~A"  j  y-output reset )))

   ;;end loop, F2STMcycle
   )
  ;;end let, F2STMcycle
  ))

;;ARRAY VERSION
#|(defun F2STMcycle (nInputs nOutputs &aux i j sum)
  "In ART2.lisp,Updates F2 STM storage"
  (dotimes (j nOutputs)
    (progn
      (setq sum 0.0)
      (dotimes (i nInputs)
        (setq sum (+ sum (* (aref *p i) (aref *wUp i j)))))
      (setf (aref *y-output j) sum)
      (if (aref *reset j) (setf (aref *y-output j) -0.1))
       ;;me
       (if (> *print-detail 1) (afout 'out (format nil  "y=~a j=~a; value= ~a " *y-output j (aref *y-output j))))
      (aref *y-output j)      
      )    
    )  
  )|#


;;UPDATEWEIGHTS
;;
;;ddd
(defun updateWeights (nInputs nOutputs);;  &aux i) ;; (j (findLargestOutput)))
    "In ART2.lisp, Updates weights for LTM model."
  (let
      ((j (findLargestOutput))
       (g)
       (p)
       (wdn)
       (wdn0)
       (wup)
       (wup0)
       )
    ;;
    (if (> *print-detail 0)  (afout 'out (format nil "--- IN updateWeights; Largest output j= ~A " j)))
    ;;Step 1: Update wdn
    (setf  g (g  j nOutputs))
    (when  (>  g  *min-g)  ;;was 0.02)
      (loop
       for i from 1 to nInputs
       do
       (setf p (getsymval 'p (list  i))
             wdn0  (getsymval 'wdn (list  j i))
              ;;downLR= 0.8? was 0.12  d= 0.4 ?
             ;;was wdn (my-floor (+ wdn0 (* downLR   d  (-  p  wdn0))) :floor .0001)
             ;;test, prevent wdn from decreasing much
             wdn  (+ wdn0 (* downLR   d  (my-floor (-  p  wdn0) :floor  -0.2))))
       (setsymval 'wdn (list  j i) wdn)    
       (if (= *print-detail 3)  (afout 'out (format nil  "Step 1: FIND wDN FOR j=~a i=~a; wdn (my-floor (+ wdn0 (* downLR   d  (-  p  wdn0))) :floor .0001))=wdn= ~A~%wdn0= ~A downLR= ~A  d= ~A  p= ~A" j i wdn wdn0 downLR d p)))

       ;;Step 2: Update wups
       (setf wup0  (getsymval 'wUp (list i j))
             p (getsymval 'p (list i))
             ;;was wup (my-floor  (+  wup0  (*  upLR    d  (- p  wup0)))  :floor 0.00001 ))
             wup (+ wup0 (* upLR   d  (my-floor (-  p  wup0) :floor  -0.2))))             
                                ;;upLR= 0.8? d=0.4?    
      (setsymval 'wUp (list i j) wup)
      (if (= *print-detail 3)(afout 'out (format nil  "Step 2: FIND wUP FOR i= ~A  j=~a; wup (my-floor (+ wup0 (* upLR   d  (-  p  wup0))) :floor .0001))=wdn= ~A~%wdn0= ~A downLR= ~A  d= ~A  p= ~A" i j wup wup0 upLR d p)))
     ;;end loop
      )
      ;;end when, let, updateWeights
    )))


;;ARRAY VERSION
#|(defun updateWeights (nInputs nOutputs  &aux i) ;; (j (findLargestOutput)))
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
            (if (> *print-detail 1)  (afout 'out (format nil  "j=~a i=~a; value= ~a " j i (aref *wdn j i))))
            (setf 
             (aref *wUp i j)
             (+
              (aref *wUp i j)
              (*
               upLR
               d  (my-floor (- (aref *p i) (aref *wUp i j))   :floor 0.00001 ))))
            ;;me
            (if (> *print-detail 1)(afout 'out (format nil  "i=~a j=~a; value= ~a " i j (aref *wUp i j)))   
              )))))|#
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
               (afout 'out (format nil  "j=~a i=~a; value= ~a " j i (aref *wdn j i)))
               (setf
                (aref *wUp i j)
                (+
                 (aref *wUp i j)
                 (*
                  upLR
                  d
                  (- (aref *p i) (aref *wUp i j)))))
               ;;me
               (afout 'out (format nil  "i=~a j=~a; value= ~a " i j (aref *wUp i j)))
 
               )))
       |#


;;COMPETITIVEF2
; Competitive learning at slab F2:
;;ddd
(defun competitiveF2 (nOutputs)
  "In ART2.lisp, Calculates competitive learning at slab F2. *onCenterThreshold [Watson used term resetThreshold, but misnomer bec really is competition winner--on-center, off-surround]"
    (multiple-value-bind (y-max-index y-max-value)
        (findlargest-nonreset-output 'y-output)
    ;;Sets all other y's to 1, if largest y > *onCenterThreshold
  (when
      (>  y-max-value *onCenterThreshold) ;; 0.25 
      ;;eliminates noise by surpressing y-outputs with low values, sets y to .00001
    (loop
     for j from 1 to nOutputs
     do
      (if  (not (=  j  y-max-index))
         (setsymval 'y-output (list j) 0.0))  ;;not necessary 0.00001))
      ;;setf *art-data-text (append *art-data-text (list  "-----> IN competitiveF2, For j= ~A y-output= ~A" j (getsymval 'y-output (list j)))))
    )
      
      ;;end loop, when
      )
    (
    )
  (if (> *print-detail 0)(afout 'out (format nil "-----> IN competitiveF2, y-max-index= ~A, y-max-value= ~A  *onCenterThreshold= ~A" y-max-index y-max-value *onCenterThreshold )))
  ;;end mvb,competitiveF2
  ))
;;TEST
;;  (competitiveF2 5)


;;ARRAY VERSION
#|(defun competitiveF2 (nOutputs &aux i) ;; (jj (findLargestOutput)))
    "In ART2.lisp, Calculates competitive learning at slab F2"
    (let
        ((jj (findLargestOutput nOutputs))
         )
      (if (> (aref *y-output jj) resetThreshold)
          (dotimes (i nOutputs)
            (if (not (equal i jj))
                (setf (aref *y-output i) 0.0))))))|#
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
(defun OneCycle (nInputs nOutputs) ;;was &aux i j)
  "In ART2.lisp,  Runs one full ART2 cycle"
  (if (> *print-detail 0)(afout 'out (format nil "=======> START  OneCycle:~% >>>>> STEP 1: BEGIN  F1STMcycle >>>>>>>>>>>>>>>")))
  (F1STMcycle nInputs nOutputs) ;;xxx ;;i j) ;;me added i j bec of error
  (if (> *print-detail 0)(afout 'out (format nil ">>>>> STEP 2: BEGIN  testRESET  >>>>>>>>>>>>>>>")))
  (testReset nInputs nOutputs)
  (if (> *print-detail 0)(afout 'out (format nil ">>>>> STEP 3: BEGIN  competitiveF2  >>>>>>>>>>>>>>>")))
  (competitiveF2  nOutputs)
  (if (> *print-detail 0)(afout 'out (format nil ">>>>> STEP 4: BEGIN  F2STMcycle  >>>>>>>>>>>>>>>")))
  (F2STMcycle nInputs nOutputs)
  (if (> *print-detail 0)(afout 'out (format nil ">>>>> STEP 5: BEGIN  updateWeights  >>>>>>>>>>>>>>>")))
  (updateWeights nInputs nOutputs)
  (if (> *print-detail 0)(afout 'out (format nil ">>>>> STEP 6: BEGIN  competitiveF2  >>>>>>>>>>>>>>>")))
  (competitiveF2 nOutputs)
  )


;;
;;TESTRESET
; Check for an F2 reset condition:
 (setq skipReset nil)
;;ddd
(defun testReset (nInputs nOutputs)  ;;was, replace in let* &aux (res 0.0) (norm1 0.0) (norm2 0.0)  (n1 (+ (L2NORM p nInputs) e)) n2 temp) uses &aux instead of let??
  "In ART2.lisp, Used in One INPUT NOT one cycle--causes flip-flop if one cycle.  Checks for an F2 reset condition.  "
;;Steele-Nothing can bedone with &aux variables that cannot be done with the special form let*:
  (let*
      ((res 0.0)
       (norm1 0.0001) ;;avoid 0?
       (norm2 0.0001) ;;avoid 0?
       ;;p = signal from inputs
       (l2norm-p (L2NORM 'p nInputs))  ;;eg p=(P0 P1 P2 P3 P4)
       (n1 (+  l2norm-p  e)) ;;e= .04?
       (n2)
       (temp)
       (l2norm-r)
       ;;this is reset on each testReset
       (was-reset-p)
       (reset-x-coord) 
       (y-max-value)
       (y-max-Index)
       (cycle-reset-info)
       (cycle-reset-info1)       
       (cycle-reset-info2)
       ;;(cycle-reset-info3)
       )
(if (=  *print-detail 0)(afout 'reset-info (format nil ">>>>NEW-CYCLE: In TESTRESET 1, learningCycleCounter= ~A~%   n1=(+  l2norm-p e)=  ~A, l2norm-p= ~A e= ~A  skipReset= ~A" learningCycleCounter n1  l2norm-p  e  skipReset)))
      ;; (afout 'out (format nil "~%>>>> CYCLE INITIAL RESET-INFO: ~A"cycle-reset-info1) )
     ;;PRE-TEST FOR ADEQUATE INPUT, ETC
     (cond
      ;;Step 1:  tests for adequate INPUT to test for reset and not skipReset
      ((and (> n1 *min-input-criteria) ;;was  0.2)
            (not skipReset))
       ;;Step 1a. When NOT on first cycle
       (when
           (> learningCycleCounter 1)

         ;;Step 2: find  WDN SIGNAL l2norm-r
         (setf  l2norm-r  (L2NORM  'r nInputs)) 
         ;;Find the largest y output VALUE
         (multiple-value-setq (y-max-Index y-max-value)
             (findLargestOutput))           

         ;;Step 3. Test ON-CENTER y-output must exceed  *reset-y-criteria = .25?
         (cond
          ((> y-max-value *reset-y-criteria)  ;;0.2, orig= 0.25
           ;;set RES based upon wdn signal norm
           (setf res  (*  *vigilence-multiplier  l2norm-r)))  ;;was also set  was-reset-p t)    
         ;;where is my equiv to Watson's   (setf (aref resetVal 0) res) 
         (t  (setf res 0.0 ;;I added
                   skipReset nil)))
         ;;end when learningcyclecounter > 1, n1> 0.2
         
         ;;Step 4:
         ;;after Watson's (setf (aref resetVal 0) res)
           ;;in any case, set resetVal 0 to res
           ;;not used in mine (setf  resetVal res) 

         ;;SSSX IF NOT RESET, make sure all non-reset  reset-vals = 0        
#|         (loop
          for n from 1 to nOutputs
          do
          (unless (> (getsymval 'reset (list n)) 0)
            (setsymval 'reset (list n) 0))
          ;;end loop
          )|#
 
         ;;no longer used? not inside a cond
         #|(setf reset-x-coord  (* *overall-cycle-n *reset-x-gap)
                 *reset-vals (append *reset-vals (list (list *overall-cycle-n (list reset-x-coord  res)))))|#
         
         ;;(afout 'out cycle-reset-info1)   

         ;;DOES THIS BROADCAST THE RESET CHANGES?
         ;;(plotActivations "reset flag" 190 220 reset-val 0.0 1.0)

         ;;SSS START HERE -- WHY WAS RES= 0 IN ALL CYCLES???
         ;;TEST FOR RESET
         (cond
          ;;Step 5. Test RES > vigilance, 
          ((> res (- 1.9  vigilance))  ;; me

           ;;Step 6: Update reset vars if reset
           ;;in case y-max-Index not found above?
           (setq y-max-Index (findLargestOutput))
           ;;was-reset-p is reset to nil on each testReset,, triggers graph display if setting right
           (setf *my-reset-broadcast t 
                 was-reset-p T)

           ;;Step 6.1: Set max-index reset to res (Grossberg way)  ;;was 1
           (setsymval 'reset (list y-max-Index) res)  ;;was 1)
           ;;not used??(setsymval 'reset-cntr (list max-Index) 80)

           ;;NO--want resets to stay thru pattern so can reject multiple y-outputs
           ;;Step 6.2 Set all non-max-Index reset values to 0
           #|(loop
            for j from 1 to  nOutputs
            do
            (when (not (= j  y-max-index))
              (setsymval 'reset (list j) 0))
            #| Watson way:       (setsymval ' -cntr (list j) (- (getsymval 'reset-cntr (list j)) 1))
      (when (< (getsymval 'reset-cntr (list j)) 0)
        (if (= (getsymval 'reset (list  j)) 1)  (setq skipReset t))     ;;was if T
        (setsymval 'reset (list j) 0))|#
            ;;end loop,
            )|#
            ;;end > res clause
            )
          ;; end t, cond
          ;;If not reset, set was-reset-p nil ??? reset reset vars to 0 too or elsewhere??
          (t  (setf was-reset-p nil )))
         ;;end when, n1 > clause
         ))
      ;;end overall cond
    ;;If not reset, set was-reset-p nil; reset reset vars to 0 too when begin new cycle.
     (t (setf was-reset-p nil )))

     ;;SSSX IF NOT RESET, make sure all non-reset  reset-vals = 0    
     ;;not needed, done above
#|     (loop
      for n from 1 to nOutputs
      do
      (let ((val (getsymval 'reset (list n)))
            )
        (unless  (and val  (>  val  0))
          (setsymval 'reset (list n) 0))
        ;;end let, loop
        ))|#

   ;;MONITORING TEXT
   (setf cycle-reset-info1 (format nil "~%===>>FROM TESTRESET;  NEW-CYCLE:learningCycleCounter = ~A;~% Step 1: IF (> n1 *min-input-criteria), n1= ~A  *min-input-criteria= ~A~%ALSO Cycle > 1;~%Step 3: IF (> y-max-value *reset-y-criteria),   y-max-value= ~A (at y-max-index= ~A)  *reset-y-criteria= ~A;   n1= ~A;  initial was-reset-p= ~A;  r  l2norm-p= ~A~%; [IF Step 3 = NIL, res= 0]~%Step 5:  res=(* *vigilence-multiplier  l2norm-r)= ~A, *vigilence-multiplier= ~A,  l2norm-r= ~A;  skipReset= ~A  ~% ** RESET TEST: (> res (- 1.9 vigilance))= ~A, res= ~A  (- 1.9 vigilance)= ~A, vigilance= ~A;"learningCycleCounter n1 *min-input-criteria  y-max-value y-max-index *reset-y-criteria  n1 was-reset-p  l2norm-p    res *vigilence-multiplier  l2norm-r  skipReset (> res (- 1.9 vigilance))  res  (- 1.9 vigilance) vigilance))     
    (setf cycle-reset-info2 (format nil "UPDATE?  *my-reset-broadcast= ~A    max-Index ~A~%  skipReset= ~A  updated was-reset-p= ~A; === end TESTRESET ===="  *my-reset-broadcast  y-max-index   skipReset was-reset-p))
    (setf  cycle-reset-info (format nil "~A~%~A" cycle-reset-info1 cycle-reset-info2))   
    (if (> *print-detail 0)
        (afout 'out (format nil "~A" cycle-reset-info))
      (afout  'reset-info (format nil "~A" cycle-reset-info)))

     (setf  *art-data-text (append *art-data-text (list (format nil "~A"  cycle-reset-info))))
    (setq skipReset nil);; temporary
  
    ;;ADD INFO TO *reset-values-list and *reset-vals RRR
    #|FIX LATER (setf  *reset-values-list (append *reset-values-list (list (format nil "In testReset, n1= ~A  skipReset= ~A  learningCycleCounter = ~A~% *y-output= ~A res= ~A *reset-val= ~A was-reset-p= ~A~%*reset= ~A" n1  skipReset  learningCycleCounter *y-output res *reset-val was-reset-p *reset)))
                       ;;next 2 follow format of other cells
                       reset-val0 reset-val
                       *reset-val '((reset-val0))
                       ;;creates a list of just reset values for making graph of ALL reset values
                       ;;no puts in bad vals  *reset-vals (append *reset-vals (list reset-val0))
                       )|#

    ;;end *let, testReset
    ))



;zzzz;
; ZEROACTIVATIONS:
;;
;;ddd
(defun zeroActivations (nInputs nOutputs &key
                        (1-dim-nInputs-sym-list 
                             '((X-Activity  0) (V  0) (R  0) (U  0) (Q  0) (P  0) (W  0)))
                            (1-dim-nOutputs-sym-list 
                             '((Y-Output  0)(Temp  .01) (reset-Val 0) (reset 0) 
                               (reset-cntr 0) (Temp2  .01))))  
  (let
      ((val)
       (sym)
       )
  ;;FOR 1-dim n-inputs variables 
  (loop
   for sym-val in 1-dim-nInputs-sym-list
   with val
   with sym
   do
   (setf sym (first sym-val)
         val (second sym-val))
  (loop
   for i from 1 to nInputs
   do
    (setsymval sym (list i) val)
    ;;(afout 'out (format nil  "~A ~a=> ~a "  sym  i (getsymval sym (list i))))
    ;;end dotimes, loop
    ))

  ;;FOR 1-dim n-outputs variables
  (loop
   for sym-val in 1-dim-nOutputs-sym-list
   with sym
   with val
   do
 (setf sym (first sym-val)
         val (second sym-val))
  (loop
   for j from 1 to nOutputs
   do
    (setsymval sym (list j) val)
    ;;(afout 'out (format nil  "~A ~a=> ~a "  sym  j (getsymval sym (list j))))
    ;;end loop, loop
    ))
  (setf  *art-data-text (list (format nil "~A~%In zeroActivations: RESETTING ~A~%~A" *art-data-text 1-dim-nInputs-sym-list  1-dim-nOutputs-sym-list)))
  ;;end let, zeroActivations
  ))
;;  (initNetwork 5 3)
;;  (zeroActivations 9 5)


;;OLD
#| (defun zeroActivations (nInputs nOutputs)
     "In ART2.lisp, sets activation levels to zero."
     (dotimes (i nInputs)
       (afout 'out (format nil  "setting aref w-p ~a => 0 " i))
       ;;is this needed??  (setsymval 'w i) 0.0) 
       (setsymval 'x-activity (list i) 0.0)
       (setsymval 'v (list i) 0.0)
       (setsymval 'r (list i) 0.0)
       (setsymval 'u (list i) 0.0)
       (setsymval 'q (list i) 0.0)
       (setsymval 'p (list i) 0.0))
     (dotimes (i nOutputs)
       (setsymval 'y-output (list i) 0)
       (afout 'out (format nil "setting aref ~a ~a => 0 " y-output i))
       (setsymval 'reset (list i) 0)
       (setsymval 'reset-cntr (list i) 0)))
      |#


;;OLD-ARRAY METHOD
#|(defun zeroActivations (nInputs nOutputs)
    "In ART2.lisp, sets activation levels to zero."
    (dotimes (i nInputs)
      (afout 'out (format nil  "setting aref w-p ~a => 0 " i))
      ;;is this needed??  (setf (aref *w i) 0.0) 
      (setf (aref *x-activity i) 0.0)
      (setf (aref *v i) 0.0)
      (setf (aref *r i) 0.0)
      (setf (aref *u i) 0.0)
      (setf (aref *q i) 0.0)
      (setf (aref *p i) 0.0))
    (dotimes (i nOutputs)
      (setf (aref *y-output i) 0)
      (afout 'out (format nil "setting aref ~a ~a => 0 " *y-output i))
      (setf (aref *reset i) 0)
      (setf (aref *reset-cntr i) 0)))|#

;;SETPATTERN
; Set up a learning pattern in the input neurons:
;;ddd
(defun setPattern (pl nInputs nOutputs)
  "In ART2.lisp, Sets up a learning pattern in the input neurons"
  (let
      ((len (length pl))
       )
  (cond
   ((not (equal len nInputs))
    (print (list "Error in setPattern input:" pl))
      (setq learningCycleCounter 0)
      (zeroActivations nInputs nOutputs))
   (t
      (loop
       for i from 1 to  len
       do
        (setsymval 'input (list i) (+ (pop pl) (frandom -0.08 0.08)))
      ;;end loop, t, cond
      )))
  ;;end let,setPattern
  ))


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
        (afout 'out (format nil "In learnPatterns, testPatterns= ~a " testPatterns))

        (setq  learningcyclecounter 0);;2013-11 added global var here

        (dolist (p testPatterns)
          (afout 'out (format nil "Inputs= ~A" p))
          (setPattern p nInputs nOutputs)
          (dotimes (i numCycles)
            (setq learningCycleCounter (1+ learningCycleCounter))
            (OneCycle ninputs noutputs)  ;; temp quoted out i j bec of error
            ;;(updateScreen))
            (setq *learned-categories*
                  (cons (list p (findLargestOutput nOutputs))
                        *learned-categories*))
            )
          (afout 'out (format nil "At END of learnPatterns, *learned-categories* = ~s "
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
           *initial-x-pix 40 *incr-x 40
        ; Global variable to remember input patterns and
        ; their associated output category code for plotting
        ; by function ART2-Postprocess:
           *learned-categories* nil)

  ;;run ART2
  (ART2-manager *num-cycles *ninputs *noutputs *test-patterns
                                *initial-x-pix *incr-x)
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




;;ARRAY VERSION
#|(defun F1STMcycle (nInputs nOutputs &aux i j sum norm max1 max2)
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
  )|#

;;ARRAY VERSION
#|(defun testReset (nInputs nOutputs)  ;;was, replace in let* &aux (res 0.0) (norm1 0.0) (norm2 0.0)  (n1 (+ (L2NORM p nInputs) e)) n2 temp) uses &aux instead of let??
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
|#
