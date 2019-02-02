;;;********************************** MyART2.lisp ************************************
;;;
;;;modifications of Mark Watson's ART2.LSP
;;;


;;
 ;        Adaptive Resonance 2 Model in Common Lisp.
 ;        Copyright 1990 by Mark Watson.
 ;
 ;        Based on the Adaptive Resonance Theory
 ;        by Gail Carpenter and Stephen Grossberg.
;;

;;TO RUN THIS ART2 PROJECT (From M Watson, Common LISP Modules CH-7 ART2: pp 77ff;
;; TO RUN INSTRUCTIONS p94: (In Listener Window at prompt, type:
;; 1-(ART2-init *nInputs *nOutputs *testPatterns) then 2- (ART2 5 3) or 
;; 2-(art2 *nInputs *nOutputs :plot? t :test-cycles 10 :xSize 700 :ySize 600) 
;;  then if wanted? 3- (ART2-postprocess)

;; Define a separate name space package for this software module:
 ;;

;;(make-package :art2)   ;; optional
;;(export '(ART2 ART2-postprocess ART2-init)  :cg-user)  ;; optional

(defparameter *print-detail 1)
(defparameter *print-details nil)

;;NOTE-the number of number of input patterns = *learnPatterns-cycles X nTestPatterns.
(defparameter *test-cycles 10)

(defparameter *nInputs 5)
(defparameter *nOutputs 3)

; Define some test patterns:

;;original
(setf nTestPatterns 15
  testPatterns15
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

(setf nTestPatterns 2
  testPatterns2
      '(
        (0.9 0.9 0.9 0.1 0.1)
        (0.7 0.9 0.8 0.1 0.2)
        ))
(setf nTestPatterns 3
  testPatterns3
      '(
        (0.9 0.0 0.0 0.0 0.9)
        (0.0 0.9 0.9 0.9 0.1)
        (0.5 0.5 0.5 0.5 0.5)
        ))
(setf nTestPatterns 4
  testPatterns4
      '(
        (0.9 0.0 0.0 0.0 0.9)
        (0.0 0.9 0.1 0.9 0.1)
        (0.5 0.5 0.5 0.5 0.5)
        (0.0 0.9 0.9 0.0 0.0)
        ))

(setf nTestPatterns 6
  testPatterns6
      '(
        (0.0 0.4 0.9 0.4 0.0)
        (1.0 0.3 0.0 0.3 1.0)
        (0.75 0.5 0.02 0.05 0.07)
        (0.02 0.41 0.91 0.36 0.08)
        (1.1 0.12 0.0 0.3 1.0)
        (0.0 0.2 0.9 0.4 0.0)
        ))
      

;;must be set for global variable
(defparameter *testPatterns testPatterns15)
;; 1-(ART2-init *nInputs *nOutputs *testPatterns) then 2- (ART2 5 3) or 
;; 2-(art2 *nInputs *nOutputs :plot? t :test-cycles 10 :xSize 700 :ySize 600) 
;;  then if wanted? 3- (ART2-postprocess)

(defun ART2-init (numInputs numOutputs testPatterns)
  ; Check for specified training patterns:
  ; me added bec of errors
  (setf NumInputs numInputs NumOutputs numOutputs)
  (format *standard-output* "NumInputs= ~a NumOutputs= ~% " NumInputs NumOutputs)
  ; Make sure the number of input neurons agrees with
  ; the size of the training patterns:
  (if (not (equal (length (car testPatterns)) numInputs))
      (print
       (list
        "ERROR: bad input to ART2-init. numInputs should have been"
        (length (car testPatterns))))
    ;else
    ; Specified number of input neurons agrees with
    ; the size of the default training patterns defined
    ; in this package; proceed with defining network data:
    (progn
      ; Define the network size:
      (let ((nInputs numInputs)
            (nOutputs numOutputs))
        ; Array storage allocation:
        ; added
        (format t "nInputs= ~a ~%" nInputs)
        (setq input (make-array (list nInputs)));; nTestPatterns)))  ;; inputs (n=5)
        (setq w (make-array (list nInputs)));; nTestPatterns)))      ;; w weights (n)
        (format t "w= ~A~%" w)
        (setq x (make-array (list nInputs)));; nTestPatterns)))      ;;x activity level (n)
        (setq v (make-array (list nInputs)));; nTestPatterns)))
        (setq r (make-array (list nInputs)));; nTestPatterns)))
        (setq u (make-array (list nInputs)));; nTestPatterns)))
        (setq q (make-array (list nInputs)));; nTestPatterns)))
        (setq p (make-array (list nInputs)));; nTestPatterns)))
        (setq temp (make-array (list nInputs)));; nTestPatterns)))
        (setq resetVal (make-array (list 1)))
        (setq y (make-array (list nOutputs)));; nTestPatterns)))     ;;y outputs (3??)
        ;;me
        (format t "nOutputs= ~a ~%" nOutputs)
        (setq reset (make-array (list nOutputs)));; nTestPatterns)))
        (setq resetCounter (make-array (list nOutputs)));; nTestPatterns)))
        (setq nCategories (make-array (list nOutputs)));; nTestPatterns)))   ;;categories (3)
        (setq temp2 (make-array (list nOutputs)));; nTestPatterns)))
        (setq wUp (make-array (list nInputs nOutputs)));; nTestPatterns)))    ;;wUp   (??)
        (setq wDown (make-array (list nOutputs nInputs)));; nTestPatterns)))   ;;wDown  (??)
        ; Global variable to remember input patterns and
        ; their associated output category code for plotting
        ; by function ART2-Postprocess:
        (setq *learned-categories* nil)))))    ;;learned categories

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
(format t "a= ~a; b= ~a; c= ~a; d= ~a; e= ~a; theta= ~a; vigilance= ~a; alpha= ~a; ~%" a b c d e theta vigilance alpha)

; Floating point  random numbers:

(defun frandom (low high)
  (let ((range (- high low)))
    (+ (* (/ (random 1000) 1000.0) range) low)))

(defun findLargestOutput (&aux (maxIndex 0) (mVal (aref y 0)))
  (dotimes (j *nOutputs)
    (if (and
         (> (aref y j) mVal)
         (not (aref reset j)))
      (setq mVal (aref y j)
          maxIndex j)))
  (if (= *print-detail 2) (format  t "maxIndex= ~a ~%" maxIndex))
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
  (format t "in F1STMcycle, *nInputs= ~s ~%" *nInputs)
  (dotimes (i *nInputs)
    (setq sum 0.0)
    (if (= *print-detail 2) (format  t "FOR i= ~s ~%" i))
    (if (= *print-detail 2) (format  t "From inside dotimes(i *nInputs), i= ~s, *nOutputs= ~s ~%" i *nOutputs))
    (dotimes (j *nOutputs)
    (if (= *print-detail 2) (format  t "FOR j= ~s ~%" j))
      (if (= *print-detail 2) (format  t "From inside dotimes(j *nOutputs), i= ~s, *nOutputs= ~s ~%" j *nOutputs))
      (setq sum (+ sum (* (g j) (aref wDown j i)))))
    (if (= *print-detail 2) (format  t "sum= ~s; a= ~s ~%" sum a))
    (setf (aref p i) (+ (aref u i) sum))a
    (if (= *print-detail 2) (format  t "(aref p i)= ~s ~%" (aref p i)))
    )
  ; Update q using eq. 5
  (setq norm (+ (L2NORM p *nInputs) e))
  (dotimes (i *nInputs)
    (setf (aref q i) (/ (aref p i) norm))
    )
  ; Update u using eq. 6:
  (setq norm (L2NORM v *nInputs))
  (dotimes (i *nInputs)
      ;;(break "dotimes (i *nInputs) in F1STMcycle")
    (if (= *print-detail 2) (format  t "FOR i= ~s ~%" i))
    (if (= *print-detail 2) (format  t "(aref u i)= ~s; norm= ~s ~%" (aref u i) norm))
    (if (= *print-detail 2) (format  t "(aref v i)= ~s; norm= ~s ~%" (aref v i) norm))
    (setf (aref u i) (/ (aref v i) norm)))
  ; Update v using eq. 7:
  (dotimes (i *nInputs)
    (setf (aref v i) (sigmoid (+ (aref x i) (* b (sigmoid (aref q i))))))
    (if (= *print-detail 2) (format  t "FOR i= ~s ~%" i))
    (if (= *print-detail 2) (format  t "(aref x i)= ~s; b= ~s ~%" (aref x i) b))
    (if (= *print-detail 2) (format  t "(aref q i)= ~s; b= ~s ~%" (aref q i) b))
    )
  ; Update w using eq. 8:
  (dotimes (i *nInputs)
    (setf (aref w i) (* alpha (+ (aref input i) (* a (sigmoid (aref u i))))))
    (if (= *print-detail 2) (format  t "FOR i= ~s ~%" i))
    (if (= *print-detail 2) (format  t "(aref w i)= ~s; alpha= ~s ~%" (aref w i) alpha))
    (if (= *print-detail 2) (format  t "(aref u i)= ~s; alpha= ~s ~%" (aref u i) alpha))
    (if (= *print-detail 2) (format  t "(aref input i)= ~s; ~%" (aref input i) ))
    )
  ; Update x using eq. 9:
  (setq norm (+ (L2NORM w *nInputs) e))
  (dotimes (i *nInputs)
    (setf (aref x i) (/ (aref w i) norm))
    (if (= *print-detail 2) (format  t "FOR i= ~s ~%" i))
    (if (= *print-detail 2) (format  t "(aref x i)= ~s; norm= ~s ~%" (aref x i) norm))
    (if (= *print-detail 2) (format  t "(aref u i)= ~s; alpha= ~s ~%" (aref w i) norm))
    )

  ; Calculate reset r from eq. 20:
  (setq max1 -1000.0 max2 -1000.0)
  (dotimes (j *nInputs)
    (progn
      (if (< max1 (aref u j)) (setq max1 (aref u j)))
      (if (< max2 (aref p j)) (setq max2 (aref p j)))
      )
    (if (= *print-detail 2) (format  t "FOR j= ~s ~%" j))
    (if (= *print-detail 2) (format  t "(aref U j)= ~s; max1= ~s ~%" (aref u j) max1))
    (if (= *print-detail 2) (format  t "(aref P j)= ~s; max2= ~s ~%" (aref p j) max2))
    )
  (setq max1 (+ max1 0.001))
  (setq max2 (+ max2 0.001))
  (dotimes (i *nInputs)
    (setf
     (aref r i)
     (- (/ (aref u i) max1) (/ (aref p i) max2)))
    (if (= *print-detail 2) (format  t "FOR i= ~s ~%" i))
    (if (= *print-detail 2) (format  t "(aref u i)= ~s; max1= ~s ~%" (aref u i) max1))
    (if (= *print-detail 2) (format  t "(aref p i)= ~s; max2= ~s ~%" (aref p i) max2))
    (if (= *print-detail 2) (format  t "r=~a i=~a; value= ~a ~%" r i (aref r i)))
    )
  )

; Update F2 STM storage:

(defun F2STMcycle (&aux i j sum)
  (dotimes (j *nOutputs)
    (progn
      (setq sum 0.0)
      (dotimes (i *nInputs)
        (setq sum (+ sum (* (aref p i) (aref wUp i j)))))
      (setf (aref y j) sum)
      (if (aref reset j) (setf (aref y j) -0.1))
       ;;me
      (format t  "y=~a j=~a; value= ~a ~%" y j (aref y j))
      (aref y j)      
      )    
    )  
  )

; Update weights:

(defun updateWeights (&aux i (j (findLargestOutput)))
  (if (> (g j) 0.02)
    (dotimes (i *nInputs)
      (setf
       (aref wDown j i)
       (+ (aref wDown j i)
          (*
           downLR
           d
           (- (aref p i) (aref wDown j i)))))
       ;;me
       (format t  "j=~a i=~a; value= ~a ~%" j i (aref wDown j i))
      (setf
       (aref wUp i j)
       (+
        (aref wUp i j)
        (*
         upLR
         d
         (- (aref p i) (aref wUp i j)))))
       ;;me
      (format t  "i=~a j=~a; value= ~a ~%" i j (aref wUp i j))
      
      )))

; Competitive learning at slab F2:

(defun competitiveF2 (&aux i (jj (findLargestOutput)))
  (if (> (aref y jj) resetThreshold)
    (dotimes (i *nOutputs)
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
                       (n1 (+ (L2NORM p *nInputs) e)) n2 temp)
  (if (and
       (> n1 0.2)
       (not skipReset))
    (if (> learningCycleCounter 1)
      (if (> (aref y (findLargestOutput)) 0.25)
        (setq res (* 3.0 (L2NORM r *nInputs))))  ; was 3.0
      (setq skipReset nil)))
  (setf (aref resetVal 0) res)
  ;;my 
  ;;(plotActivations "reset flag" 190 220 resetVal 0.0 1.0)
  (if (> res (- 1.9 vigilance))  ;; me
    (progn
      (print (list "Vigilance reset =" res "  Learning cycle ="
                   learningCycleCounter))
      (setq maxIndex (findLargestOutput))
      (setf (aref reset maxIndex) 1)
      (setf (aref resetCounter maxIndex) 80))
    (dotimes (i *nOutputs)
      (setf (aref resetCounter i) (- (aref resetCounter i) 1))
      (if (< (aref resetCounter i) 0)
        (progn
          (if (aref reset i)  (setq skipReset t))
          (setf (aref reset i) nil)))))
  (setq skipReset nil)) ;; temporary

; Zero activations:

(defun zeroActivations ()
  (dotimes (i *nInputs)
    (format t  "setting aref w-p ~a => 0 ~%" i)
    (setf (aref w i) 0.0)
    (setf (aref x i) 0.0)
    (setf (aref v i) 0.0)
    (setf (aref r i) 0.0)
    (setf (aref u i) 0.0)
    (setf (aref q i) 0.0)
    (setf (aref p i) 0.0))
  (dotimes (i *nOutputs)
    (setf (aref y i) 0)
   (format t "setting aref ~a ~a => 0 ~%" y i)
    (setf (aref reset i) 0)
    (setf (aref resetCounter i) 0)))

; Set up a learning pattern in the input neurons:

(defun setPattern (pl &aux (len (length pl)))
  (if (not (equal len *nInputs))
    (print (list "Error in setPattern input:" pl))
    (progn
      (setq learningCycleCounter 0)
      (zeroActivations)
      (dotimes (i len)
        (setf (aref input i) (+ (pop pl) (frandom -0.08 0.08)))))))

; Initialize the network:

(defun initNetwork ()
  (zeroActivations)
  (dotimes (j *nOutputs)
    (progn
      (dotimes (i *nInputs)
        (setf
         (aref wUp i j) (frandom 0.05 0.1)
         (aref wDown j i) (frandom 0.01 0.03))
        (format t "wUp ~a ~a => ~a ~%" i j (aref wUp i j))
        (format t  "wUp ~a ~a => ~a ~%" j i (aref wDown j i))        
        )
      (setf (aref nCategories j) 0))))


; Cycle through all training patterns once:
;;;lll
(defun learnPatterns (&optional (numCycles 10)) ;; Watson uses num = 50 when calls in original (ART2) was this orig args? (i j) 
  (let ((patternNum 0)
        )
    (dolist (p *testPatterns)
      (incf patternNum)
      (print (list "Inputs:" p))
      (format t "In learnPatterns, testPatterns= ~a ~%" *testPatterns)
      (setPattern p)
      (dotimes (i numCycles)
        (setq learningCycleCounter (1+ learningCycleCounter))
        (OneCycle)  ;; temp quoted out i j bec of error
        ;;(updateScreen))        
        ;;(cons (list '(.2 .4 .5) 2 (list 3)) '(L1  L2 L3 L4))
        (setq *learned-categories*
              (cons (list p (findLargestOutput) (list patternNum)) ;;I added patternNum
                *learned-categories*)))
      (format t "At END of learnPatterns, *learned-categories* = ~s ~%"
        *learned-categories*)
      )
    )
  *learned-categories*
  )


 ;;Main test program:
(defun ART2 (nInputs nOutputs &key (plot? nil) (xSize 700) (ySize 600)(test-cycles 10))  
  "ART2 in MyART2.cl, is main function to run ART2 demo"
  
  (initNetwork)
  (setf *learned-categories* 
    (learnPatterns test-cycles))
  
  ;;my addition for input to init-plot, *ART2-values* used as input in init-plot
  (setf *ART2-values*
    (make-chart-list nInputs *learned-categories*))
  ;;(format t "*ART2-values* = ~A ~%" *ART2-values*)
  (if plot?
      (init-plot :ART2-Inputs-Output *ART2-values*
        "ART2 Last Learned Categories" xSize ySize)  
    )
  )

;;(print-list *learned-categories*)

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

|#