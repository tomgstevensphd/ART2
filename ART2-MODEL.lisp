;;********************* ART-MODEL.lisp ***********************
;;
;;
#|
From Mark Watson, Common LISP Modules

FOR input= i and output=j

BASED UPON Carpenter and Grossberg, ART2: p7

      F2 reset
    /p  /-----------------> [ F2      {yj     }              (@= LTM, specific node, 
      ^                                    zij @     |                  O= non-specific signal,
      |                                           |       |
     O                                          |       |
      |                                           |     @ zji
   {ri }<---------cpi--------------{pi    }---------------------------->{qi  } ;;F2 buffering vars
   {   }<-----------O--------------{       }--------------O------------>{     }
   {   }< ---- from ui                   ^                                                   |
   {   }<--O-from ui                    |                                                   |
                                                 ui                                                bf(qi)
                                                  |                                                   |
                                                  |                                                  \/
                     <-------to ri------{ui   }<-----------------------------{vi   };;Main F1 activity vars
                     <---O--to ri------{      }<--------------O-------------{      }
                                                  |                                                  ^
                                                  |                                                  |
                                                aui                                              f(xi)
                                                  |                                                  |
                                                 \/                                                  |
                                               {wi  }------------------------------->{xi  };;Input buffering vars
                                               {      }---------------O-------------->{     }
                                                 ^
                                                 |
                                             INPUT


;;NOTE: V-ACTIVITY is more like X-activity in ART1

;;F1 INPUT, V-Activity RELATED FUNCTIONS, VARIABLES

;;FINAL signal from F1 at  cell i? [also see pi = in Eq 15 below]
pi = (+ ui  SUMj (* (g yj) wdnji))  ;  ui = net v-activity signal from input    (Eq 4)
  ;;where SUMj (* (g yj) wdnji)= max-y expect-signal from all y-outputs
  ;;where g produces  output constant=d (if largest non-reset y index and y-output >  *onCenterThreshold, Otherwise = .00001) OR g 
   ;;pi = FINAL output from F1 at  cell i 
;;ME-NEW:  Reset all yjs to 0 for each new input; otherwise causes expectation to be from the previous cycle--not starting from neutral on a new input.

;;NORMALIZED FINAL F1 cell i signal.  q is q-activity
qi = (/ pi (+ e   L2norm-p))  ;; qi = NORMALIZED FINAL F1 cell i signal?       (Eq5)
          ;;e = 0.04?

;;ui is signal output of v-activity, used for MATCH betw F1 v-activity and F2 p signal
   to determine if get F2 reset or not.
ui = (/ vi (+ e  L2norm-v))  ;;ui=NET Normalized V-Activity SIGNAL?                                (Eq6)
          ;; based on normalized STM signal?

;;V-Activity SIGNAL; v is v-activity
vi = (+ f(xi)  (* b f(qi)))  = (+ x (* b x))  OR  0           (Eq7)
          ;xi=x-activity-i, constant b= 0.2, f(qi) = see f(x) below and qi above
          ;X-ACTIVITY MUST BE > THETA=0.3 to not be set to 0

;;SIGNAL FROM INPUT i?
wi = (+ Ii (* a ui)) ;;wi = SIGNAL FROM INPUTI?,                                  (Eq8)
          ;Ii = Input i, constant a=0.5, ui= gain?

;;STM X is x-activity at cell i based upon signal from input i (wi)
xi = (/ wi (+ e  L2norm-w))  ;;xi = X-ACTIVITY-I  STM                                  (Eq9)


;;SIGMOID FUNCTION
;;piecewise linear [using this one?]
f(x) = 0  if (<x theta)  ;;constant theta = 0.3                                              (Eq11)
f(x) = x  if (>= x theta)
OR continuously differentiable
f(x) = (/ (* 2 theta (* x x)) (+ (* x x) (* theta theta))) ;;;( if (<= x theta)     (Eq10?)
f(x) = x   if (>= x theta)   ;; theta = 0.3


;;F2 Y-OUTPUT FUNCTION
yi = SUMj (* pi wupij) ;;Y-OUTPUT   ;;me shouldn't yi be yj??

;;F2 CONTRAST ENHANCEMENT via F2 competition, Tj
;;Tj = summed, filtered F1-->F2 input to the jth F2 node:
Tj = SUMi (* pi zij)  ;;(j = M + 1...N), M= max i number                         (Eq12)

F2 MAKES A CHOICE if Jth F2 node becomes maximally active,
while all other nodes are inhibited, when [see function g below]
Tj = max{Tj:  j= M+1...N}                                                                         (Eq13)

;;F2 RESET TEST (eg.)
 Step 1: IF (> n1 *min-input-criteria), n1= 1.6471107  *min-input-criteria= 0.2
ALSO Cycle > 1;
Step 3: IF (> y-max-value *reset-y-criteria),   y-max-value= 2.308497 (at y-max-index= 2)  *reset-y-criteria= 0.2;   n1= 1.6471107;  initial was-reset-p= NIL;  r  l2norm-p= 1.6071107
; [IF Step 3 = NIL, res= 0]
;; ==> Note: Critical variable:  L2NORM-R tests U vs P  overall MISMATCH.
Step 5:  res=(* *vigilence-multiplier  l2norm-r)= 0.030890122, *vigilence-multiplier= 3.0, l2norm-r= 0.010296707;  skipReset= NIL  
 ** RESET TEST: (> res (- 1.9 vigilance))= NIL, res= 0.030890122  (- 1.9 vigilance)= 0.79999996, vigilance= 1.1;
;;
;;F2 RESET may be carried out in several ways, one being a GATED DIPOLE field in F2.
When a NONSPECIFIC arousal input reaches an F2 gated dipole, 
 nodes are INHIBITED OR RESET IN PROPORTION to 
 their former F2 STM activity levels. Endures until F1 input shuts off.

;;WHEN F2 MAKES A CHOICE (via Wdn weights to F1, only largest "fires"?)
g(yj) = d  ;if Tj = maxi Tj  & the jth F2 node not reset                               (Eq14)
g(yj) = 0  ;otherwise

Eq 14 implies that Eq 4 reduces to:
pi = ui               ;if F2 is inactive                                                                 (Eq15)
pi = ui + dzji     ;if the Jth F2 node is active


;;UPDATING LTM MEMORY TRACES
;;Note: As weights get larger (- pi weight) gets smaller. 
;;
;;LTM, long-term memory traces
;;priming, expectancy LTM from F2 to F1
wdnji =  SUMi  (* downLR  d  (my-floor (- pi  wdnji) *p-wdn-floor)) ;;constant d= 0.4; downLR=0.4; *p-wdn-floor= -0.2

;;LTM weights facilitating F1 firing learned F2 feature cell(s)
wupij =  SUMi  (*  upLR  d  (my-floor (- pi  wupij) *p-wup-floor))  ;;constant d= 0.4; upLR= 0.5; *p-wup-floor= -0.2

;;NOTATION (not exactly correct lisp)
SUMi = (loop for i from 1 to N do (setf sum (+ sum vari)))

;;L2NORM MEANING
L2norm = (SQRT (SUMi (* var var)); normally  value = (/ var  L2norm-var)
g =   If index = max y-output index AND that y NOT reset AND the y-output for that index > *onCenterThreshold, then RETURNS constant d, 
 otherwise returns 0.00001.
;;Note: To get an idea of how values are affected
;;  (my-normalize '(1 1 1 1.0)) = 2.0;  sum= 4;  1/2.0 = 0.5
;;  (my-normalize '(.1 .1 .1 .1)) = 0.2  sum= 0.4
;;  (my-normalize '(1 2 3 4 5)) = 7.4161983  sum=15;  1/7.4=0.14,, 5/7.4=0.68
;;  (my-normalize '(11 12 13 14)) = 25.0998  sum=50;  11/25=0.44, 14/25=0.56
;;  (my-normalize '(1 -2  3 -4 5)) = 7.4161983  sum=3; -2/7.4= -0.27
;;  (my-normalize '(0.1 0.2 0.3 0.4 0.5)) = 0.7416199  sum=1.5
;;  (my-normalize '(0.01 0.02 0.03 0.04 0.05)) = 0.074161984
;;  (my-normalize '(1 2 3 4 1000)) = 1000.015, sum=1011;  1/1000= 0.001,1000/1000.02 = 0.999 
;;  (my-normalize '(1 1 1  .0001)) = 1.7320508  sum=3.0001
;; (load 


QUESTIONS:
1. What is learning-rate?
2.
|#


;;XXX ---------------------  DATA RESULTS OBSERVATIONS --------------------
;;
#|
* Increasing values of input vars from 1 to 100 with same patterns had NO NOTICEABLE AFFECT on V-activites,  Wups, Y-outputs, or anything else I noticed.=[Pattern '(((0 0 0 100 0 0 0 100 100) "D") ((0 0 0 100 0 0 0 0 0) "A") ((0 0 0 100  100 100 0 0 0) "X")) vs *pattern1=(((0 0 0 1 0 0 0 1 1) "D") ((0 0 0 1 0 0 0 0 0) "A") ((0 0 0 1 1 1 0 0 0) "X")) ]

* In above D,A,X patterns, first runs:
1. For D: Quickly learned D, y=1.
2. For A: Generalized to A, y=1, no resets, etc.
3. For X: Unstable, undecided, switched betw y=1, 5 every other cycle. If reset on cycle, then chose y=5, if not, chose y=1.

*** WHEN MINIMIZED, GRAPHS ALL REVERT TO THE SAME GRAPH!!!


|#