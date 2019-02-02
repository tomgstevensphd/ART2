;;************************ config-ART2.lisp ****************************
;;
;;NOTES:
;; 1. LOAD-ART2-FILES FROM ART2.LISP
;; 2. Use (FOUT OUT0)  to output INITIALIZATION VALUES
;;
;; CELL/SYMBOL FORMAT ------------------------------------------------------------
;; (NOTE: If unbind-global-vars-p = NIL, then * global vars bound, otherwise only local variables by same name returned in values by fun make-new-dim-symbol-types)
;;
;;  CELL NAME LISTS:  EG: new-symbol-type-list= (INPUT WUP)
;;*new-symbols-type-list-of-lists= (((INPUTC1... INPUTC7)) ((WUPC1F1...WUPC2F3) (WUPC3F1...WUPC3F3) (WUPC4F1 ...WUPC4F3)))
;;*new-symbol-type-spec-list-of-lists= (((("inputC1" (7 1 1 "C" "")) ...("inputC7" (7 7 1 "C" "")))) ((("wupC1F1" (3 1 1 "F" "")) ... ("wupC1F3" (3 3 1 "F" ""))) (("wupC2F1" (3 1 1 "F" "")) ... ("wupC2F3" (3 3 1 "F" ""))) (("wupC3F1" (3 1 1 "F" "")) ... ("wupC3F3" (3 3 1 "F" ""))) (("wupC4F1" (3 1 1 "F" "")) ... ("wupC4F3" (3 3 1 "F" "")))))
;;*new-root-list= ("input" "wup")
;;*new-symbol-type-symbol-string-list-of-lists=  ((("inputC1"... "inputC7")) (("wupC1F1" "wupC1F2" "wupC1F3") ("wupC2F1" "wupC2F2" "wupC2F3") ("wupC3F1" "wupC3F2" "wupC3F3") ("wupC4F1" "wupC4F2" "wupC4F3"))) 
;; INPUTC2  =  ("inputC2" (7 2 1 "C" ""))
;; WUPC3F3 =  ("wupC3F3" (3 3 1 "F" ""))
;; *make value 3rd ("inputC2" (7 2 1 "C" "") value)
;;
;; end format ------------------------------------------------------------------------------------

;;LOAD-ART2-FILES
;;
;;ddd
(defun load-ART2-files ()
;;these should load in .lispworks
 ;; (load "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-Arrays.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-function-plotter.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-files.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-debug.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-tstring.lisp") 
  (load "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-capi.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\SHAQ\\U-data-functions.lisp")
;;  (load "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-photo-info.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-sequences.lisp")
;;  (load "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-photos.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\ART-LW\\U-art-math-utilities.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\ART-LW\\ART2-multipane-interface.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\ART-LW\\U-ART.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\ART-LW\\ART2-data-analysis.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\ART-LW\\ART2-inputs.lisp")
  )

 (load-art2-files)



;;USING FIND-SYMBOL MUST TYPE ALL CAPS FOR SYMBOL NAME
;;(find-symbol "ART2-MULTIPANE-INTERFACE" 'common-lisp-user)=> WORKS
;;LOAD ART2 FILES (unless already loaded)
(unless (or (and (boundp '*load-art2-files-p) (null *load-art2-files-p))
            (find-symbol "ART2-MULTIPANE-INTERFACE" 'common-lisp-user))
  (load-ART2-files))

;;Sets curser and other Editor variables
(my-config-editor-after-start)
;
;; LATER?? Define a separate name space package for this software module??
;;(make-package :art2)   ;; optional
;;(export '(ART2 ART2-postprocess ART2-init)  :cg-user)  ;; optional


;;------------------------- SOME PARAMETERS (OTHERS LATER) ----------
(defparameter *print-detail  0  "*print-detail= 1 less than = 2, w/in do loops 3." )
(defparameter *fout-reset-info T "Outputs a fout window with all reset info")
;;done below (defparameter *learnPatterns-cycles 10)
;;done below (setf  *nInputs 5 *nOutputs 3)


;; ART2 FORMULA CALCULATION PARAMETERS ---------------
; Neuron size for plots:
(setq PEsize 18)
(setq PEsizem1 (- PEsize 1))
(setq PEsizep1 (+ PEsize 1))

; Model constants
(defparameter a 0.7 "LTM Weight parameter") ;;default= 0.5
(defparameter b 0.2 "v parameter")  ;;default= 0.2
(defparameter c  -1.0 "??" )   ;;default= -1.0
(defparameter d  0.6 "wUp and wdn parameter")  ;;default= 0.4
(defparameter e  0.04  "norm parameter?")  ;;default= 0.04
(defparameter theta 0.3 "f(x) Activation criteria")  ;;default= 0.3
(defparameter *vigilence-multiplier 2.0  "In res= (* *vigilence-multiplier l2-norm-r)") ;;default = 3.0
(defparameter vigilance  0.9 "Sigmoid degree of match criteria, max 1.00")  ;;default= 0.94
                       ;;higher number may increase final output
(defparameter alpha 1.0 "LTM weight w parameter")  ;;default= 1.0

(defparameter *onCenterThreshold 0.25 "Threshold for F2 min y activity for on-center, off-surround. Watson used term resetThreshold")  ;;default= 0.05, Watson used term resetThreshold  
(defparameter *min-input-criteria  0.20 "Min input for testing x r mismatch for F2 reset.")
;;not used (defparameter  *v-activity-reset-criteria 0.20 "Threshold for mismatch betw p  and u  thru wdn to cause reset of y")
(defparameter  *min-g  0.02  "Min value of  (g  j nOutputs) for updating wup")
(defparameter *reset-y-criteria 0.40 "y must be greater than this to cause on-center, off-") ;;;was 0.25)  
(defparameter  *show-last-reset-vals-n 20 "Number of reset-vals in graph")

;;for creating limits in INITIAL Art variable values
(defparameter upLR 0.5 "wUp parameter")  ;;default= 0.12
(defparameter downLR 0.4 "wdn parameter")  ;;default= 0.12
(defparameter *p-wdn-floor -0.2 "My-floor value for (- pi wdnji)")
(defparameter *p-wup-floor -0.2 "My-floor value for (- pi wupij)")
(defparameter *wUpInitLo 0.40 "random init low value")
(defparameter *wUpInitHi  0.50 "random init high value. Must be not too small-grossberg.")
(defparameter *wDnInitLo 0.20 "random init low value. Must be SMALL-grossberg.")
(defparameter *wDnInitHi 0.30"random init high value")
;;for creating limits in Art variables-during processing (with my-floor-ceiling)
(defparameter *wt-floor  0.0001) ;;for both wup and wdn
(defparameter *wt-ceiling 1.00)  ;;for both wup and wdn
(defparameter *x-activity-floor -100)
(defparameter *x-activity-ceiling 1000)
(defparameter *y-output-floor -100)
(defparameter *y-output-ceiling 1000)

;;ART DATA TEXT
(defparameter *make-art-data-text-p t "Make detailed data text for each cycle")
(defparameter *art-data-text nil "A list of  data text lists for each art cycle")

;;for initializing 1-dim values with zeroActivations function
(defparameter *1-dim-nInputs-sym-list  '((X-Activity  .01) (V  .01) (R  .01) (U  .01) (Q  .01) (P  .01) (W  .01)))
(defparameter *1-dim-nOutputs-sym-list  '((Y-Output  .01)(Temp  .01) (reset-Val 0) (reset nil) (reset-cntr 0) (Temp2  .01)))

;;FOR DRAWING GRAPHS ETC
(defparameter *make-art-graphs-p T "Must be T to create any kind of graph windows")
(defparameter *no-graph-p nil "Prevents any graph windows from being created")
(defparameter  *art-inst-counter 0 "Number of art graph window instances.")
(defparameter  *art-inst-name-root  "*art-inst-sym"  "Root name for making *art-inst-syms")
(defparameter  *art-inst-sym  nil  "New instance for each graph window, renewed for each runart")
(defparameter  *art-graph-win-counter 0 "Number of art graph window instances.")
(defparameter  *art-graph-win-root  "*art-graph-sym"  "Root name for making *art-inst-syms")
(defparameter  *art-graph-win-sym  nil  "New instance for each graph window, renewed for each runart")
(defparameter *graphing-symbols-list  '(input x-activity wUp wDn y-output reset-val))
(defparameter *graphing-sym-points-list  '(*input *x-activity *wUp *wDn *y-output  *reset-val))
(defparameter *num-cycles 10)  ;;was 20
(defparameter *graph-every-x-cycle 10)
(defparameter  *end-test-all-patterns-n 3 "Test all patterns n times at end")
(defparameter  *graph-end-tests-p T "If end-tests, then graph last end test pattern")
(defparameter *graph-every-x-test-cycle 3)
(defparameter *sort-wup-dim 2 "Graph wup points by dim-n (1 or 2)")
(defparameter *sort-wdn-dim 1  "Graph wdn points by dim-n (1 or 2)")
(defparameter *initial-x-pix 0)
(defparameter *initial-y-pix 0)
(defparameter *incr-x-pix 40)
(defparameter *v-activity-y-increment  1.0)    ;;was 10 ;;*x-activity-y-increment
(defparameter  *x-max 400)   ;;was 250
(defparameter *graph-last-cycle  NIL)
(defparameter *graph-color-list '(:red :blue :orange :green :black :yellow :lightblue :magenta :pink  :gray :violet :cyan1 :darkorchid :tomato :slateblue :salmon3 :mistyrose1 :brown ))
(defparameter *graph-sym-abrv-begin-n 3 "N begin digits when abreviate graph label on x-axis")
(defparameter *graph-sym-abrv-end-n 3 "N end digits on abrev label" )
(defparameter *setsym-2dim-nested-lists-p T "Puts 2-dim points in nested lists")


;;THE ART2 CELLS-VARIABLES -------------------------------
(defun set-var-lists ()
  (declare (special *art2-vars *art2-cells *art2-array-names *reset-vals *reset-values-list *n-cats ))
  (setf *art2-vars '(INPUT X-ACTIVITY V-ACTIVITY R U Q-ACTIVITY P WUP WDN Y-Output TEMP RESET-VAL  RESET  RESET-CNTR N-CATS TEMP2))
  ;;CELLS ETC
 ;; (setf *art2-cells '((INPUT0 INPUT1 INPUT2 INPUT3 INPUT4) (X-ACTIVITY0 X-ACTIVITY1 X-ACTIVITY2 X-ACTIVITY3 X-ACTIVITY4) (V0 V1 V2 V3 V4) (R0 R1 R2 R3 R4) (U0 U1 U2 U3 U4) (Q0 Q1 Q2 Q3 Q4) (P0 P1 P2 P3 P4) (WUP00 WUP10 WUP20 WUP30 WUP40) (WDN01 WDN11 WDN21 WDN31 WDN41) (Y-Output0 Y-Output1 Y-Output2) (TEMP0 TEMP1 TEMP2 TEMP3 TEMP4) (RESET-VAL0) (RESET0 RESET1 RESET2 RESET3 RESET4) (RESET-CNTR0 RESET-CNTR1 RESET-CNTR2) (N-CATS0 N-CATS1 N-CATS2) (TEMP20 TEMP21 TEMP22)))
  ;;ARRAYS
  ;;(setf *art2-array-names '(*INPUT *X-ACTIVITY *V *R *U *Q *P *WUP *WDN *Y-Output *TEMP *RESET-VAL *RESET *RESET-CNTR *N-CATS *TEMP2))
   
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
  "In ART2.lisp, also sets  *n-inputs, and *n-outputs--evaled in runart."
  (declare (special *n-inputs *n-outputs ))  ;;*num-cycles *graph-every-x-cycle *x-activity-y-increment  *x-max))
  (setf *n-inputs  9
        *n-outputs  5
        ;;Is his default num-cycles is replaced by setf pattern-num-cycles (car p-list) in learnpatterns function within art2-manager function??
        ;;graphing parameters
       ;;done in config-art *num-cycles 40  ;;was 20
#|       *graph-every-x-cycle 10
       *initial-xn-pix 0 *initial-y-pix 0  *incr-x-pix 40 
       *x-activity-y-increment  1.0    ;;was 10 ;;*x-activity-y-increment
        *x-max 400   ;;was 250|#
        )

  (setf *patternA `(,(find-symbol-digits "D") "D")
         *patternB `(,(find-symbol-digits "A") "A")
         *patternC `(,(find-symbol-digits "X") "X"))
  (setf  *pattern1 (list  *patternA *patternB  *patternC))

#| was (setf *pattern1 
        `((,(find-symbol-digits "A") "A")
          ( ,(find-symbol-digits "D") "D")
          (,(find-symbol-digits "X") "X")
          ;;*pattern1 = (((0 0 0 1 0 0 0 1 1) "D") ((0 0 0 1 0 0 0 0 0) "A") ((0 0 0 1 1 1 0 0 0) "X"))
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



;; hhh =====================   ART ARRAY, LIST, and VARIABLE DOCS =====================
;;
;;
;;--------------------------OVERALL ARRAY-LIST REPRESENTATION SCHEME ----------------
;;
(defparameter @ARRAY-LIST-REPR-SCHEME  
 "RULES:
Rule 1. EACH DIM MUST = AN ARRAY DIM.
For cells could be 1 = fromcell 2 = fromcell field,  3 =  tocell, 4 = tocelll field.
EX.  cel35-26=47-31
Rule 2. EACH NEST LEVEL A LATER DIM.
A fromcell in field 1 TO A tocell in  field 2 would be
EG. (((C1-1=1-2 c2-1=1-2. ..))((c1-1=2-2  c2-1=2-2 ..))(( c1-1=3-2. C2-1=3-2...))...)))???
Note, Having only paths from F1 go to only F2 omits some numerucal possibilities and array cells bec ONLY field 1 cels are from cells and only field 2 are tocels. Note how this keeps up and down cell numbering different. 
Rule 3: FORMAT OF ARRAY-SPECS: 
 New:(\"cell-root-str\" ((N begin-n incr-n end-str &key ) (N begin-n incr-n end-str &key) etc)  &key doc)  ;;&key means keys can be later added
 Eg.  (\"Wup\" ((8 1 1 \"-\" :doc \"doc1\")(1 1 1 \"=\")(5 1 1 \"-\")(1 2  1 \"\")) :doc \"whole doc\")
==>Note: use 0 for N and incr if all cells in same field.
                                 ;;old (ncells (cell-root ((begin-n incr-n) ....) \"betw-index-str\" \"end-index-string\")) 
;;In practice, use formats:
;; Eg CELLVAR:  (\"Input\" ((8 1 1 \"-\")(1 1 1)))
;; Eg.PATHVAR:  (\"Wup\" ((8 1 1 \"-\")(1 1 1 \"=\")(5 1 1 \"-\")(1 2 1)))

ART 2 2-FIELD EXAMPLES:
Ex1:  1-DIM CELLS: Input = ((in1-1 in2-1..)(in1-2 in2-2..)?
Ex2:  2-DIM PATHS/VARS:
Wup = ((((wup1-1=1-2 wup2-1=1-2...))
((Wup1-1=2-2 wup2-1=2-2...))
((Wup1-1=3-2 wup2-1=3-2...))
((Wup1-1=4-2 wup2-1=4-2...))
...)) check??? Note the 2nd and 4th dims only have 1 value, so their dims, nested levels look empty.

EX OF 4 VARYING DIMS (all dims fully represented):
((((X1-1=1-1 x21-1=1-1..)(x1-2=1-1 x2-2=1-1))
((X1-1=2-1 x2-1=2-1..)(x1-2=2-1 x2-2=2-1..)...)
(((X1-1=1-2 x2-1=1-2...)(x1-2=1-2 x2-2=1-2,,)...)
((x1-1=2-2 x2-1=2-2..)(x1-2=2-2,,)..)
...)check???
NESTING DEMO SHOWING X AS THE VARYING INDEX
((((Vary x-1=1-1)(vary1-x=1-1))
((Vary1-1=x-1)))
(((Vary1-1=1-x))))
")
;;FOR ALL FUNCTION WRITING: ASSUME MORE FIELDS AND WRITE FUNS TO HANDLE ALL.

;;------------------------------------- end ARRAY-LIST SCHEME ------------------------





;;xxx ---------------------------------------  MY-ART2-INIT INFO  -----------------------------------------------------
;;
;; SEE ARRAY-LIST SCHEME ABOVE = @ARRAY-LIST-REPR-SCHEME
;;
;; EXPLANATION 
;; run (testmai) to test it--works, see below
;;
;;RETURNS
;;  1. VARIABLE-TYPE LIST (eg=
;;(INPUT X-ACTIVITY V R U Q P W WUP WDN Y-OUTPUT TEMP RESET-VAL RESET RESET-CNTR N-CATS TEMP2)
;;
;; 2. CREATES VARIABLE ARRAYS
;;  (eg=  (*INPUT *X-ACTIVITY *V *R *U *Q *P *W *WUP *WDN *Y-OUTPUT *TEMP *RESET-VAL *RESET *RESET-CNTR *N-CATS *TEMP2)
;;
;; 3. ALL ARRAY-LIST VARIABLES LIST 
;; From above, Rule 3: FORMAT OF ARRAY-SPECS:  
;; New:(\"cell-root-str\" ((N begin-n incr-n end-str &key ) (N begin-n incr-n end-str &key) etc)  &key doc)  ;;&key keys can be later added
;; Eg.  (\"Wup\" ((8 1 1 \"-\" :doc \"doc1\")(1 1 1 \"=\")(5 1 1 \"-\")(1 2  1 \"\")) :doc \"whole doc\")
                                 ;;old (ncells (cell-root ((begin-n incr-n) ....) \"betw-index-str\" \"end-index-string\")) 
;;In practice, use formats:
;; Eg CELLVAR:  (\"Input\" ((8 1 1 \"-\")(1 1 1)))
;; Eg.PATHVAR:  (\"Wup\" ((8 1 1 \"-\")(1 1 1 \"=\")(5 1 1 \"-\")(1 2 1)))
;;
;;  4. LABELS FOR EACH VARIABLE
;; (eg= (("Input0" "Input1" "Input2" "Input3" "Input4") ("X-Activity0" "X-Activity1" "X-Activity2" "X-Activity3" "X-Activity4") ("V0" "V1" "V2" "V3" "V4") ("R0" "R1" "R2" "R3" "R4") ("U0" "U1" "U2" "U3" "U4") ("Q0" "Q1" "Q2" "Q3" "Q4") ("P0" "P1" "P2" "P3" "P4") ("W0" "W1" "W2" "W3" "W4") ("Wup00" "Wup10" "Wup20" "Wup30" "Wup40") ("Wdn01" "Wdn11" "Wdn21" "Wdn31" "Wdn41") ("Y-Output0" "Y-Output1" "Y-Output2") ("Temp0" "Temp1" "Temp2" "Temp3" "Temp4") ("reset-Val0") ("reset0" "reset1" "reset2") ("reset-cntr0" "reset-cntr1" "reset-cntr2") ("n-cats0" "n-cats1" "n-cats2") ("Temp20" "Temp21" "Temp22"))
;;
;;  ACTUAL FUNCTION CALL IN ART2 (my-ART2-init nInputs nOutputs :not-return-arrays nil    :no-cell-type-arrays  nil   :make-lower-cell-arrays  nil :initial-element  0  :prefix "*")
;;
;;
;; end --------------------------------    ART ARRAY, LIST, and VARIABLE DOCS -----------------------------------------------






;;MY-ART2-INIT   FUNCTION
;; run (testmai) to test it--works, see below
;;
;;ddd
(defun my-ART2-init (nInputs nOutputs &key (make-sublists-for-each-dim-p T)  
                            unbind-global-vars-p  return-flat-lists-p
                             ;;not needed?
                               prefix)

  "In ART2.lisp, initializes ART2 and creates strings, data lists, and arrays related to both running ART2 and analyzing and reporting it's data based upon lists. RETURNS (values new-symbol-type-list  new-symbols-type-list-of-lists  new-symbol-type-spec-list-of-lists  new-root-list  new-symbol-type-symbol-string-list-of-lists). GLOBAL VARS SET=  *new-seq-nested-lists  *new-symbol-nested-lists *new-dim-list-nested-lists) . Each symbol-type eval to a nested list of all its symbols. Each symbol evals to a spec-list eg."
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
    *cum-array-list *cum-inputs *cum-v-activity *cum-wdn *cum-wup  *cum-y-output *cum-reset-vals 
     ))
   (setf  ;;*total-num-cycles 0 
     ;;*cum-array-list nil 
  *cum-inputs nil *cum-v-activity nil *cum-wdn nil *cum-wup nil  *cum-y-output nil *cum-reset-vals nil)
  ;;the cummulative list
;;  (setf  *cum-array-list  '((*input *x-activity *wdn *wup *y-output *reset )))
      ;;was  '((*input *cum-inputs)(*x-activity *cum-x-activity)(*wdn *cum-wdn)(*wup *cum-wup)(*y-output *cum-y-output)))

;;FOR THE GRAPHICS DATA DISPLAYS -----------------------------------
  (declare
   (special 
    n-dims
    value
    converted-arrays-list
    new-symbol-list
    *i-points
    *v-points
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

  (setf  n-dims 0
   value 0
   converted-arrays-list nil
   new-symbol-list nil
   *i-points nil
   *v-points nil
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
;;EACH symbol-spec-list= (ROOT all-dims-spec-list). ALL-DIMS-SPEC-LIST= (sublist1 sublist2 etc).  Each dim sublist =  (n-elements begin-n/or/cur-dim-n  dim-incr  begin-str end-str. Eg. (\"root\" '((4 1 1 \"C\" \"F\")(3 1 1 \"C\" \"F\"))).
;;KEYS: If set-global-vars-p, sets global * versions of all return vars. 
  (let 
      ((symbol-spec-lists
        `(("Input" ((,nInputs 1 1 ) ))
          ("X-Activity" ((,nInputs 1 1 )) )
          ("V-Activity" ((,nInputs 1 1 )) )
          ("R" ((,nInputs 1 1 )) )
          ("U" ((,nInputs 1 1 )) )
          ("Q-Activity" ((,nInputs 1 1 )) )
          ("P" ((,nInputs 1 1 )) )
          ("W" ((,nInputs 1 1 )) )          
          ;; (var-root (fromcelldim fromfielddim  tocelldim tofielddim))   EACH DIM SPEC= (N  begin incr end-str) 
          ("Wup" ((,nInputs 1 1"" "-") (,nOutputs 1 1) ))
          ("Wdn" ((,nOutputs 1 1 ""  "-") (,nInputs 1 1)  ))
          ("Y-Output" ((,nOutputs 1 1)) )
          ;;others
          ("Temp" ((1 1 1 )) )
          ("reset-Val" ((1 1 1 )) )
          ("reset" ((,nOutputs 1 1 )) )
          ("reset-cntr" ((,nOutputs 1 1)) )
          ("n-cats" ((,nOutputs 1 1 )))   ;;was(1 2 1)) )
          ("Temp2" ((,nOutputs 1 1 )))  ;;was(1 2 1)) )
          ;;end list, symbol-spec-lists
        ))

       ;;other let vars
       (n-symbol-types)
       (dims-list)
#|       (array-symbol-list-of-lists)
       (all-dims-lists)
       (all-arrays-list)
       (array-sym-types-list)|#

       ;;for spec lists
       (ncells1) ;; =  (length spec-list1)
       (dims-lists1) ;; =  (second (car spec-list1))
       (n-dims1) ;;= (length dims-lists1)

       ;;for return values
       (new-symbol-type-list)
       (new-symbols-type-list-of-lists)
       (new-symbol-type-spec-list-of-lists)
       (new-root-list)
       (new-symbol-type-symbol-string-list-of-lists)
       ;;end let vars
       )
    (afout 'out (format nil "1 symbol-spec-lists= ~A~%" symbol-spec-lists))

    ;; USING NEW FUNCTIONS??
    (multiple-value-setq  (new-symbol-type-list  new-symbols-type-list-of-lists
            new-symbol-type-spec-list-of-lists  new-root-list
            new-symbol-type-symbol-string-list-of-lists)
    (make-new-dim-symbol-types symbol-spec-lists
                               :make-sublists-for-each-dim-p make-sublists-for-each-dim-p                                              :return-flat-lists-p return-flat-lists-p))

    ;;SET THE SYMBOL-TYPE  = (sym-root-str LIST OF INSTANCES)
    ;; eg. WDN = ((WDN1-1 ... WDN1-5) (WDN2-1 ...WDN2-5) (WDN3-1 ...WDN3-5))
    ;; The symbols were set to eg ("Wup" (2 4) 9) in a lower function
    (loop
     for symbol-type in new-symbol-type-list
     for symbol-list in new-symbols-type-list-of-lists
     for root in new-root-list
     do
     (set symbol-type (list root symbol-list))
     )     

    ;;INITIALIZE RESET VALUES TO 0
    (loop
     for resetn from 1 to nOutputs
     do
     (setsymval 'reset (list resetn) 0)
     )
     

   ;;SSSS START HERE FINISH INIT??

   ;;note global versions with * in front also created for each value below
   (values new-symbol-type-list  new-symbols-type-list-of-lists
            new-symbol-type-spec-list-of-lists  new-root-list)
    ;;end defun my-ART2-init
    ))
;;TEST
;;  (my-ART2-init  9 5)
;; RESULTS=
;;new-symbol-type-list= (INPUT X-ACTIVITY V R U Q P W WUP WDN Y-OUTPUT TEMP RESET-VAL RESET RESET-CNTR N-CATS TEMP2)
;;new-symbols-type-list-of-lists=  (((INPUT1 INPUT2 INPUT3 INPUT4 INPUT5)) ((X-ACTIVITY1 X-ACTIVITY2 X-ACTIVITY3 X-ACTIVITY4 X-ACTIVITY5)) ((V1 V2 V3 V4 V5)) ((R1 R2 R3 R4 R5)) ((U1 U2 U3 U4 U5)) ((Q1 Q2 Q3 Q4 Q5)) ((P1 P2 P3 P4 P5)) ((W1 W2 W3 W4 W5)) ((WUP1-1 WUP1-2 WUP1-3) (WUP2-1 WUP2-2 WUP2-3) (WUP3-1 WUP3-2 WUP3-3) (WUP4-1 WUP4-2 WUP4-3) (WUP5-1 WUP5-2 WUP5-3)) ((WDN1-1 WDN1-2 WDN1-3 WDN1-4 WDN1-5) (WDN2-1 WDN2-2 WDN2-3 WDN2-4 WDN2-5) (WDN3-1 WDN3-2 WDN3-3 WDN3-4 WDN3-5)) ((Y-OUTPUT1 Y-OUTPUT2 Y-OUTPUT3)) ((TEMP1)) ((RESET-VAL1)) ((RESET1 RESET2 RESET3)) ((RESET-CNTR1 RESET-CNTR2 RESET-CNTR3)) (NIL NIL NIL) (NIL NIL NIL))
;;new-symbol-type-spec-list-of-lists= (((("Input1" (5 1 1 "" "")) ("Input2" (5 2 1 "" "")) ("Input3" (5 3 1 "" "")) ("Input4" (5 4 1 "" "")) ("Input5" (5 5 1 "" "")))) ((("X-Activity1" (5 1 1 "" "")) ("X-Activity2" (5 2 1 "" "")) ("X-Activity3" (5 3 1 "" "")) ("X-Activity4" (5 4 1 "" "")) ("X-Activity5" (5 5 1 "" "")))) ((("V1" (5 1 1 "" "")) ("V2" (5 2 1 "" "")) ("V3" (5 3 1 "" "")) ("V4" (5 4 1 "" "")) ("V5" (5 5 1 "" "")))) ((("R1" (5 1 1 "" "")) ("R2" (5 2 1 "" "")) ("R3" (5 3 1 "" "")) ("R4" (5 4 1 "" "")) ("R5" (5 5 1 "" "")))) ((("U1" (5 1 1 "" "")) ("U2" (5 2 1 "" "")) ("U3" (5 3 1 "" "")) ("U4" (5 4 1 "" "")) ("U5" (5 5 1 "" "")))) ((("Q1" (5 1 1 "" "")) ("Q2" (5 2 1 "" "")) ("Q3" (5 3 1 "" "")) ("Q4" (5 4 1 "" "")) ("Q5" (5 5 1 "" "")))) ((("P1" (5 1 1 "" "")) ("P2" (5 2 1 "" "")) ("P3" (5 3 1 "" "")) ("P4" (5 4 1 "" "")) ("P5" (5 5 1 "" "")))) ((("W1" (5 1 1 "" "")) ("W2" (5 2 1 "" "")) ("W3" (5 3 1 "" "")) ("W4" (5 4 1 "" "")) ("W5" (5 5 1 "" "")))) ((("Wup1-1" (3 1 1 "" "")) ("Wup1-2" (3 2 1 "" "")) ("Wup1-3" (3 3 1 "" ""))) (("Wup2-1" (3 1 1 "" "")) ("Wup2-2" (3 2 1 "" "")) ("Wup2-3" (3 3 1 "" ""))) (("Wup3-1" (3 1 1 "" "")) ("Wup3-2" (3 2 1 "" "")) ("Wup3-3" (3 3 1 "" ""))) (("Wup4-1" (3 1 1 "" "")) ("Wup4-2" (3 2 1 "" "")) ("Wup4-3" (3 3 1 "" ""))) (("Wup5-1" (3 1 1 "" "")) ("Wup5-2" (3 2 1 "" "")) ("Wup5-3" (3 3 1 "" "")))) ((("Wdn1-1" (5 1 1 "" "")) ("Wdn1-2" (5 2 1 "" "")) ("Wdn1-3" (5 3 1 "" "")) ("Wdn1-4" (5 4 1 "" "")) ("Wdn1-5" (5 5 1 "" ""))) (("Wdn2-1" (5 1 1 "" "")) ("Wdn2-2" (5 2 1 "" "")) ("Wdn2-3" (5 3 1 "" "")) ("Wdn2-4" (5 4 1 "" "")) ("Wdn2-5" (5 5 1 "" ""))) (("Wdn3-1" (5 1 1 "" "")) ("Wdn3-2" (5 2 1 "" "")) ("Wdn3-3" (5 3 1 "" "")) ("Wdn3-4" (5 4 1 "" "")) ("Wdn3-5" (5 5 1 "" "")))) ((("Y-Output1" (3 1 1 "" "")) ("Y-Output2" (3 2 1 "" "")) ("Y-Output3" (3 3 1 "" "")))) ((("Temp1" (1 1 1 "" "")))) ((("reset-Val1" (1 1 1 "" "")))) ((("reset1" (3 1 1 "" "")) ("reset2" (3 2 1 "" "")) ("reset3" (3 3 1 "" "")))) ((("reset-cntr1" (3 1 1 "" "")) ("reset-cntr2" (3 2 1 "" "")) ("reset-cntr3" (3 3 1 "" "")))) (NIL NIL NIL) (NIL NIL NIL))
;;new-root-list= ("Input" "X-Activity" "V" "R" "U" "Q" "P" "W" "Wup" "Wdn" "Y-Output" "Temp" "reset-Val" "reset" "reset-cntr" "n-cats" "Temp2")
;;
;;ALSO VARIABLES SET TO VALUES:
;; CL-USER 2 > INPUT =  ((INPUT1 INPUT2 INPUT3 INPUT4 INPUT5))
;; CL-USER 3 > WDN  = ((WDN1-1 WDN1-2 WDN1-3 WDN1-4 WDN1-5) (WDN2-1 WDN2-2 WDN2-3 WDN2-4 WDN2-5) (WDN3-1 WDN3-2 WDN3-3 WDN3-4 WDN3-5))
;; CL-USER 4 > WDN1-3 = ("Wdn" (1 3) 5) 
;;Also the lists were set to gloval vars
;; CL-USER 5 > *new-symbol-type-list = (INPUT X-ACTIVITY V R U Q P W WUP WDN Y-OUTPUT TEMP RESET-VAL RESET RESET-CNTR N-CATS TEMP2)
;; ETC same as above output values


;;moved from ART2.lisp

;;XXX ------------------------------- NETWORK INITIALIZATION --------------------
;; 


;;INITNETWORK
; Initialize the network:
;;ddd
(defun initNetwork (nInputs nOutputs &key
                            (1-dim-nInputs-sym-list  *1-dim-nInputs-sym-list)
                            (1-dim-nOutputs-sym-list *1-dim-nOutputs-sym-list))
   ;;NOTE: Currently,  *1-dim-nInputs-sym-list = '((X-Activity  .01) (V  .01) (R  .01) (U  .01) (Q  .01) (P  .01) (W  .01)))
  ;;*1-dim-nOutputs-sym-list =  '((Y-Output  .01)(Temp  .01) (reset-Val 0) (reset nil) (reset-cntr 0) (Temp2  .01))
  "In Config-ART2.lisp, Original ART2 function that initializes the network computational values"
  (let
      ((sym)
       (val)
       )
    ;;(afout 'out (format nil "INITIAL VALUES:~%"))
   ;;replace by below (zeroActivations nInputs nOutputs)
  ;;FOR 2-dim variables
  (loop
   for j from 1 to nOutputs
   do
    (progn
      (loop
       for i from 1 to nInputs
       do
         (setsymval 'wUp  (list i j) (frandom *wUpInitLo *wUpInitHi))     ;;were 0.05 0.1)  ;;THESE MUST BE SMALL or too manyF2 nodes activated initially
         (setsymval  'wdn (list j i) (frandom  *wDnInitLo  *wDnInitHi))  ;;were 0.01 0.03))  ;;THESE MUST BE LARGE or no learning
        ;;(afout 'out (format nil "INITIAL wUp ~a ~a => ~a " i j (getsymval 'wup (list i j))));;was (aref *wUp i j)))
       ;; (afout 'out (format nil  "INITIAL wDn ~a ~a => ~a " j i (getsymval 'wdn (list j i))))    

        ;;end inner loop
        )
            (setsymval 'n-cats (list j)) 0)
    ;;end loop
    )
  (loop
   for i from 1 to nInputs
   do 
        ;;ADDED BEC LOOKUP GOT NIL FOR VALUES IN FIRST GETSYMVAL
        (setsymval 'v-activity (list i) (frandom  0.001 0.1))
        )


 ;; (afout 'out (format nil "INITIAL wUp= ~a~%wDn= ~a" wUp wDn ))

 ;;TO INIT 1-DIM VARS
 (zeroactivations nInputs nOutputs :1-dim-nInputs-sym-list  1-dim-nInputs-sym-list
                         :1-dim-nOutputs-sym-list 1-dim-nOutputs-sym-list)

  ;;end let, initNetwork
  ))
;;  (initNetwork 5 3)



#|ARRAY VERSION
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
  )|#



;;OOO ---------------------------- OLDER, DELETE? ----------------------------------


#|
;;MAKE-DUAL-INDEX-SPEC-LIST--REPLACED BY GENERAL PURPOSE
;;
;;ddd
(defun make-dual-index-spec-list (field1-n field2-n varname  field1-spec field2-spec
                                            betw-index-str end-index-str)
  (let*
      ((index-spec-list)
       (dim-specs (list field1-spec))
#|       (f1begin-n (first field1-spec))
       (f1incr (second field1-spec))
       (f2begin-n (first field1-spec))
       (f2incr (second field1-spec))|#
       )
    (loop
     for n from 1 to (- field2-n 1)
     do
     (setf dim-specs (append dim-specs (list field2-spec)))
     )
    (setf  index-spec-list 
           (list field1-n (list varname  dim-specs betw-index-str end-index-str)))
    ))
;;TEST
;;  (make-dual-index-spec-list 5 3  "Wup" '(1 1) '(1 1) "" "")
;; = (5 ("Wup" ((1 1) (1 1) (1 1)) "" ""))

|#

#|OLD-DELETE
(symbol-spec-lists
        `((,nInputs ("Input" ((1 1)) "" ""))
          (,nInputs ("X-Activity" ((1 1)) "" ""))
          (,nInputs ("V" ((1 1)) "" ""))
          (,nInputs ("R" ((1 1)) "" ""))
          (,nInputs ("U" ((1 1)) "" ""))
          (,nInputs ("Q" ((1 1)) "" ""))
          (,nInputs ("P" ((1 1)) "" ""))
          (,nInputs ("W" ((1 1)) "" ""))
          ;;NOTE: these must have 1 DIM for each CELL in field 2.
          ;;make the dual index variable spec lists (NOTE commas to eval)
          ,(make-dual-index-spec-list  nInputs nOutputs "Wup" '(1 1) '(1 1) "" "")
          ,(make-dual-index-spec-list  nInputs nOutputs "Wdn" '(1 1) '(1 1) "" "")  
           ;;old-doesn't work (,nInputs  ("Wup" ((1 1)(1 1)) "" ""))
          ;; (,nInputs  ("Wdn" ((1 1)(1 1)) "" ""))
          (,nOutputs ("Y-Output" ((1 1)) "" ""))
          ;;others
          (,nInputs ("Temp" ((1 1)) "" ""))
          (1 ("reset-Val" ((1 1)) "" ""))
          (,nOutputs ("reset" ((1 1)) "" ""))
          (,nOutputs ("reset-cntr" ((1 1)) "" ""))
          (,nOutputs ("n-cats" ((1 1)) "" ""))
          (,nOutputs ("Temp2" ((1 1)) "" ""))))|#

;;OLD
#|      ((symbol-spec-lists
        `(("Input" ((,nInputs 1 1 "-")(1 1 1)) )
          ("X-Activity" ((,nInputs 1 1)(1 1 1)) )
          ("V" ((,nInputs 1 1 "-")(1 1 1)) )
          ("R" ((,nInputs 1 1 "-")(1 1 1)) )
          ("U" ((,nInputs 1 1 "-")(1 1 1)) )
          ("Q" ((,nInputs 1 1 "-")(1 1 1)) )
          ("P" ((,nInputs 1 1 "-")(1 1 1)) )
          ("W" ((,nInputs 1 1 "-")(1 1 1)) )          
          ;; (var-root (fromcelldim fromfielddim  tocelldim tofielddim))   EACH DIM SPEC= (N  begin incr end-str) 
          ("Wup" '((,nInputs 1 1 "-")(1 1 1 "=") '(,nOutputs 1 1 "-") (1 2 1)))
          ("Wdn" '((,nOutputs 1 1 "-")(1 2 1 "=") '(,nInputs 1 1 "-") (1 1 1) ))
          ("Y-Output" ((,nOutputs 1 1)(1 3 1)) )
          ;;others
          ("Temp" ((1 1 1 "-")(1 1 1)) )
          ("reset-Val" ((1 1 1 "-")(1 1 1)) )
          ("reset" ((,nOutputs 1 1 "-")(1 1 1)) )
          ("reset-cntr" ((,nOutputs 1 1"-")(1 1 1)) )
          ("n-cats" ((,nOutputs 1 1 "-")(1 2 1)) )
          ("Temp2" ((,nOutputs 1 1 "-")(1 2 1)) )))|#


;;OLD DELETE? ---------------------------------------------------
#|(defun my-ART2-init (nInputs nOutputs 
                             &key not-return-arrays  no-cell-type-arrays 
                             make-lower-cell-arrays 
                             (make-sublists-for-each-dim-p T) unbind-global-vars-p 
                             return-flat-lists-p
                             ;;not needed?
                             initial-element  prefix)

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
 "In U-tstring.lisp, processes list of SPEC-LISTS to make a list of new symbols. Each symbol-spec-list (N-SYMBOLS (PREFIX (spec-sublist) betw-symbol end)) is used to create a new symbol The SPEC-SUBLIST is a list of  (begin-num increment-num) used to create the array index values. The last 2 can be empty. n-symbol-types is number of  symbols in list."
  (let 
      ((symbol-spec-lists
        `(("Input" ((,nInputs 1 1 "-")(1 1 1) )
          ("X-Activity" ((,nInputs 1 1 "-")(1 1 1)) )
          ("V" ((,nInputs 1 1 "-")(1 1 1)) )
          ("R" ((,nInputs 1 1 "-")(1 1 1)) )
          ("U" ((,nInputs 1 1 "-")(1 1 1)) )
          ("Q" ((,nInputs 1 1 "-")(1 1 1)) )
          ("P" ((,nInputs 1 1 "-")(1 1 1)) )
          ("W" ((,nInputs 1 1 "-")(1 1 1)) )          
          ;; (var-root (fromcelldim fromfielddim  tocelldim tofielddim))   EACH DIM SPEC= (N  begin incr end-str) 
          ("Wup" '((,nInputs 1 1 "-")(1 1 1 "=") '(,nOutputs 1 1 "-") (1 2 1)))
          ("Wdn" '((,nOutputs 1 1 "-")(1 2 1 "=") '(,nInputs 1 1 "-") (1 1 1) ))
          ("Y-Output" ((,nOutputs 1 1)(1 3 1)) )
          ;;others
          ("Temp" ((1 1 1 "-")(1 1 1)) )
          ("reset-Val" ((1 1 1 "-")(1 1 1)) )
          ("reset" ((,nOutputs 1 1 "-")(1 1 1)) )
          ("reset-cntr" ((,nOutputs 1 1"-")(1 1 1)) )
          ("n-cats" ((,nOutputs 1 1 "-")(1 2 1)) )
          ("Temp2" ((,nOutputs 1 1 "-")(1 2 1)) )))
        )

       ;;other let vars
       (n-symbol-types)
       (dims-list)
       (array-symbol-list-of-lists)
       (all-dims-lists)
       (all-arrays-list)
       (array-sym-types-list)

       ;;for spec lists
       (ncells1) ;; =  (length spec-list1)
       (dims-lists1) ;; =  (second (car spec-list1))
       (n-dims1) ;;= (length dims-lists1)
       (array-dims1) ;;= 0
       ;;for return values
       (new-symbol-type-list)
       (new-symbols-type-list-of-lists)
       (new-symbol-type-spec-list-of-lists)
       (new-root-list)
       (new-symbol-type-symbol-string-list-of-lists)
       ;;end let vars
       )

    ;; USING NEW FUNCTIONS??
    (multiple-value-setq  (new-symbol-type-list  new-symbols-type-list-of-lists
            new-symbol-type-spec-list-of-lists  new-root-list
            new-symbol-type-symbol-string-list-of-lists)
    (make-new-dim-symbol-types symbol-spec-lists
                               :make-sublists-for-each-dim-p make-sublists-for-each-dim-p                                               :unbind-global-vars-p unbind-global-vars-p 
                               :return-flat-lists-p return-flat-lists-p))
#|    (loop
     for cellvar-spec in cellvar-spec-lists
     do
     ;; (var-root (fromcelldim fromfielddim  tocelldim tofielddim))   EACH DIM SPEC= (N  begin incr end-str) 
     (let*
         ((var-root (first cellvar-spec))
          (dims-spec-list (second cellvar-spec))
          (rest-vars (cddr cellvar-spec) ;;for any docs, etc not currently used
          )
          (loop
           for dim-spec in dims-spec-list
           do
           ;;USE STRING FUNCTIONS HERE???
       
    ;;
    (dolist (spec-list symbol-spec-lists)
      (setf dims-list (second (second spec-list))
            all-dims-lists (append all-dims-lists  dims-list))
      )
    (afout 'out0 (format nil "dims-list= ~A~%" dims-list))

    ;;MAKES THE SYMBOLS, DIMENSION-LISTS ETC,
   ;;                         and (extra) strings of info for viewing?
    (multiple-value-setq (new-symbol-type-list  new-symbols-type-list-of-lists 
                                                new-symbol-type-spec-list-of-lists 
                                                new-symbol-type-symbol-string-list-of-lists)
        (make-new-index-symbol-types   symbol-spec-lists :make-sublists-for-each-dim-p T))|#

    (setf n-symbol-types (length new-symbol-type-list))
    ;;was (length new-symbols-type-list-of-lists))


    ;;NOT REDUNDANT -- DONE AT LOWER LEVEL, but lower level creates an array for EVERY CELL not for each TYPE of CELL (lower level array creation may be more useful for more complex networks where num of cells vary more for each subtype of cell.

;;  CELL NAME LISTS:  EG: new-symbol-type-list= (INPUT WUP)
;;new-symbols-type-list-of-lists= (((INPUTC1... INPUTC7)) ((WUPC1F1...WUPC2F3) (WUPC3F1...WUPC3F3) (WUPC4F1 ...WUPC4F3)))
;;new-symbol-type-spec-list-of-lists= (((("inputC1" (7 1 1 "C" "")) ...("inputC7" (7 7 1 "C" "")))) ((("wupC1F1" (3 1 1 "F" "")) ... ("wupC1F3" (3 3 1 "F" ""))) (("wupC2F1" (3 1 1 "F" "")) ... ("wupC2F3" (3 3 1 "F" ""))) (("wupC3F1" (3 1 1 "F" "")) ... ("wupC3F3" (3 3 1 "F" ""))) (("wupC4F1" (3 1 1 "F" "")) ... ("wupC4F3" (3 3 1 "F" "")))))
;;new-root-list= ("input" "wup")
;;new-symbol-type-symbol-string-list-of-lists=  ((("inputC1"... "inputC7")) (("wupC1F1" "wupC1F2" "wupC1F3") ("wupC2F1" "wupC2F2" "wupC2F3") ("wupC3F1" "wupC3F2" "wupC3F3") ("wupC4F1" "wupC4F2" "wupC4F3"))) 
;; INPUTC2  =  ("inputC2" (7 2 1 "C" ""))
;; WUPC3F3 =  ("wupC3F3" (3 3 1 "F" ""))
;; make value 3rd ("inputC2" (7 2 1 "C" "") value)

;; use later??     
 ;;  (loop
  ;;     for symbol-type1 in new-symbol-type-list ;;eg INPUT or WUP
  ;;     for symbol-type-list1 in new-symbols-type-list-of-lists  ;;eg ((INPUTC1... INPUTC7)) or ((WUPC1F1...WUPC2F3) (WUPC3F1...WUPC3F3) (WUPC4F1 ...WUPC4F3))
       ;;NOTE: Since INPUTC1=(7 1 1 "C" "")  WUPC2F3=("wupC2F3" (3 3 1 "F" "")), etc, then no need for using accessing these same values in new-symbol-type-spec-list-of-lists, so omit using them.
     ;;  for spec-list1 in  new-symbol-type-spec-list-of-lists ;;(("inputC1" (7 1 1 "C" "")) ...("inputC7" (7 7 1 "C" "")))  or ((("wupC1F1" (3 1 1 "F" "")) ... ("wupC1F3" (3 3 1 "F" ""))) (("wupC2F1" (3 1 1 "F" "")) ... ("wupC2F3" (3 3 1 "F" ""))) (("wupC3F1" (3 1 1 "F" "")) ... ("wupC3F3" (3 3 1 "F" ""))) (("wupC4F1" (3 1 1 "F" "")) ... ("wupC4F3" (3 3 1 "F" ""))))

    ;;LOOP TO MAKE ARRAY FOR EACH CELL TYPE
    ;;mmm
    #|(unless no-cell-type-arrays
      (loop
       for symbol-type1 in new-symbol-type-list ;;eg INPUT or WUP
       for symbol-type-list1 in new-symbols-type-list-of-lists  ;;eg ((INPUTC1... INPUTC7)) or ((WUPC1F1...WUPC2F3) (WUPC3F1...WUPC3F3) (WUPC4F1 ...WUPC4F3))
       ;;NOTE: Since INPUTC1=(7 1 1 "C" "")  WUPC2F3=("wupC2F3" (3 3 1 "F" "")), etc, then no need for using accessing these same values in new-symbol-type-spec-list-of-lists, so omit using them.
     ;;  for spec-list1 in  new-symbol-type-spec-list-of-lists ;;(("inputC1" (7 1 1 "C" "")) ...("inputC7" (7 7 1 "C" "")))  or ((("wupC1F1" (3 1 1 "F" "")) ... ("wupC1F3" (3 3 1 "F" ""))) (("wupC2F1" (3 1 1 "F" "")) ... ("wupC2F3" (3 3 1 "F" ""))) (("wupC3F1" (3 1 1 "F" "")) ... ("wupC3F3" (3 3 1 "F" ""))) (("wupC4F1" (3 1 1 "F" "")) ... ("wupC4F3" (3 3 1 "F" ""))))
       do
       (setf  ncells1   (length spec-list1)
              dims-lists1   (second (car spec-list1))
              n-dims1  (length dims-lists1))

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
       ))|#

    ;;loop to make an array for each cell (or cell subtype)
    #|(loop
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
     )|#

    ;;not-return-arrays  no-cell-type-arrays no-lower-cell-arrays)
    #|(cond
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
     (t nil))|#


    ;;end defun my-ART2-init
    ))|#
;;SSS START HERE     
;; FIX TO 2 CORRECT INDICES  ((W1 W2 W3 W4 W5)) ((WUP111 WUP211 WUP311 WUP411 WUP511) (WUP121 WUP221 WUP321 WUP421 WUP521) (WUP112 WUP212 WUP312 WUP412 WUP512))                          
#|(progn (setf out nil) (my-ART2-init 5 3)) ;; :initial-element 1 :prefix "*"))|#
;;works? returns=>
;; CL-USER 1 > (testmai)
;;



   
;;OUTPUT FROM MAKE-ARRAYS IN U-ARRAYS.LISP
;; use for writing functions to initialize ART2
#|
((INPUT0 INPUT1 INPUT2 INPUT3 INPUT4) (X-ACTIVITY0 X-ACTIVITY1 X-ACTIVITY2 X-ACTIVITY3 X-ACTIVITY4) (V0 V1 V2 V3 V4) (R0 R1 R2 R3 R4) (U0 U1 U2 U3 U4) (Q0 Q1 Q2 Q3 Q4) (P0 P1 P2 P3 P4) (WUP00 WUP11 WUP22 WUP33 WUP44) (WDN00 WDN11 WDN22 WDN33 WDN44) (Y-Output0 Y-Output1 Y-Output2) (TEMP0 TEMP1 TEMP2 TEMP3 TEMP4) (RESET-VAL0) (RESET0 RESET1 RESET2) (RESET-CNTR0 RESET-CNTR1 RESET-CNTR2) (N-CATS0 N-CATS1 N-CATS2) (TEMP20 TEMP21 TEMP22))
((("Input" ((0 1)) "" "") ("Input" ((1 1)) "" "") ("Input" ((2 1)) "" "") ("Input" ((3 1)) "" "") ("Input" ((4 1)) "" "")) (("X-Activity" ((0 1)) "" "") ("X-Activity" ((1 1)) "" "") ("X-Activity" ((2 1)) "" "") ("X-Activity" ((3 1)) "" "") ("X-Activity" ((4 1)) "" "")) (("V" ((0 1)) "" "") ("V" ((1 1)) "" "") ("V" ((2 1)) "" "") ("V" ((3 1)) "" "") ("V" ((4 1)) "" "")) (("R" ((0 1)) "" "") ("R" ((1 1)) "" "") ("R" ((2 1)) "" "") ("R" ((3 1)) "" "") ("R" ((4 1)) "" "")) (("U" ((0 1)) "" "") ("U" ((1 1)) "" "") ("U" ((2 1)) "" "") ("U" ((3 1)) "" "") ("U" ((4 1)) "" "")) (("Q" ((0 1)) "" "") ("Q" ((1 1)) "" "") ("Q" ((2 1)) "" "") ("Q" ((3 1)) "" "") ("Q" ((4 1)) "" "")) (("P" ((0 1)) "" "") ("P" ((1 1)) "" "") ("P" ((2 1)) "" "") ("P" ((3 1)) "" "") ("P" ((4 1)) "" "")) (("Wup" ((0 1) (0 1)) "" "") ("Wup" ((1 1) (1 1)) "" "") ("Wup" ((2 1) (2 1)) "" "") ("Wup" ((3 1) (3 1)) "" "") ("Wup" ((4 1) (4 1)) "" "")) (("Wdn" ((0 1) (0 1)) "" "") ("Wdn" ((1 1) (1 1)) "" "") ("Wdn" ((2 1) (2 1)) "" "") ("Wdn" ((3 1) (3 1)) "" "") ("Wdn" ((4 1) (4 1)) "" "")) (("Y-Output" ((0 1)) "" "") ("Y-Output" ((1 1)) "" "") ("Y-Output" ((2 1)) "" "")) (("Temp" ((0 1)) "" "") ("Temp" ((1 1)) "" "") ("Temp" ((2 1)) "" "") ("Temp" ((3 1)) "" "") ("Temp" ((4 1)) "" "")) (("reset-Val" ((0 1)) "" "")) (("reset" ((0 1)) "" "") ("reset" ((1 1)) "" "") ("reset" ((2 1)) "" "")) (("reset-cntr" ((0 1)) "" "") ("reset-cntr" ((1 1)) "" "") ("reset-cntr" ((2 1)) "" "")) (("n-cats" ((0 1)) "" "") ("n-cats" ((1 1)) "" "") ("n-cats" ((2 1)) "" "")) (("Temp2" ((0 1)) "" "") ("Temp2" ((1 1)) "" "") ("Temp2" ((2 1)) "" "")))
(("Input0" "Input1" "Input2" "Input3" "Input4") ("X-Activity0" "X-Activity1" "X-Activity2" "X-Activity3" "X-Activity4") ("V0" "V1" "V2" "V3" "V4") ("R0" "R1" "R2" "R3" "R4") ("U0" "U1" "U2" "U3" "U4") ("Q0" "Q1" "Q2" "Q3" "Q4") ("P0" "P1" "P2" "P3" "P4") ("Wup00" "Wup11" "Wup22" "Wup33" "Wup44") ("Wdn00" "Wdn11" "Wdn22" "Wdn33" "Wdn44") ("Y-Output0" "Y-Output1" "Y-Output2") ("Temp0" "Temp1" "Temp2" "Temp3" "Temp4") ("reset-Val0") ("reset0" "reset1" "reset2") ("reset-cntr0" "reset-cntr1" "reset-cntr2") ("n-cats0" "n-cats1" "n-cats2") ("Temp20" "Temp21" "Temp22"))
|#

;; END DELETE ------------------------------------------------