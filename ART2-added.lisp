;;******************************** ART2-added.lisp **********************************
;;
;; FUNCTIONS IN WATSON BOOK, BUT NOT IN FILE I DOWNLOADED
;; (THESE WERE COPIED MANUALLY FROM BOOK)
;;
(defun plotActivations (title w y data dmin dmax)
  (let ((size (array-dimension data 0)) (ypos 0) (xpos x))
    (plot-string-italic (- x 47)(+ y 7) title)
    (dotimes (i size)
      (if (< size 20)
          (setq ypos y xpos (+ x(* iPEsizep1)))
        (if (< i (/ size 2))
            (setq ypos (- y 7) xpos (+ x(* i PEsizep1)))
          (setq ypos (+ y 2) xpos (+ x(*(- i (/ size 2)) PEsizep1)))))
      (plot-size-rect
       xpos ypos PEsizem1 PEsizem1
       (max 1
         (truncate (*(/(-(aref data i) dmin) (- dmax dmin)) PEsize))))
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


;;STAND ALONE PROGRAM TO PLOT THE INPUT PATTERNS + OUTPUTS
;; produced by running (ART2)
;;

(defun ART2-Postprocess (&aux (width 130) category-counts)
  
  ;;this defun is part of larger defun
  ;;
 (defun plot-input (x-org y-org values)
  (let ((delta-x (/ (- width 20) (length values))))
    (plot-line x-org y-org x-org (- y-org 20))
    (plot-line x-org y-org (+ x-org (- width 40)) y-org)
    (dotimes (n (- (length values) 1))
      (plot-line
       (+ x-org (* n delta-x))
       (- y-org (* 20 (nth n values)))
       (+ x-org (* (+ 1 n) delta-x)) ;;he had (1 + (nth... an error?
       (- y-org (* 20 (nth (+ 1 n) values)))))))   ;;he had (1 + (nth... an error?
  
  (setq category-counts (copy-tree '(0 0 0)))
  (my-init-plot "ART2: CAtegory codes for input patterns" 282 270)
  (plot-string-italic 20 15 "Category 1")
  (plot-string-italic (+ 20 width width) 15 "Category 3")
  (dolist (p *learned-categories*)
    (print (list "inputs:" (car p) "category:" (cadr p)))
    (let ((pattern (car p))
          (category-code (cadr p)))
      (plot-input
       (+ 20 (* category-code width)) ;; x coord of plot origin
       (+ 40 (* nth category-code category-counts) ;; y coord
         pattern)
       (setf (nth category-code category-counts)
         (+ 1 (nth category-code category-counts)))))) ;;he had (1 + (nth... an error?
  
  ;;watson left out last paren, but is needed if above is correct and part of bigger defun
  )


