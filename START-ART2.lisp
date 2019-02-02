;;RUN ART2

;;TO RUN THIS ART2 PROJECT (From M Watson, Common LISP Modules CH-7 ART2: pp 77ff;
;; TO RUN INSTRUCTIONS p94: (In Listener Window at prompt, type:
;; 1- (ART2-init 5 3); then 2- (ART2);  then 3- (ART2-postprocess)

;BR:ART2

;; start-art2
(defun (start-art2 i j)
  (format t "Starting ART2 with i= ~a  j= ~a" i j)
 ;; (break "hello")
   (ART2-init 5 3)

   (ART2)

  ;(ART2-postprocess)
  )

 ;;(start-art2 5 3)