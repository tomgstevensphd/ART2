;;; Code for the form named |Watson-Window| of class watson1.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)

(defclass |Watson1| (DIALOG)
  ())

;;(in-package :cg-user) 
;;from allegro help example
;;
(defun ask-for-string (prompt &optional (string ""))
  (multiple-value-bind (string1 string2 result)
      (ask-user-for-string prompt string 
                           "~OK" "~Cancel")
    (declare (ignore string2))
    (when (string-equal result "~OK")
      string1)))

;;ask-user-for-string
;;;
;;;  (let ((list-box (find-sibling :list-box widget))
;;;        (string (ask-for-string 
;;;                 "Enter string to add to list"
;;;                 "Greetings")))
;;;    (when string 
;;;      (setf (range list-box) 
;;;        (adjoin string (range list-box)
;;;                :test 
;;;                #'string-equal))
;;;      ))
;;;   t)