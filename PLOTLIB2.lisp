;;
 ; Common plot routines for Common LISP Compatibility -- for MACINTOSH CL version 2.0 (uses CLOS)
 ;
 ; Externally callable functions:
 ;
 ; init-plot( )         ;; creates a graphics window
 ; plot-fill-rect(x y xsize ysize value)    ;; fills a rectangle with a gray-scale value
 ; plot-size-rect(x y xsize ysize value)    ;; plots a rectangle value pixels wide
 ; clear-plot( )            ;; clears the graphics window
 ; pen-width (nibs)         ;; sets the pen drawing width
 ; plot-frame-rect(x y xsize ysize) ;; plots a frame rectangle
 ; plot-line(x1 y1 x2 y2)       ;; plots a line between two points
 ; show-plot( )         ;; shows graphics window
 ; plot-string(x y str)     ;; plots a string at position (x y)
 ; plot-string-bold(x y str)        ;; plots a bold string at position (x y)
 ; plot-string-italic(x y str)      ;; plots a italic string at position (x y)
 ; plot-mouse-down( )       ;; returns position of mouse click
 ;;

;;
 ; Initializes a standard plot window:
 ;;

;;;(defun init-plot (&optional (title "Plot Window") (xSize 250) (ySize 250) )
;;;  (setq *w*
;;;        (make-instance 'window :window-title title
;;;                                      :view-size (make-point xSize ySize)
;;;         :window-type :document-with-zoom) 
;;;      ))

;;
 ; Fills in a rectangle with one of five gray-scale values:
 ;;

(defun plot-fill-rect (x y xsize ysize pattern)
  (setq pattern (truncate pattern))
  (let ((ppp *black-pattern*))
    (if (< pattern 1)
      (setq ppp *white-pattern*)
      (if (equal pattern 1)
        (setq ppp *light-gray-pattern*)
        (if (equal pattern 2)
          (setq ppp *gray-pattern*)
          (if (equal pattern 3)
            (setq ppp *dark-gray-pattern*)
            (setq ppp *black-pattern*)))))
    (fill-rect *w* ppp x y (+ x xsize) (+ y ysize))))

;;
 ; Makes a black rectangle of size proportional to val. This is an alternative
 ; to using function plot-fill-rect for showing graphically the value of a number.
 ;;

(defun plot-size-rect (x y xsize ysize val)
  (setq val (min val xsize))
  (erase-rect *w* x y (+ x xsize) (+ y ysize))
  (fill-rect *w* *black-pattern* x y (+ x val) (+ y val)))
 
;;
 ; Clears (erases) the plot window:
 ;;

(defun clear-plot ()
  (fill-rect *w* *white-pattern* 0 0 400 400))

;;
 ; Sets the drawing size for the pen:
 ;;

(defun pen-width (nibs)
  (set-pen-size *w* (make-point nibs nibs)))

;;
 ; Frames a rectangle of size (xsize ysize) at position (x y):
 ;;

(defun plot-frame-rect (x y xsize ysize)
  (let ((x2 (+ x xsize))
        (y2 (+ y ysize)))
    (frame-rect *w* x y x2 y2)))

;;
 ; Draws a line between two points:
 ;;

(defun plot-line (x1 y1 x2 y2)
  (move-to *w* (truncate x1) (truncate y1))
  (line-to *w* (truncate x2) (truncate y2)))

;;
 ; Shows plot window if it is obscured:
 ;;

(defun show-plot ()
  (window-select *w*))

;;
 ; Plots a string at position (x y):
 ;;

(defun plot-string (x y str &optional (size 10))
  (setq x (truncate x)  y (truncate y))
  ;;causes error (set-view-font *w* (list "Times" size :srcor :plain ))
  (move-to *w* (list x y))
  (princ str *w*))

;;
 ; Plots a string in bold font at position (x y):
 ;;

(defun plot-string-bold (x y str &optional (size 12))
  (setq x (truncate x)  y (truncate y))
  (set-view-font *w* (list "Times" size :srcor :bold))
  (move-to *w* x y) ;; was (move-to *w* x y) caused error
  (princ str *w*))

;;
 ; Plots a string in italic font at position (x y):
 ;;

(defun plot-string-italic (x y str)
  (setq x (truncate x)  y (truncate y))
  ;;(set-view-font *w* '("Times" :plain :italic 12))
  (move-to *w* (list x y)) ;; was (move-to *w* x y) caused error
  (princ str *w*))

;;
 ; Tests for a mouse down event (returns nil if the mouse button is
 ; not being held down when this function is called;  returns a list
 ; of the x and y screen coordinates relative to the plot window
 ; origin if the mouse button is being held down while this
 ; function is being called): 
 ;;

(defun plot-mouse-down ()
  (if (mouse-down-p)
    (let ((p (view-mouse-position *w*)))
      (list (point-h p) (point-v p)))))

;;
 ; A simple test program:
 ;;

(defun test ()
  (init-plot) ;; this line is not in the book
  (show-plot)
  (clear-plot)
  (dotimes (i 6)
    (plot-fill-rect
     (* i 9)
     (* i 9)
     8 8
     i)
    (plot-frame-rect (* i 9) (* i 9) 8 8))
  (dotimes (i 50)
    (plot-size-rect
     (+ 160 (random 200)) (random 100) (random 20) (random 20) (random 5)))
  (dotimes (i 4)
    (plot-string (* i 10) (+ 150 (* i 22)) "Mark's plot utilities..."))
  (plot-string-bold 20 260 "This is a test... of BOLD")
  (plot-string-italic 20 280 "This is a test... of ITALIC"))