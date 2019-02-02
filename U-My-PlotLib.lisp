;;***************************************** U-My-PlotLib.lisp ******************************
;;
;; My modification of Watson's PLOTLIB.LSP combined with Allegro cgs
;;
;;
#|
;; FROM MARK WATSON PLOTLIB2.LSP-currently in that file (For Common Lisp)
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
|#
;;

;;ARRAY HELP
;;;copied from LW help:
;;;(setq x-array 
;;;      (make-array '(4 2 3) :initial-contents
;;;             '(((a b c) (1 2 3))
;;;              ((d e f) (3 1 2))
;;;              ((g h i) (2 3 1))
;;;               ((j k l) (0 0 0)))))
    
;;;evaluates to:
;;;#3A(((A B C) (1 2 3))
;;;    ((D E F) (3 1 2))
;;;    ((G H I) (2 3 1))
;;;    ((J K L) (0 0 0)))
;;;which evaluates to itself
;;(aref x-array 1 0 1) => E
;;(setf (aref x-array 1 0 1) 'test)
;;(aref x-array 1 0 1) => test
;;
;;I mix up the types 
;;;(make-array '(2 3) :initial-contents
;;;  '(("this" 7 'what)
;;;    (99 33.2 t)))
;;=> #2A(("this" 7 'WHAT) (99 33.2 T))


;;WATSON INIT-PLOT VERSION from PLOTLIB2.LSP (for CL) BELOW
;;
 ; Initializes a standard plot window:
 ;;

;;;(defun init-plot (&optional (title "Plot Window") (xSize 250) (ySize 250) )
;;;  (setq *w*
;;;        (make-instance 'window :window-title title
;;;                                      :view-size (make-point xSize ySize)
;;;         :window-type :document-with-zoom) 
;;;      ))


;;; to test
;;;(defparameter *init-plot-values*
;;;  '(((Cell 0 1) :Input 0.9 :Field-1 0.7  :Field-2 0.2 :Output 0.9)
;;;    ((Cell 0 2) :Input 0.1 :Field-1 0.04 :Field-2 0.2 :Output 0.2)
;;;    ((Cell 0 3) :Input 0.5 :Field-1 0.6  :Field-2 0.6 :Output 0.6)
;;;    ((Cell 0 4) :Input 0.7 :Field-1 0.3  :Field-2 0.5 :Output 0.5)
;;;    ((Cell 0 5) :Input 0.2 :Field-1 0.05 :Field-2 0.1 :Output 0.3))
;;;    )
(defparameter *init-plot-values*
  '(((Cell 0 1) :Input1 0.9 :Input2 0.7  :Input3 0.2 :Input4 0.9)
    ((Cell 0 2) :Input1 0.1 :Input2 0.04 :Input3 0.2 :Input4 0.2)
    ((Cell 0 3) :Input1 0.5 :Input2 0.6  :Input3 0.6 :Input4 0.6)
    ((Cell 0 4) :Input1 0.7 :Input2 0.3  :Input3 0.5 :Input4 0.5)
    ((Cell 0 5) :Input1 0.2 :Input2 0.05 :Input3 0.1 :Input4 0.3)
    ((Output fx y) :Input1 0.5 :Input2 0.6  :Input3 0.6 :Input4 0.6)
     ))



 ;;;(defparameter *ART2-values*
;;;  '(((Cell 0 1) :Input1 0.9 :Input2 0.7  :Input3 0.2 :Input4 0.9)
;;;    ((Cell 0 2) :Input1 0.1 :Input2 0.04 :Input3 0.2 :Input4 0.2)
;;;    ((Cell 0 3) :Input1 0.5 :Input2 0.6  :Input3 0.6 :Input4 0.6)
;;;    ((Cell 0 4) :Input1 0.7 :Input2 0.3  :Input3 0.5 :Input4 0.5)
;;;    ((Cell 0 5) :Input1 0.2 :Input2 0.05 :Input3 0.1 :Input4 0.3 )
;;;    ((Output fx y) :Input1 0.5 :Input2 0.6  :Input3 0.6 :Input4 0.6)
;;;     )) 

;;;  '(((Cell 0 1) :Input1 (aref input  :Input2 0.7  :Input3 0.2 :Input4 0.9)
;;for testing
(setf *learned-categories* '(((1.0 0.0 3 0.0 0.3 1.0) 0) ((1.0 0.3 0.0 0.3 1.0) 0) ((1.0 0.3 .0  0.3 1.0) 0.0) ((1.0 0.3 0.0 0.3 1.0)0)))
 ;;*learned-categories* = '(... ((1.0 0.3 0.0 0.3 1.0) 0) ...) = ((inputs) output)
;;;(dotimes (n 5)
;;;  (format t "n=~A ~%" n))
;;;(progn
;;;  (setf a1 (make-array 5)
;;;    (aref a1 3) '(1 2 3 4))
;;;  (setf (aref a1 3)  '(3 4 6 b)))
;;;(car (last '((1.0 0.0 3 0.0 0.3 1.0) 0)))

;;(setf *xx* (make-chart-list 5 *learned-categories*))

;;;xxxx
(defun make-chart-list (nInputs learned-categories-list &key incrementValue)
  "MyART2, for creating input to my init-plot. Returns chart-input-list."
  (let ((cellname)
        (cell-list)
        (chart-input-list)
        (cycle-num 0)
        (num-cycles (length learned-categories-list))
        ;;storage for each input cell plus output in an array of lists
        ;; use array for easy replacement/modification
        (cell-input-array (make-array (+ nInputs 1)))
        (new-incrementValue 0)
        )
        
    ;;for test
    ;;;    (setf (aref cell-input-array 5) '(1 2 3 4))
    ;;;    (if *print-details (format t "(aref cell-input-array 6)= ~A~%" 
    ;;;                         (aref cell-input-array 5)))
    ;;start chart-input-list with chart-lists beginning with cell id lists
    ;;  '(((cell 0 0))(cell 0 1))...)
    (dotimes (nI (+ nInputs 1))
      (cond
       ((< nI nInputs)
        (setq cellname 'InCell))
       (t (setq cellname 'OutCell)))
      (setf cell-list (list cellname 0 nI)
        (aref cell-input-array nI)  (list cell-list))) ;;was chart-input-list))
    ;;add test pattern inputs for each cell/each cycle to each cell list   
    (dolist (item learned-categories-list)
      (let* ((input-list (car item))
             (output-value  (second item))
             (info-list (last item))
             (pattern-num (car info-list))
             (chart-sublist)
             (cycle-name (format nil ":Input~A-~A" pattern-num cycle-num))
             )
        (if incrementValue
            (setq new-incrementValue (+ new-incrementValue incrementValue)))
        (setq cycle-num (+ cycle-num 1))
        ;;add each input after the cell id ((cell 0 1).. :input2 0.7..)
        ;; include the output cell, because :inputN must match others for plot??
        (dotimes (nI nInputs)
          (let* ((input-value (nth nI input-list))
                 (chart-sublist (aref cell-input-array nI))                      
                 )
            (if incrementValue 
                (setf input-value (+ input-value new-incrementValue)))
            (setf chart-sublist (append chart-sublist (list cycle-name input-value)))
            ))  
    ;;end let*, dolist
    ))
    (setf chart-input-list nil)
    (dotimes (n (+ nInputs 1))
      (let ((cell-sublist (aref cell-input-array n))
            )
        (setf chart-input-list (append chart-input-list (list cell-sublist)))
        ))
    (if *print-details (format t "Number of Cycles= ~A~% chart-input-list= ~A ~%" 
                         num-cycles chart-input-list)) 
    chart-input-list
    ;;end let, defun
    ))
      
;;;(defun old-make-chart-list (nInputs learned-categories-list)
;;;  "MyART2, for creating input to my init-plot. Returns chart-input-list."
;;;  (let ((cellname)
;;;        (cell-list)
;;;        (chart-input-list)
;;;        (cycle-num 0)
;;;        (num-cycles (length learned-categories-list))
;;;        ;;storage for each input cell plus output in an array of lists
;;;        ;; use array for easy replacement/modification
;;;        (cell-input-array (make-array (+ nInputs 1)))
;;;        )
;;;    ;;for test
;;;    ;;;    (setf (aref cell-input-array 5) '(1 2 3 4))
;;;    ;;;    (if *print-details (format t "(aref cell-input-array 6)= ~A~%" 
;;;    ;;;                         (aref cell-input-array 5)))
;;;    ;;start chart-input-list with chart-lists beginning with cell id lists
;;;    ;;  '(((cell 0 0))(cell 0 1))...)
;;;    (dotimes (nI (+ nInputs 1))
;;;      (cond
;;;       ((< nI nInputs)
;;;        (setq cellname 'InCell))
;;;       (t (setq cellname 'OutCell)))
;;;      (setf cell-list (list cellname 0 nI)
;;;        (aref cell-input-array nI)  (list cell-list))) ;;was chart-input-list))
;;;    ;;add test pattern inputs for each cell/each cycle to each cell list   
;;;    (dolist (item learned-categories-list)
;;;      (let* ((input-list (car item))
;;;             (output-value  (second item))
;;;             (info-list (last item))
;;;             (pattern-num (car info-list))
;;;             (chart-sublist)
;;;             (cycle-name (format nil ":Input~A-~A" pattern-num cycle-num))
;;;             )
;;;        (setq cycle-num (+ cycle-num 1))
;;;        ;;add each input after the cell id ((cell 0 1).. :input2 0.7..)
;;;        ;; include the output cell, because :inputN must match others for plot??
;;;        (dotimes (nI nInputs)
;;;          (let* ((input-value (nth nI input-list))
;;;                 (chart-sublist (aref cell-input-array nI))                      
;;;                 (chart-sublist (append chart-sublist (list cycle-name input-value)))
;;;                 )
;;;
;;;            ;;replace old sublist in array with modfied one
;;;            (setf (aref cell-input-array nI) chart-sublist)
;;;            ))
;;;        ;;(if *print-details (format t "1 value chart-sublist= ~A~%" chart-sublist))
;;;        ;;do the same for the output cell
;;;        (setf chart-sublist (aref cell-input-array nInputs)
;;;          chart-sublist (append chart-sublist (list cycle-name output-value))
;;;          (aref cell-input-array nInputs) chart-sublist)
;;;        ;;(if *print-details (format t "2 end chart-sublist= ~A~%" chart-sublist))
;;;        ;;end let and dolist
;;;        ))
;;;    
;;;    ;;
;;;    (setf chart-input-list nil)
;;;    (dotimes (n (+ nInputs 1))
;;;      (let ((cell-sublist (aref cell-input-array n))
;;;            )
;;;        (setf chart-input-list (append chart-input-list (list cell-sublist)))
;;;        ))
;;;    (if *print-details (format t "Number of Cycles= ~A~% chart-input-list= ~A ~%" 
;;;                         num-cycles chart-input-list)) 
;;;    chart-input-list
;;;    ;;end let, defun
;;;    ))
;;;               



;;(init-plot :ART2-Inputs-Output *init-plot-values*)
;;(init-plot :ART2-Inputs-Output *xx*)      
;;
(defun init-plot (plot-name-key init-plot-value-list &optional (title "ART2 Final Plot Window") (xSize 500) (ySize 500) )
  "in U-My-PlotLib.cl, for NN ART representations. Replaces Watson’s init-plot function."
  (let* ((width xSize)
       (height ySize)
       (chart-widget
        (make-instance 'chart-widget
          :title title
          :chart-view :line
          :fit-chart-items t
          :item-axis (make-instance 'item-axis
                       :on-print-major-label
                       (lambda (cell)
                         (format nil "~:(~a~) ~a ~a"
                           (first cell)(second cell)(third cell))))
          :chart-legend (make-instance 'chart-legend
                          :on-print-chart-object 'capitalize-object)
          :right-attachment :right
          :bottom-attachment :bottom
          :left 0 :top 0 :width width :height height))
       (dialog (make-window plot-name-key  ;;was :ART2-plot
                 :class 'dialog
                 :title title
                 :scrollbars nil
                 :interior (make-box-relative 40 40 width height)
                 :dialog-items (list chart-widget)))
       item-id)
  (dolist (item init-plot-value-list)
    (setq item-id (pop item))
    (loop (unless item (return))
          (set-chart-value
           chart-widget
           :item-id item-id
           :object-id (first item)
           :value (second item))
          (setq item (cddr item))))
  dialog))
  
  



(defparameter *points*

  ;; Points for the first chart object.
  #3a(((51.68 18.49) (59.28 41.71) (10.26 130.29) (12.16 144.05)
       (20.14 136.31) (52.44 42.57) (49.4 59.77) (50.16 72.67)
       (41.42 76.97) (35.72 92.45) (33.06 112.23) (39.52 110.51)
       (47.88 95.89) (38.76 89.87) (25.84 131.15) (17.86 119.11)
       (30.4 104.49) (44.08 89.01) (55.86 70.09) (47.5 52.03)
       (44.08 70.95) (51.68 94.17) (36.86 105.35) (39.9 122.55)
       (23.94 120.83) (16.72 132.01) (31.92 124.27) (56.24 31.39)
       (53.58 55.47) (49.4 83.85) (43.32 105.35))

      ;; Points for the second chart object.
      ((37.0 123.0) (53.58 138.03) (45.98 140.61) (43.32 119.97)
       (25.08 75.25) (24.32 123.41) (37.24 89.01) (39.52 101.91)
       (34.96 107.93) (30.78 114.81) (24.32 112.23) (25.08 99.33)
       (25.08 89.87) (20.9 109.65) (18.24 87.29) (11.02 69.23)
       (12.54 58.05) (17.86 40.85) (19.38 58.05) (25.08 63.21)
       (21.66 81.27) (13.68 88.15) (29.64 128.57) (20.9 118.25)
       (16.72 113.09) (17.1 98.47) (22.42 101.91) (30.02 83.85)
       (33.82 101.05) (28.88 96.75) (27.36 119.11))))

(defparameter *chart-objects
    '((:id first :label "First Collection")(:id second :label "Second Collection")))



;; Initializes a standard plot window:
;;
;;
;;original watson function
;;;(defun init-plot (&optional (title "Plot Window") (xSize 250) (ySize 250) )
;;;  (setq *w*
;;;        (oneof *window* :window-title title
;;;                                      :window-size (make-point xSize ySize)
;;;                                      :window-type :document-with-zoom)))
;;222


(defun init-plot2 (title chart-objects &key (xSize 500) (ySize 500)
                    draw-lines?  line-colors-list  icon-sizes-list icon-fill-colors-list
                    line-dashings-list line-widths-list icon-images-list)
 
  "in U-My-PlotLib.cl, for NN ART representations. Relies on preset *points* array"
  (let* ((width xSize)
         (height ySize)
         (plot-widget
          (make-instance 'plot-widget
            :title title
            :chart-objects chart-objects
            ;;chart-object was '((:id first :label "First Collection")(:id second :label "Second Collection"))
            :plot-view (make-instance 'plot-view
                         :icon-images icon-images-list
                         :draw-lines draw-lines?
                         :icon-sizes icon-sizes-list
                         :icon-fill-colors icon-fill-colors-list
                         :line-colors line-colors-list
                         :line-dashings line-dashings-list
                         :line-widths line-widths-list                         
                         )
            :x-axis (make-instance 'plot-value-axis
                      :axis-label "The X Axis")
            :y-axis (make-instance 'plot-value-axis
                      :axis-label "The Y Axis")
            :chart-legend (make-instance 'chart-legend
                            :layout-orientation :one-column)
            :right-attachment :right
            :bottom-attachment :bottom
            :left 0 :top 0 :width width :height height))
         (dialog (make-window :example-plot
                   :class 'dialog
                   :title "Example Plot"
                   :scrollbars nil
                   :interior (make-box-relative 40 40 width height)
                   :dialog-items (list plot-widget)))
         item-id)
    
    ;; This version copies your data to the plot-widget,
    ;; one element at a time.
    (dotimes (object-index (array-dimension *points* 0))
      (dotimes (value-index (array-dimension *points* 1))
        (set-plot-value plot-widget
          :object-index object-index
          :value-index value-index
          :x (aref *points* object-index value-index 0)
          :y (aref *points* object-index value-index 1))))
    (setq *w* dialog)
    ;;end let*
    ))
;;TEST
;;works (init-plot2 "My Plot Window" *chart-objects )
;; (init-plot2 "My Plot Window" *chart-objects :icon-images-list '(:square :triangle)   :icon-fill-colors-list '(red green)   :xSize 700 :ySize 700 :draw-lines? nil :icon-sizes-list '(30 2))

  


;;FROM ALLEGRO HELP

;;;(defparameter *scores*
;;;  '(((2005 dec 12) :doris 164 :tal 152)
;;;    ((2006 feb 3) :doris 168 :tal 145 :hubert 103)
;;;    ((2006 feb 18) :doris 160 :tal 173 :hubert 110 :myrtle 124)
;;;    ((2006 jun 17) :doris 172 :tal 160 :myrtle 142)
;;;    ((2006 aug 31) :tal 170 :myrtle 135)))

;; My above init-plot function is based upon this version of ‘chart-widget using dolist
;;The first version of the example code uses the approach of 
;;   calling set-chart-value for each datum, after creating the chart-widget. 

#|
(let* ((width 300)
       (height 400)
       (chart-widget
        (make-instance 'chart-widget
          :title "Bowling Scores"
          :chart-view :line
          :fit-chart-items t
          :item-axis (make-instance 'item-axis
                       :on-print-major-label
                       (lambda (date)
                         (format nil "~:(~a~) ~a ~a"
                           (second date)(third date)(first date))))
          :chart-legend (make-instance 'chart-legend
                          :on-print-chart-object 'capitalize-object)
          :right-attachment :right
          :bottom-attachment :bottom
          :left 0 :top 0 :width width :height height))
       (dialog (make-window :bowling-scores
                 :class 'dialog
                 :title "Bowling Scores"
                 :scrollbars nil
                 :interior (make-box-relative 40 40 width height)
                 :dialog-items (list chart-widget)))
       item-id)
  (dolist (item *scores*)
    (setq item-id (pop item))
    (loop (unless item (return))
          (set-chart-value
           chart-widget
           :item-id item-id
           :object-id (first item)
           :value (second item))
          (setq item (cddr item))))
  dialog)
|#

;;The second version builds the same chart by supplying a chart-value-returner function
;;rather than by calling set-chart-value in a loop. 
;;When using a chart-value-returner, you generally need to supply a list of chart-objects
;;and a chart-items-max-index to tell the widget the range of items and objects
;;for which it should query values. 

#|
(let* ((width 300)
       (height 400)
       (chart-widget
        (make-instance 'chart-widget
          :title "Bowling Scores"
          :chart-view :line
          :fit-chart-items t
          :item-axis (make-instance 'item-axis
                       :on-print-major-label
                       (lambda (date)
                         (format nil "~:(~a~) ~a ~a"
                           (second date)(third date)(first date))))
          :chart-legend (make-instance 'chart-legend
                          :on-print-chart-object 'capitalize-object)
          :chart-objects '((:id :doris)(:id :hubert)(:id :myrtle)(:id :tal))
          :chart-items-max-index (1- (length *scores*))
          :chart-value-returner
          (lambda (chart-widget value-type item-index
                                object-index object-id)
            (declare (ignore chart-widget object-index))
            (let* ((item (nth item-index *scores*)))
              (case value-type
                (:id (first item))
                (:value (getf (rest item) object-id)))))
          :right-attachment :right
          :bottom-attachment :bottom
          :left 0 :top 0 :width width :height height)))
  (make-window :bowling-scores
    :class 'dialog
    :title "Bowling Scores"
    :scrollbars nil
    :interior (make-box-relative 40 40 width height)
    :dialog-items (list chart-widget)))

|#


;;; ************************** MY VERSIONS OF OTHER FUNCTIONS IN PLOTLIB2.LSP  ******************
;;;
;;;  COMPLETE REVISING THESE LATER IF AT ALL;  MY INIT-PLOT MAY REPLACE THEM ALL???


;;
;;      INIT-PLOT IS DEFINED ABOVE
;;
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
    ;;(fill-rect *w* ppp x y (+ x xsize) (+ y ysize)) ;;undefined function
    ))

;;
 ; Makes a black rectangle of size proportional to val. This is an alternative
 ; to using function plot-fill-rect for showing graphically the value of a number.
 ;;

(defun plot-size-rect (x y xsize ysize val)
  (setq val (min val xsize))
  ;;(erase-rect *w* x y (+ x xsize) (+ y ysize)) ;;undefined function
  ;; undefined function (fill-rect *w* *black-pattern* x y (+ x val) (+ y val))
  )
 
;;
 ; Clears (erases) the plot window:
 ;;

(defun clear-plot ()
  (fill-rect *w* *white-pattern* 0 0 400 400))

;;
 ; Sets the drawing size for the pen:
 ;;

(defun pen-width (nibs)
  ;;(set-pen-size *w* (make-point nibs nibs)) ;;2 undefined functions
  )

;;
 ; Frames a rectangle of size (xsize ysize) at position (x y):
 ;;

(defun plot-frame-rect (x y xsize ysize)
  (let ((x2 (+ x xsize))
        (y2 (+ y ysize)))
    ;;(frame-rect *w* x y x2 y2))  ;;undefined function    
    ))



;;
 ; Draws a line between two points:
 ;;

(defun plot-line (x1 y1 x2 y2)
  (move-to *w* (truncate x1) (truncate y1))
  ;;(line-to *w* (truncate x2) (truncate y2)) ;;undefined function
  )

;;
 ; Shows plot window if it is obscured:
 ;;

(defun show-plot ()
  ;;(window-select *w*) ;;undefined function
  )

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
  ;;(set-view-font *w* (list "Times" size :srcor :bold))
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

;;;(defun plot-mouse-down ()
;;;  (if (mouse-down-p)
;;;    (let ((p (view-mouse-position *w*)))
;;;      (list (point-h p) (point-v p)))))

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