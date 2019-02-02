;;; Define :form1
 
;;(in-package :common-graphics-user)

(defclass ART-plot-window1 (BITMAP-WINDOW)
  ())


;; The finder-function, which returns the window if it already
;; exists, and otherwise creates and returns it.
;; Call this function if you need only one copy of this window,
;; and that window is a non-owned top-level window.
(defun art-plot-form1 () (find-or-make-application-window :form1 'make-form1))
 
;; The maker-function, which always creates a new window.
;; Call this function if you need more than one copy,
;; or the single copy should have a parent or owner window.
;; (Pass :owner to this function; :parent is for compatibility.)



(defun make-art-plot-form1
    (&key parent (owner (or parent (screen *system*))) exterior
      (interior (make-box 130 343 972 689)) (name :form1) (title "ART-plot-Form1")
      (border :frame)
     (child-p nil) form-p)
  (let ((owner
         (make-window name :owner owner
           :class '|ART-plot-window1|
           :exterior exterior
           :interior interior
           :border border
           :child-p child-p
           :close-button t
           :font (make-font-ex :swiss "Arial / ANSI" 12)
           :form-state :normal
           :maximize-button t
           :minimize-button t
           :name :form1
           :pop-up nil
           :resizable t
           :scrollbars nil
           :state :normal
           :system-menu t
           :title title
           :title-bar t
           :dialog-items (make-form1-widgets)
           :form-p form-p
           :form-package-name nil)))
    owner))

(defun make-form1-widgets ()
  (list (make-instance 'plot-widget :chart-legend (make-instance 'chart-legend) :font
          (make-font-ex nil "Segoe UI / Default" 12) :left 74 :name
          :plot-widget-2 :plot-view (make-instance 'plot-view) :title
          "Empty Plot" :top 53 :x-axis (make-instance 'plot-value-axis)
          :x-axis-2 (make-instance 'plot-value-axis) :y-axis
          (make-instance 'plot-value-axis) :y-axis-2
          (make-instance 'plot-value-axis))
    
    (make-instance 'chart-widget :bar-chart-view (make-instance 'bar-chart-view)
      :chart-legend (make-instance 'chart-legend) :font
      (make-font-ex nil "Segoe UI / Default" 12) :item-axis
      (make-instance 'item-axis) :left 472 :line-graph-view
      (make-instance 'line-graph-view) :name :chart-widget-1 :title
      "Empty Chart" :top 53 :value-axis (make-instance 'value-axis)
      :value-axis-2 (make-instance 'value-axis))))
