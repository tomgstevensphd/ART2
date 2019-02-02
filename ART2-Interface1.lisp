;;*********************ART2-Interface1.lisp ************************
;;
;;
;;(artt)
;;
;;ART2-interface1
;;ddd
;;
(capi:define-interface ART2-interface1 ()
  ()
  (:panes
   (editor-pane-1
    capi:editor-pane
    :buffer "ART2-output-buffer1" :buffer-name "ART2  Output Buffer1"  
  :documentation  "MUST specify buffer names to avoid crashing IDE editor buffers"
   :text  "ART1 Output Buffer1 "
   :visible-min-width 150
   :visible-min-height 100
   :vertical-scroll t
   :horizontal-scroll t
   :title "ART2  Output Buffer1"  
   ;; :title-position nil
   ;; :enabled
   ;; :contents
   ;; :after-input-callback
   ;; :change-callback
   ;;  :composition-callback
   ;; :color-requitements
   ;; :default-height
   ;; :internal-border
   ;; :pane-menu
   ;; :wrap-style
   ;; :relative
   ;; :help-key
   ;; :echo-area-pane
   ;; :echo-area
   ;; :background
   ;; :foreground
   ;; :x
   ;; :y
   ;; :automatic-resize 
   ;; :window-styles
   

    )
   (display-pane-1
    capi:display-pane
    :text "Display-Pane-1")
   (display-pane-2
    capi:display-pane
    :text "Display-Pane-2")
   (editor-pane-2
    capi:editor-pane
    :buffer "ART2-output-buffer2" :buffer-name "ART2  Output Buffer2"  
  :documentation  "MUST specify buffer names to avoid crashing IDE editor buffers"
  :text  "  ART2  Output Buffer2 "
  )
   (push-button-panel-1
    capi:push-button-panel
    :items '("Push-Button-Panel-1" "Button 2" "Button 3")
    :max-width t
    :max-height t)
   (graph-pane-1
    capi:graph-pane
    :roots '(1)
    :children-function 'children-function
      ;;was 'interface-builder::ib-default-graph-pane-children-function
    :print-function 'princ-to-string)
   (output-pane-1
    capi:output-pane)
   (item-pinboard-object-1
    capi:item-pinboard-object
    :text "Item-Pinboard-Object-1")
   (expandable-item-pinboard-object-1
    capi:expandable-item-pinboard-object
    :text "Expandable-Item-Pinboard-Object-1")
   (line-pinboard-object-1
    capi:line-pinboard-object
    :width 50
    :height 50)
   (drawn-pinboard-object-1
    capi:drawn-pinboard-object)
   (labelled-line-pinboard-object-1
    capi:labelled-line-pinboard-object
    :width 50
    :height 50))
  (:layouts
   (grid-layout-1
    capi:grid-layout
    '(editor-pane-1 display-pane-1 display-pane-2 editor-pane-2  graph-pane-1 output-pane-1 push-button-panel-1))
   (pinboard-layout-1
    capi:pinboard-layout
    '(item-pinboard-object-1 expandable-item-pinboard-object-1 line-pinboard-object-1 drawn-pinboard-object-1 labelled-line-pinboard-object-1)))
  (:menu-bar menu-1 menu-3 menu-4)
  (:menus
   (menu-3
    "Menu-3"
    ("Item-5"
     "Item-6"
     "Item-7"))
   (menu-2
    "Menu-2"
    ("Item-3"
     "Item-4"))
   (menu-1
    "Menu-1"
    (menu-2
     "Item-1"
     "Item-2"))
   (menu-4
    "Menu-4"
     ("Item-8"
     "Item-9"))
   )
  (:default-initargs
   :best-height 500
   :best-width 700
   :layout 'grid-layout-1
   :title "ART2 Manager"))







;;--------------------------------------- TTT TEST AREA ---------------------------------
;;(artt)
;;ddd
(defun artt ()
  (let
      ((art-instance  (make-instance  'ART2-Interface1))
       )
    (capi:display art-instance)))
