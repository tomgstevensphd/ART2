;;****************************** 1-my-artnotes.cl ******************************8
;;
;;
;;
#|

ORDER OF ART EVALUATION

;;from ART2.LSP and ART2-added.cl (from me)
;;
;;TO RUN THIS ART2 PROJECT (From M Watson, Common LISP Modules CH-7 ART2: pp 77ff;
;; TO RUN INSTRUCTIONS p94: (In Listener Window at prompt, type:
;; 1- (ART2-init 5 3); then 2- (ART2);  then 3- (ART2-postprocess)



ART2 PLOTTING FUNCTIONS NOTES:
* See file U-My-PlotLib.cl  (Watson’s CL plotting functions mostly in PLOTLIB2.LSP, 
PLOTLIB.LSP are same functions, but not CL versions?

* My init-plot function may be a better version of Watson’s and plot lines instead of boxes etc.
* His functions have many errors for current CL; so I’d save time not revising them,
thou I listed them in U-My-PlotLib.cl 


While compiling these undefined functions were referenced:
         PLOT-INPUT from character position 1450 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\ART2-added.cl
       X  -I from character position 190 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\ART2-added.cl

       X  ADD-OTHER-COLOR from character position 2453 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\coefficient-dialog.cl
        X DRAW-CURVE from character position 1616 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\coefficient-dialog.cl
         XCURRENT-COLOR from character position 1616 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\coefficient-dialog.cl

      X   AUTHOR-NAME? from character position 5968 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\LIBRARY.LSP
       X  CADAAAR from character position 3195 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\LIBRARY.LSP

       x  POINT-V from character position 3973 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\PLOTLIB2.LSP, character position 15278 in C:\TOM\LISP PROJECTS\allegro-projects\CL-help\U-My-PlotLib.cl
       x  POINT-H from character position 3973 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\PLOTLIB2.LSP, character position 15278 in C:\TOM\LISP PROJECTS\allegro-projects\CL-help\U-My-PlotLib.cl
       x  VIEW-MOUSE-POSITION from character position 3973 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\PLOTLIB2.LSP, character position 15278 in C:\TOM\LISP PROJECTS\allegro-projects\CL-help\U-My-PlotLib.cl
       x  MOUSE-DOWN-P from character position 3973 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\PLOTLIB2.LSP, character position 15278 in C:\TOM\LISP PROJECTS\allegro-projects\CL-help\U-My-PlotLib.cl
       x  SET-VIEW-FONT from character position 3171 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\PLOTLIB2.LSP, character position 14476 in C:\TOM\LISP PROJECTS\allegro-projects\CL-help\U-My-PlotLib.cl
       x  WINDOW-SELECT from character position 2816 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\PLOTLIB2.LSP, character position 14121 in C:\TOM\LISP PROJECTS\allegro-projects\CL-help\U-My-PlotLib.cl
       x  LINE-TO from character position 2647 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\PLOTLIB2.LSP, character position 13952 in C:\TOM\LISP PROJECTS\allegro-projects\CL-help\U-My-PlotLib.cl
       x  FRAME-RECT from character position 2477 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\PLOTLIB2.LSP, character position 13782 in C:\TOM\LISP PROJECTS\allegro-projects\CL-help\U-My-PlotLib.cl
       x  SET-PEN-SIZE from character position 2336 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\PLOTLIB2.LSP, character position 13641 in C:\TOM\LISP PROJECTS\allegro-projects\CL-help\U-My-PlotLib.cl
       x  from character position 2336 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\PLOTLIB2.LSP, character position 13641 in C:\TOM\LISP PROJECTS\allegro-projects\CL-help\U-My-PlotLib.cl
       x  ERASE-RECT from character position 1996 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\PLOTLIB2.LSP, character position 13301 in C:\TOM\LISP PROJECTS\allegro-projects\CL-help\U-My-PlotLib.cl

       x  FILL-RECT from character position 1349 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\PLOTLIB2.LSP, 
               character position 1996 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\PLOTLIB2.LSP, character position 2221 in C:\TOM\LISP PROJECTS\allegro-projects\Watson-allegro\Watson-allegro\PLOTLIB2.LSP, character position 12654 in C:\TOM\LISP PROJECTS\allegro-projects\CL-help\U-My-PlotLib.cl


|#