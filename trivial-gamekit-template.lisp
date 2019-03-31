;;;; Sample to get you started with trivial-gamekit
;;;; trivial-gamekit-template.lisp
;;;; trivial-gamekit guide https://borodust.org/projects/trivial-gamekit/manual/

(in-package #:trivial-gamekit-template)

;; These get set once don't fiddle with em after setting-no effect
(defconstant +screen-width+ 1024)
(defconstant +screen-height+ 768)
(defparameter *score-table* nil)

;; These get updated when the game loops
(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)

;; We'll store user button presses in a key bag each cycle
(defparameter *key-bag* nil)

(defun center-x () (/ +screen-width+ 2.0))
(defun center-y () (/ +screen-height+ 2.0))

(defun get-screen-center ()
  (gamekit:vec2 (center-x) (center-y)))

(defun get-mouse-pos ()
  (gamekit:vec2 *mouse-x* *mouse-y*))

(defconstant TAU (* 2.0 PI))

;; We'll assume a very simple game, tear these out if you're storing score
;; on a per instance basis (remove em from the draw loop too)
(defparameter *lives* 3)
(defparameter *score* 0)

(defun clear-scores (&optional(table-size 10))
  (setf *score-table* (loop
			 :for i :from 0 :below table-size
			 :collecting (cons "---" 0))))

(defun add-score (name score)
  "Adds a score if it's high enough, does nothing if not"
  (setf *score-table* (append *score-table* (list (cons name score))))
  (setf *score-table* (stable-sort *score-table* #'> :key #'cdr))	   
  (setf *score-table* (butlast *score-table*)))

(gamekit:defgame basic-template () ()
		 (:viewport-width +screen-width+)
		 (:viewport-height +screen-height+))

(defun bind-movement-button (button)
  "This function updates the key-bag each game cycle"
  (gamekit:bind-button button :pressed ;Add each cycle
		       (lambda ()
			 (push button *key-bag*)))
  (gamekit:bind-button button :released ;Clean up each cycle
		       (lambda ()
			 (setf *key-bag* (delete button *key-bag*))))
  (gamekit:bind-cursor (lambda (x y)
			 (setf *mouse-x* x)
			 (setf *mouse-y* y))))

(defun key-down (key)
  "High level key checking function"
  (member key *key-bag*))

(defun reset-game ()
  "Save scores, reset game, reposition instances"
  (setf *score* 0)
  (setf *lives* 3))

(defmethod gamekit:post-initialize ((app basic-template))
  (clear-scores)
  ;; Load scores would go here ...
  "Redefines post-initialize to set up our *key-bag*"
  (loop :for key :in '(:space :left :right :up :down :w :a :s :d :mouse-left)
     :do (bind-movement-button key))
  (loop :for key :in '(:escape :q) 
     :do (gamekit:bind-button key :released #'gamekit:stop))
(reset-game))

(defmethod gamekit:act ((this basic-template))
  "We redefine the act method here, all game events get called from here"
  (when (key-down :mouse-left) (incf *lives* 1)))

(defun ty (y)
  "This transforms the y coordinate into a traditional top = 0 bottom = height"
  (- +screen-height+ y))

(defun draw-scores (x-pos y-pos)
  ;; Note, this uses the y at top drawing coordinates
  ;; As always never call this outside gamekit:draw
  (loop
     :for i :in *score-table*
     :for y = y-pos :then (+ y 24)
     :do
     (gamekit:draw-text (format nil "~a : ~a" (car i) (cdr i))
			(gamekit:vec2 x-pos (ty y)))))

(defmethod gamekit:draw ((this basic-template))
  "We redefine the draw method here, YOU ONLY CALL DRAWING FROM HERE"
  (gamekit:draw-text
   (concatenate 'string
		"Keys Pressed: "
		(when (key-down :mouse-left) "mouse:left")
		(when (key-down :space) "space ")
		(when (key-down :up   ) " up ")
		(when (key-down :down ) " down ")
		(when (key-down :left ) " left ")
		(when (key-down :right) " right ")
		(when (key-down :w   ) " w ")
		(when (key-down :a ) " a ")
		(when (key-down :s ) " s ")
		(when (key-down :d) " d "))
   (gamekit:vec2 32 32))
  (gamekit:draw-text "Press ESC or Q to quit" (gamekit:vec2 0 (ty 24)))
  (draw-scores 64 128)
  (gamekit:draw-text (format nil "Score: ~A" *score*)
		     (gamekit:vec2 0 (ty 48)))
  
  (gamekit:draw-text (format nil "Lives: ~A" *lives*)
		     (gamekit:vec2 0 (ty 68)))
  
  ;; Circle circle overlap test. (more types exist in misc.lisp)
  (let* ((pos1 (get-mouse-pos))
	 (pos2 (get-screen-center))
	 (radius1 32)
	 (radius2 128)
	 (color (if (circles-overlap-p pos1 radius1 pos2 radius2)
		    (gamekit:vec4 1 0 0 1)
		    (gamekit:vec4 0 1 0 1))))
    
    (gamekit:draw-circle pos2 radius2
			 :fill-paint color
			 :stroke-paint  (gamekit:vec4 0 .3 1 1))
    
    (gamekit:draw-circle pos1 radius1
			   :fill-paint color
			   :stroke-paint (gamekit:vec4 0 .3 1 1))))


;; To run this hot mamma run the following in a live REPL
;; (ql:quickload :trivial-gamekit-template)
;; (in-package :trivial-gamekit-template)
;; (gamekit:start 'basic-template)
