(cl:in-package :trivial-gamekit-template)

(defun point-on-rect-p (point-pos rect-pos rect-pos2)
  "Returns true if point is inside a rectangle"
  (not
   (or (< (x point-pos) (x rect-pos))
       (< (y point-pos) (y rect-pos))
       (> (x point-pos) (x rect-pos2))
       (> (y point-pos) (y rect-pos2)))))

(defun point-on-circle-p (point-pos circle-center radius)
  "Point on circle counts as in circle"
  (<= (+
       (* (- (x circle-center) (x point-pos)) 
	  (- (x circle-center) (x point-pos))) ;A^2
       (* (- (y circle-center) (y point-pos)) 
	  (- (y circle-center) (y point-pos)))) ;B^2
      (* radius radius)))
   

(defun rects-overlap-p (r-tl r-br r2-tl r2-br)
  "Returns true of two rectangles (top left - bottom right) intersect"
  (not
   (or (< (x r2-br) (x r-tl))
       (< (y r2-br) (y r-tl))
       (> (x r2-tl) (x r-br))
       (> (y r2-tl) (y r-br)))))

(defun circles-overlap-p (cpos radius cpos2 radius2)
  "Returns true of circles overlap"
  (<=
   (+ (* (- (x cpos2) (x cpos)) (- (x cpos2) (x cpos)))
      (* (- (y cpos2) (y cpos)) (- (y cpos2) (y cpos))))
   (expt (+ radius radius2) 2)))
