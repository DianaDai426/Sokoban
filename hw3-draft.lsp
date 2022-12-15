; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

; get list of goal position (top to bottom)

(defun get-goals-row (row r c m)
	(cond
		((not row) m)
		((isStar (car row)) (get-goals-row (cdr row) r (+ c 1) (cons (list r c) m)))
		(t (get-goals-row (cdr row) r (+ c 1) m))
	)
)

(defun get-goals (s r m)
	(cond
		((not s) m)
		(t (append (get-goals-row (car s) r 0 '()) (get-goals (cdr s) (+ r 1) m)))
	)
)

; get list of box position (top to bottom)
(defun get-boxes-row (row r c m)
	(cond
		((not row) m)
		((isBox (car row)) (get-boxes-row (cdr row) r (+ c 1) (cons (list r c) m)))
		(t (get-boxes-row (cdr row) r (+ c 1) m))
	)
)

(defun get-boxes (s r m)
	(cond
		((not s) m)
		(t (append (get-boxes-row (car s) r 0 '()) (get-boxes (cdr s) (+ r 1) m)))
	)
)
; calculate distance by pairs
(defun distance (goal box)
	(+
		(abs (- (or (car goal) 0) (or (car box) 0)))
		(abs (- (or (cadr goal) 0) (or (cadr box) 0)))
		0
	)
)

; calculate total distances of pairs
(defun distances (goals boxes)
	(or 
		(if
			(or (not goals) (not boxes))
			0
			(+ 
				(distance (car goals) (car boxes))
				(distances (cdr goals) (cdr boxes))
				0
			)
		)
		0
	)
)

; min distance between a box and all goals (any one)
; ok
(defun closest-distance (goals box min)
	(cond
		((not goals) min)
		(t (if	(< (distance (car goals) box) min)
			(closest-distance (cdr goals) box (distance (car goals) box))
			(closest-distance (cdr goals) box min)
		))
	)
)

(defun closest-distance-total (goals boxes)
	(cond 
		((not boxes) 0)
		(t (+ 
			(closest-distance goals (car boxes) (expt 2 31))
			(closest-distance-total goals (cdr boxes))
		))
	)

)

(defun h105383173 (s)
	(or 
	(closest-distance-total 
		(cleanUpList (get-goals s 0 ()))
		(cleanUpList (get-boxes s 0 ()))
	)
	0)
)

(defun hdistance (s)
	(distances
		(cleanUpList (get-goals s 0 ()))
		(cleanUpList (get-boxes s 0 ()))
	)
)

(defun heur (s)
  (cond
    ((not s) 0)
    (T (+ (DistDiff (car s) 0 0) (heur (cdr s))))
  )
)

; DistDiff heuristic utilizes logic similar to the Manhattan Distance
; heuristic, counting the number of boxes/goals and returning the 
; row-based difference between them. Takes in row, numbers of goals & boxes.
; DistDiff must be admissible because a distance >= 1 denotes that there is 
; a goal or box in a row without another respective box or goal. This indicates
; AT LEAST one move must be made so that the box is moved into this goal row
; or vice versa. So, DistDiff will never overestimate the moves. 
(defun DistDiff (row goal_count box_count)
  (cond
    ((not row) (abs (- goal_count box_count)))
    ((isStar (car row)) (DistDiff (cdr row) (+ 1 goal_count) box_count))
    ((isBox (car row)) (DistDiff (cdr row) goal_count (+ 1 box_count)))
    (t (DistDiff (cdr row) goal_count box_count))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; calculate the average distance between a box and all goals
(defun avg_dist (goals box)
	(cond 
		((not goals) 0)
		(t (distance (car goal) box))
	
	)
)

; calculate the average distance between each pair of goal and box
(defun avg_dists (goals boxes)
	(cond 
		((not boxes) 0)
		(t (+ (avg-dist goals (car boxes)) (avg_dists goals (cdr boxes))))
	)
)

; heuristics of average distance
(defun heur_avg_dist (s)
	(avg_dist
		(cleanUpList (get-goals s 0 ()))
		(cleanUpList (get-boxes s 0 ()))
	)	

)

; h1 succeeds until p14
; heur p13 ~16s

(load-a-star)
#| (a* p1 #'goal-test #'next-states #'h0) |#
#| (a* p14 #'goal-test #'next-states #'heur) |#
(printstates (a* p14 #'goal-test #'next-states #'h105383173) 0.2)

(setq s1 '(
	(1 1 1 1 1)
    (1 4 0 4 1) 
	(1 0 2 0 1) 
	(1 0 3 0 1) 
	(1 0 0 2 1) 
	(1 1 1 1 1) ))

#| (write (cleanUpList (get-goals p7 0 ())))
(terpri)
(write (cleanUpList (get-boxes p7 0 ())))
(terpri)
(write (closest-distance-total
	(cleanUpList (get-goals p7 0 ()))
	(cleanUpList (get-boxes p7 0 ()))
	)) |#
