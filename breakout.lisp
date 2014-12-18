;;;; breakout.lisp

(in-package #:breakout)

;;; "breakout" goes here. Hacks and glory await!

(defparameter *data-root* "src/lisp/breakout/")
(defparameter *font-root* (merge-pathnames "fonts/" *data-root*))
(defparameter *audio-root* (merge-pathnames "audio/" *data-root*))
(defparameter *gfx-root* (merge-pathnames "gfx/" *data-root*))
(defparameter *level-root* (merge-pathnames "levels/" *data-root*))

;;;; Game Params
(defparameter *game-width* 480)
(defparameter *game-height* 600)
(defparameter *game-state* 0) ; 0=menu, 1:ready, 2:in-game, 3:win-lose

(defparameter *level-width* 15) ;25
(defparameter *level-height* 15) ;20
(defparameter *level* nil)
(defparameter *tile-width* 32) ;32
(defparameter *tile-height* 20) ;20


(defparameter *player* nil)
(defparameter *player-score* 0)
(defparameter *player-lives* 3)
(defparameter *level-number* 1)

(defparameter *ball* nil)
(defparameter *pause* 0)
(defparameter *get-ready* 0)
(defparameter *get-ready-count* 0)

(defparameter *bricks-in-level* 0)

;;;; Sound Params
(defparameter *mixer-opened* nil)
(defparameter *music* nil)
(defparameter *soundfx* nil)

;;;; GFX Params
(defparameter *gfx-breakout* (merge-pathnames "intro.png" *gfx-root*))
(defparameter *gfx-game-over* (merge-pathnames "game_over.jpg" *gfx-root*))

;;;; Font Params
(defparameter *terminus-ttf-12* 
  (make-instance 'SDL:ttf-font-definition
		 :size 12
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))

(defparameter *terminus-ttf-18* 
  (make-instance 'SDL:ttf-font-definition
		 :size 18
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))

(defparameter *terminus-ttf-24* 
  (make-instance 'SDL:ttf-font-definition
		 :size 24
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))

(defparameter *terminus-ttf-32* 
  (make-instance 'SDL:ttf-font-definition
		 :size 32
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))

(defparameter *ttf-font-small* nil)
(defparameter *ttf-font-normal* nil)
(defparameter *ttf-font-large* nil)
(defparameter *ttf-font-huge* nil)


;;;; PADDLE class

(defclass paddle ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (r :accessor r :initarg :r)
   (g :accessor g :initarg :g)
   (b :accessor b :initarg :b)
   (spd :accessor spd :initarg :spd)))

;;;; BALL class

(defclass ball ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (r :accessor r :initarg :r)
   (g :accessor g :initarg :g)
   (b :accessor b :initarg :b)
   (dx :accessor dx :initarg :dx)
   (dy :accessor dy :initarg :dy)
   (spd :accessor spd :initarg :spd)))


;;;; CONTINUABLE macro

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))


;;;; UPDATE-SWANK function

(defun update-swank ()
  (continuable
   (let ((connection (or swank::*emacs-connection*
			 (swank::default-connection))))
     (when connection
       (swank::handle-requests connection t)))))


;;;;;;;;;;;;;;;;;;;;;;;; PRIMITIVES ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DRAW-TEXT function

(defun draw-text (string x y r g b &optional (font *ttf-font-normal*))
  (sdl:draw-string-solid-* string
			   x y
			   :color (sdl:color :r r :g g :b b)
			   :font font))


;;;; DRAW-BOX function

(defun draw-box (x y w h r g b)
  (sdl:draw-box-* x y w h
		  :color (sdl:color :r r :g g :b b)))


;;;; DRAW-LINE function

(defun draw-line (x0 y0 x1 y1 r g b)
  (sdl:draw-line-* x0 y0 x1 y1
		  :color (sdl:color :r r :g g :b b)))


;;;; PLAY-SOUND function

(defun play-sound (s)
  (sdl-mixer:play-sample (aref *soundfx* s)))


;;;;;;;;;;;;;;;;;;;;;;;; LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; MAKE-LEVEL function

(defun make-level ()
  (setf *level* (make-array (list *level-height* *level-width*))))


;;;; PARSE-LEVEL function

(defun parse-level (lines)
  (setf *level-width* (length (first lines)))
  (setf *level-height* (length lines))
  (make-level)
  (let* ((width *level-width*)
	 (height *level-height*))
    (loop for y below height
	 for line in lines
	 do (loop for x below width
	       as element = (aref line x)
		 do (progn (setf (aref *level* y x) element)
			   (unless (or (equalp element #\0) (equalp element #\8))
			     (setf *bricks-in-level* (incf *bricks-in-level*))))))))


;;;; LOAD-LEVEL function

(defun load-level (level)
  (let ((filename (format nil "~alevel_~1,'0d.dat" *level-root* level)))
    (with-open-file (stream filename)
      (loop as line = (read-line stream nil nil)
	 until (null line)
	 collect line into lines
	 finally (return (parse-level lines))))))



;;;; DRAW-TILE function

(defun draw-tile (tile x y)
  (let ((pos-x (* x *tile-width*))
	(pos-y (* y *tile-height*)))
    (case tile
      ((#\1) (draw-box pos-x pos-y (- *tile-width* 2) (- *tile-height* 2) 209 17 65))

      ((#\2) (draw-box pos-x pos-y (- *tile-width* 2) (- *tile-height* 2) 0 177 89))
      
      ((#\3) (draw-box pos-x pos-y (- *tile-width* 2) (- *tile-height* 2) 0 174 219))

      ((#\4) (draw-box pos-x pos-y (- *tile-width* 2) (- *tile-height* 2) 243 119 53))

      ((#\5) (draw-box pos-x pos-y (- *tile-width* 2) (- *tile-height* 2) 255 196 37))

      ((#\6) (draw-box pos-x pos-y (- *tile-width* 2) (- *tile-height* 2) 114 0 172))
      
      ((#\7) (draw-box pos-x pos-y (- *tile-width* 2) (- *tile-height* 2) 99 47 0))

      ((#\8) (draw-box pos-x pos-y (- *tile-width* 2) (- *tile-height* 2) 192 192 192)))))

;;;; DISPLAY-LEVEL function

(defun display-level ()
  (loop for y below *level-height*
     do (progn (fresh-line)
	       (loop for x below *level-width*
		  for tile = (aref *level* y x)
		  do (draw-tile tile x y)))))


;;;; DISPLAY-BACKGROUND function

(defun display-background ()
  (sdl:draw-surface-at-* 
   (sdl:load-image (format nil "~alevel_~a.jpg" *gfx-root* *level-number*)) 0 0))



;;;; DISPLAY-GET-READY function

(defun display-get-ready ()
  (if (>= *get-ready-count* 100)
      (progn (setf *get-ready* 0)
	     (setf *pause* 0))
      (setf *get-ready-count* (incf *get-ready-count*)))
  
  (if (= *get-ready-count* 1)
      (play-sound 5))

  (if (< *get-ready-count* 60)
      (draw-text "READY!" 180 300 255 255 255 *ttf-font-huge*)
      (draw-text "GO!" 200 300 255 255 255 *ttf-font-huge*)))

;;;;;;;;;;;;;;;;;;;;;;;; BALL ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; CREATE-BALL function

(defun create-ball ()
  (setf *ball* (make-instance 'ball
			      :x (- (/ *game-width* 2) 3) :y (- *game-height* 57)
			      :w 5 :h 5
			      :r 255 :g 255 :b 255
			      :dx (- (random 5) 2) :dy -2 :spd 2)))


;;;; RESET-BALL function
(defun reset-ball (b)
  (setf (x b) (- (/ *game-width* 2) 3))
  (setf (y b) (- *game-height* 57))
  (setf (dy b) -2)
  (setf (dx b) (- (random 7) 3)))


;;;; DISPLAY-BALL function

(defun display-ball (b)
  (draw-box (x b) (y b) (w b) (h b) (r b) (g b) (b b)))


;;;;;;;;;;;;;;;;;;;;;;;; BALL PHYSICS ;;;;;;;;;;;;;;;;;;;;;;;;
1

;;;; UPDATE-BALL function

(defun update-ball (b p)
  (setf (x b) (+ (x b) (* (dx b) (spd b))))
  (setf (y b) (+ (y b) (* (dy b) (spd b))))

  ; Bricks
  (collide-brick b)

  ; Left Wall
  (if (<= (x b) 0)
      (progn (setf (dx b) (abs (dx b)))
	     (play-sound 2)))

  ; Right Wall
  (if (>= (+ (x b) (w b)) *game-width*)
      (progn (setf (dx b) (- (dx b)))
	     (play-sound 2)))

  ; Top Wall
  (if (<= (y b) 0)
      (progn (setf (dy b) (abs (dy b)))
	     (play-sound 2)))

  ; Paddle
  (if (and (<= (x p) (x b))
	   (>= (+ (x p) (w p)) (+ (x b) (w b)))
	   (<= (y p) (+ (y b) (h b)))
	   (>= (+ (y p) (h p)) (+ (y b) (h b))))
      (handle-paddle-collision p b))

  ; Fall Out
  (if (> (+ (y b) (h b)) (- *game-height* 30))
      (progn (setf *player-lives* (decf *player-lives*))
	     (if (zerop *player-lives*)
		 (progn (play-sound 6)
			(change-game-state))
		 (progn (play-sound 4)
			(reset-level))))))


;;;; COLLIDE-BRICK function

(defun collide-brick (b)
  (loop for y below *level-height*
       do (loop for x below *level-width*
	  for brick = (aref *level* y x)
	  do (if (equalp brick #\0)
		 t			 
		 (handle-brick-collision brick x y b)))))
			 

;;;; HANDLE-BRICK-COLLISION function			 

(defun handle-brick-collision (brick x y b)
  (let* ((w *tile-width*)
	 (h *tile-height*))

          ;top
    (cond ((and (<= (* y h) (+ (y b) (h b))) 
		(>= (* y h) (y b))
		(<= (* x w) (x b))
		(>= (+ (* x w) w) (+ (x b) (w b))))
	   (progn (setf (dy b) (- (dy b)))
		  (update-score brick x y)))
	  
	  ; bottom
	  ((and (>= (+ (* y h) h) (y b))
		(<= (+ (* y h) h) (+ (y b) (h b)))
		(<= (* x w) (x b))
		(>= (+ (* x w) w) (+ (x b) (w b))))
	   (progn (setf (dy b) (- (dy b)))
		  (update-score brick x y)))

	  ; left
	  ((and (<= (* y h) (y b))
		(>= (+ (* y h) h) (+ (y b) (h b))) 
		(>= (* x w) (x b))
		(<= (* x w) (+ (x b) (w b))))
	   (progn (setf (dx b) (- (dx b)))
		  (update-score brick x y)))

	  ; right
	  ((and	(<= (* y h) (y b))
		(>= (+ (* y h) h) (+ (y b) (h b)))
		(>= (+ (* x w) w) (x b))
		(<= (+ (* x w) w) (+ (x b) (w b))))
	   (progn (setf (dx b) (- (dx b)))
		  (update-score brick x y))))))


;;;; HANDLE-PADDLE-COLLISION function

(defun handle-paddle-collision (p b)
  (let* ((pw-div (/ (w p) 4))
	 (b-mid (+ (x b) (/ (w b) 2))))
	  
    (cond ((and (<= (x p) b-mid)
		(>= (+ (x p) pw-div) b-mid)
		(<= (y p) (+ (y b) (h b)))
		(>= (y p) (- (y b) (h b))))
	   (progn (setf (dy b) (- (dy b)))
		  (if (sdl:get-key-state :sdl-key-left)
		      (setf (dx b) -3)
		      (setf (dx b) -2))
		  (play-sound 1)))

	  ((and (<= (+ (x p) pw-div) b-mid)
		(>= (+ (x p) (* pw-div 2)) b-mid)
		(<= (y p) (+ (y b) (h b)))
		(>= (y p) (- (y b) (h b))))
	   (progn (setf (dy b) (- (dy b)))
		  (setf (dx b) -1)
		  (play-sound 1)))

	  ((and (<= (+ (x p) (* pw-div 2)) b-mid)
		(>= (+ (x p) (* pw-div 3)) b-mid)
		(<= (y p) (+ (y b) (h b)))
		(>= (y p) (- (y b) (h b))))
	   (progn (setf (dy b) (- (dy b)))
		  (setf (dx b) 1)
		  (play-sound 1)))
	  
	  ((and (<= (+ (x p) (* pw-div 3)) b-mid)
		(>= (+ (x p) (w p)) b-mid)
		(<= (y p) (+ (y b) (h b)))
		(>= (y p) (- (y b) (h b))))
	   (progn (setf (dy b) (- (dy b)))
		  (if (sdl:get-key-state :sdl-key-right)
		      (setf (dx b) 3)
		      (setf (dx b) 2))
		  (play-sound 1)))

	  (t (progn (setf (dy b) (- (dy b)))
		    (setf (dx b) 0))))))


;;;;;;;;;;;;;;;;;;;;;;;; SCORING ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; UPDATE-SCORE function

(defun update-score (brick x y)

  (unless (equalp brick #\8)
    (progn (setf (aref *level* y x) #\0)
	   (setf *bricks-in-level* (decf *bricks-in-level*))))

  (cond ((equalp brick #\1)
	 (progn (play-sound 0)
		(setf *player-score* (+ *player-score* (* 10 *player-lives*)))))

	((equalp brick #\2)
	 (progn (play-sound 0)
		(setf *player-score* (+ *player-score* (* 20 *player-lives*)))))

	((equalp brick #\3)
	 (progn (play-sound 0)
		(setf *player-score* (+ *player-score* (* 30 *player-lives*)))))

	((equalp brick #\4)
	 (progn (play-sound 0)
		(setf *player-score* (+ *player-score* (* 40 *level-number*)))))

	((equalp brick #\5)
	 (progn (play-sound 0)
		(setf *player-score* (+ *player-score* (* 50 *player-lives*)))))

	((equalp brick #\6)
	 (progn (play-sound 0)
		(setf *player-score* (+ *player-score* (* 60 *player-lives*)))))

	((equalp brick #\7)
	 (progn (play-sound 0)
		(setf *player-score* (+ *player-score* (* 70 *player-lives*))))))

  (if (zerop *bricks-in-level*)
      (progn (setf *player-score* 
		   (+ *player-score* (* 1000 *player-lives* *level-number*)))
	     (setf *level-number* (incf *level-number*))
	     (play-sound 3)
	     (load-new-level))))


;;;;;;;;;;;;;;;;;;;;;;;; PLAYER ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; CREATE-PLAYER function

(defun create-player ()
  (setf *player* (make-instance 'paddle
				:x (/ *game-width* 2) :y (- *game-height* 50)
				:w 60 :h 12
				:r 255 :g 0 :b 0
				:spd 5)))


;;;; RESET-PLAYER function

(defun reset-player ()
  (setf (x *player*) (- (/ *game-width* 2) (/ (w *player*) 2))))


;;;; DISPLAY-PLAYER function

(defun display-player (p)
  (draw-box (x p) (y p) (w p) (h p) (r p) (g p) (b p)))


;;;; MOVE-PLAYER function

(defun move-player (direction)
  (unless (= *pause* 1) 
    (cond ((eq direction 'left)
	   (unless (<= (x *player*) -5)
	     (setf (x *player*) (- (x *player*) (spd *player*)))))
	  
	  ((eq direction 'right)
	   (unless (>= (+ (x *player*) (w *player*)) (+ *game-width* 5))
	     (setf (x *player*) (+ (x *player*) (spd *player*))))))))


;;;;;;;;;;;;;;;;;;;;;;;; SCREENS ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-UI function

(defun display-ui ()
  (draw-box 0 (- *game-height* 30) *game-width* *game-height* 0 0 0)
  (draw-line 0 (- *game-height* 30) *game-width* (- *game-height* 30) 20 20 20)
  
  (draw-text "Level:" 20 (- *game-height* 25) 255 255 255 *ttf-font-normal*)
  (draw-text (format nil "~a" *level-number*) 85 (- *game-height* 25) 255 255 0 *ttf-font-normal*)

  (draw-text "Score:" 150 (- *game-height* 25) 255 255 255 *ttf-font-normal*)
  (draw-text (format nil "~a" *player-score*) 215 (- *game-height* 25) 255 255 0 *ttf-font-normal*)

  (draw-text (format nil "Lives: ~a" *player-lives*) 
	     375 (- *game-height* 25) 255 255 255 *ttf-font-normal*))


;;;; DISPLAY-END-GAME function

(defun display-end-game ()
  (sdl:draw-surface-at-* (sdl:load-image *gfx-game-over*) 0 0)

  (draw-text "BREAKOUT" 180 20 255 255 0 *ttf-font-huge*)

  (if (and (zerop *bricks-in-level*) (= *level* 10))
      (draw-text "CONGRATULATIONS!!!  YOU WON!!!" 100 100 255 255 0 *ttf-font-huge*)
      (progn (draw-text "GAME OVER!" 160 130 255 255 255 *ttf-font-huge*)
	     (draw-text (format nil "YOUR SCORE IS ~a" *player-score*) 
			     100 250 255 0 0 *ttf-font-huge*)))
  (draw-text "Press SPACE to Continue..." 120 560 255 255 255))


;;;; DISPLAY-MENU function

(defun display-menu ()
  (sdl:draw-surface-at-* (sdl:load-image *gfx-breakout*) 0 0)
  ;(sdl:draw-surface-at-* (sdl:load-image *gfx-logo*) 5 50)
  ;(draw-text "BREAKOUT" 180 20 255 255 0 *ttf-font-huge*)
  (draw-text "Press SPACE to Start..." 120 560 255 255 255))


;;;;;;;;;;;;;;;;;;;;;;;; GAME STATE ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; CONTINUE-OPTION function

(defun continue-option ()
  (cond ((zerop *game-state*) (change-game-state))
	((= *game-state* 2) (change-game-state))
	(t ())))


;;;; CHANGE-GAME-STATE function

(defun change-game-state ()
  (cond ((zerop *game-state*) (progn (reset-game)
				     (setf *game-state* 1)))
	((= *game-state* 1) (setf *game-state* 2))
	((= *game-state* 2) (setf *game-state* 0))
	(t ())))


;;;;;;;;;;;;;;;;;;;;;;;; THE GAME ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; RENDER function

(defun render ()
  (update-swank)
  (sdl:clear-display sdl:*black*)

  (cond ((= *game-state* 1)
	 (display-background)
	 (display-level)
	 (if (zerop *pause*)
	     (update-ball *ball* *player*))
	 (display-player *player*)
	 (display-ball *ball*)
	 (if (= *get-ready* 1)
	     (display-get-ready))
	 (display-ui))

	((= *game-state* 2)
	 (display-end-game))

	(t (display-menu)))

  (sdl:update-display))


;;;; RESET-GAME function

(defun reset-level ()
  (reset-player)
  (reset-ball *ball*)
  (setf *get-ready-count* 0)
  (setf *get-ready* 1)
  (setf *pause* 1))
  ;(play-sound 5))


;;;; LOAD-NEW-LEVEL function

(defun load-new-level ()
  (setf *bricks-in-level* 0)
  (load-level *level-number*)
  (reset-player)
  (reset-ball *ball*))


;;;; RESET-GAME function

(defun reset-game ()
  (setf *player-score* 0)
  (setf *player-lives* 3)
  (setf *level-number* 1)
  (setf *get-ready-count* 0)
  (setf *get-ready* 1)
  (setf *pause* 1)
  (load-new-level))
  

;;;; INITIALIZE-GAME function

(defun initialize-game ()
  (setf *game-state* 0)
  (create-player)
  (create-ball))


;;;; SETUP-FONTS function

(defun setup-fonts ()
  (unless (sdl:initialise-default-font *terminus-ttf-18*)
    (error "FONT-EXAMPLE: Cannot initialize the default font."))

  (setf *ttf-font-small* (sdl:initialise-font *terminus-ttf-12*))
  (setf *ttf-font-normal* (sdl:initialise-font *terminus-ttf-18*))
  (setf *ttf-font-large* (sdl:initialise-font *terminus-ttf-24*))
  (setf *ttf-font-huge* (sdl:initialise-font *terminus-ttf-32*)))


;;;; SETUP-AUDIO function

(defun setup-audio ()
  (setf *soundfx* (make-array 7))
  (sdl-mixer:init-mixer :mp3)
  (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO :chunksize 1024 :enable-callbacks nil))
  (when *mixer-opened*
    (setf (aref *soundfx* 0) (sdl-mixer:load-sample (sdl:create-path "zap_1.ogg" *audio-root*)))
    (setf (aref *soundfx* 1) (sdl-mixer:load-sample (sdl:create-path "ping_1.ogg" *audio-root*)))
    (setf (aref *soundfx* 2) (sdl-mixer:load-sample (sdl:create-path "wall_bounce_1.ogg" *audio-root*)))
    (setf (aref *soundfx* 3) (sdl-mixer:load-sample (sdl:create-path "level_complete.ogg" *audio-root*)))
    (setf (aref *soundfx* 4) (sdl-mixer:load-sample (sdl:create-path "die.ogg" *audio-root*)))
    (setf (aref *soundfx* 5) (sdl-mixer:load-sample (sdl:create-path "ready_go_2.ogg" *audio-root*)))
    (setf (aref *soundfx* 6) (sdl-mixer:load-sample (sdl:create-path "game_over_1.ogg" *audio-root*)))
    (sample-finished-action)
    (sdl-mixer:allocate-channels 16)))


;;; SAMPLE-FINISHED-ACTION function

(defun sample-finished-action ()
  (sdl-mixer:register-sample-finished
   (lambda (channel)
     (declare (ignore channel))
     nil)))


;;;; CLEAN-UP function

(defun clean-up ()
  (when (sdl-mixer:sample-playing-p nil)
    (sdl-mixer:pause-sample t)
    (sdl-mixer:Halt-sample :channel t))

  (loop for s below (length *soundfx*)
     do (if (equal (aref *soundfx* s) 0)
	    t
	    (progn (sdl:free (aref *soundfx* s))
		   (setf (aref *soundfx* s) 0))))
  
  (when *mixer-opened*
    (sdl-mixer:Close-Audio t)
    (setf *mixer-opened* nil))
  (sdl-mixer:quit-mixer))


;;;; START function

(defun start ()
  (initialize-game)
  (reset-game)
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (sdl:window *game-width* *game-height* :title-caption "Breakout")
    (setf (sdl:frame-rate) 60)

    (setup-audio)

    (setup-fonts)
    
    (sdl:with-events ()
      (:quit-event ()
		   (clean-up)
		   t)
      (:key-down-event (:key key)
		       (case key
			 (:sdl-key-q (if (= *game-state* 1)
					 (change-game-state)))
			 (:sdl-key-space (continue-option))
			 (:sdl-key-escape (sdl:push-quit-event))))
      (:key-up-event (:key key)
		     (case key))
      (:idle ()
	     (when (sdl:get-key-state :sdl-key-left) (move-player 'left))
	     (when (sdl:get-key-state :sdl-key-right) (move-player 'right))
	     (render)))))
