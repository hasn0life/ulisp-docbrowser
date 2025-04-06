/*
Doc Browser by hasn0life for M5 Cardputer - April 2025

Featuring some code from Hartmut Grawe - github.com/ersatzmoco

Licensed under the MIT license: https://opensource.org/licenses/MIT
*/

const char LispLibrary[] PROGMEM = R"lisplibrary(


(defun write-text (str)
  (with-gfx (scr)
    (princ str scr)
  )
)

(defvar SCR-W 240)
(defvar SCR-H 135)
	
(defun rgb (r g b) (logior (ash (logand r #xf8) 8) (ash (logand g #xfc) 3) (ash b -3)))
(defvar code_col   (rgb 220  220  220))
(defvar line_col   (rgb 90  90  90))
(defvar header_col (rgb 140  140  140))
(defvar border_col (rgb 63  40  0))
(defvar bg_col     (rgb 0  0  0))
(defvar cursor_col (rgb 160  60  0))

(defvar tscale 1)
(defvar leading (* 15 tscale))
(defvar cwidth (* 8 tscale))

(defun obj:window (x y w h &optional title)
(let* (	
	(set-pos_ (lambda (x_ y_) (setf x x_ y y_)))
	(set-size_ (lambda (w_ h_) (setf w w_ h h_)))
	)
	(lambda (&rest messages)
		(case (car messages)
		(x x) 
		(y y)
		(w w)
		(h h)
		(title title)
		(in-x (+ x 5))
		(in-y (if title (+ y 5 3 leading) (+ y 5)))
		(in-w (- w 5))
		(in-h (if title (- h 5 3 leading) (- h 5)))
		(set-pos (apply set-pos_ (cdr messages)))
		(set-size (apply set-size_ (cdr messages)))
		(set-title (setf title (cadr messages)))
		
		(draw-border 
			(fill-rect x y w h bg_col ) 
			(draw-rect x y w h border_col )
					
			(when title 
			  (draw-rect x y w (+ 3 leading) border_col  )
			  (set-text-color header_col bg_col )
			  (set-cursor (+ x 5)  (+ y 3))
			  (write-text title))
		)
	))
))
	
(defun obj:txtwindow (x y w h &optional title)
(let* (
		(win (obj:window x y w h title))
		(tmax-x (lambda () (- (truncate (win 'in-w) cwidth) 1)))
		(tmax-y (lambda () (truncate (win 'in-h) leading)))
	   
		(disp-line_ (lambda (line y &optional is_selected)
		  (let ((ypos (+ (win 'in-y) (* y leading)))  (myl " "))
			(when line (setf myl (concatenate 'string line myl)))
			(set-cursor (win 'in-x) ypos)
			(when (> (length myl) 0)
				(if is_selected 
					(set-text-color code_col cursor_col) 
					(set-text-color code_col bg_col ))
				(write-text (subseq myl 0 (min (length myl) (+ (tmax-x) 1))))
			))))
		)
	(lambda (&rest messages)
		(case (car messages)
		(disp-line (apply disp-line_ (cdr messages)))
		(txtmax (cons (tmax-x) (tmax-y)))
		(tmax-x (tmax-x))
		(tmax-y (tmax-y))
		(print (format t " txtmax ~a" (cons (tmax-x) (tmax-y))))
		(t (apply win messages))))))


	
(defun obj:menu (opts &optional (win (obj:txtwindow  0 0 100 100 )))
(let* (
	(scroll 0)
	(selected 0)
	(show-opts_ (lambda (opts selected scroll)
		(win 'draw-border)
		(let ((i 0) (ymax (min (win 'tmax-y) (- (length opts) scroll))))
			(loop
				(win 'disp-line (princ-to-string (nth (+ scroll i) opts)) i  (= (- selected scroll)  i))
				(incf i)
				(when (>= i ymax) (return))
			)
		)
	))
	)
	
	(lambda (&rest messages)
		(case (car messages)
		(show (show-opts_ opts selected scroll))
		(down (when (< selected (- (length opts) 1)) 
			(incf selected)
			(setf scroll (max (- selected (win 'tmax-y) -1) scroll))		
			(show-opts_ opts selected scroll)))
		(up (when (> selected 0) 
			(decf selected)
			(when (< selected scroll) (setf scroll selected)) 
			(show-opts_ opts selected scroll)))
		(select (nth selected opts))
		(opts opts)
		(set-opts (setf opts (cadr messages))
				(setf scroll 0) (setf selected 0))
		(print (format t "scroll: ~a selected: ~a txtmax ~a" scroll  selected (win 'txtmax)))
		(e (apply eval (cdr messages))) 
		(t (apply win messages)))
	)
))


(defun split-line (str len)
(let ((index 0) (lines nil))
(loop
	(if (> (length str) (+ len index)) 
		(setf lines (append lines (list (subseq str index (+ index len)))))
		(return (append lines (list (subseq str index)))))
		(incf index len))))



(defun obj:textdisplay (text &optional (win (obj:txtwindow 0 0 100 100 )) )
(let* (	
	(scroll 0)
	(show-text_ (lambda (buf scroll )
		(win 'draw-border)
		(let* ((i 0) 
			  (lines (mapcan (lambda (x) (split-line x (win 'tmax-x))) buf))
			  (ymax (min (win 'tmax-y) (- (length lines) scroll))))
			(loop
				(win 'disp-line (nth (+ scroll i) lines) i)
				(incf i)
				(when (>= i ymax) (return))
			)
		)))
	)
	
	(lambda (&rest messages)
		(case (car messages)
			(text text)
			(set-text (setf text (cadr messages)) (setf scroll 0))
			(down 
				(incf scroll) 
				(show-text_ text scroll))
			(up (when (> scroll 0) 
				(decf scroll) 
				(show-text_ text scroll)))
			(show (show-text_ text scroll))
			(print (format t "scroll: ~a txtmax ~a" scroll (win 'txtmax)))
			(t (apply win messages))
		)))) 


(defun get-doc-text (keyword)
(let ((doc-str (documentation keyword )))
	(if doc-str
		(split-string-to-list (string #\Newline)  
			(format nil "~a~%~%" doc-str)) 
		(list (concatenate 'string "No doc for " (string keyword)))
	)
))

(defun update-doc ()
(doc 'set-text (get-doc-text (menu 'select))) 
(doc 'show))

(defun update-menu ()
(menu 'set-opts (apropos-list search))
(menu 'set-title search)
(menu 'show))

(defun doc-browser ()
(let* ((lastkey nil) (exit nil) 
	(menu (obj:menu (apropos-list "") (obj:txtwindow 0 0 SCR-W (truncate (* SCR-H .33))  "")))
	(doc (obj:textdisplay (get-doc-text (menu 'select)) 
		(obj:txtwindow  0 (truncate (* SCR-H .33)) SCR-W (truncate (* SCR-H .66))  )))
	(search "")
	)
	
	(menu 'show)
	(doc 'show)
	(loop
		(setf lastkey (keyboard-get-key))
		(when lastkey 
			(case lastkey
				(218 (menu 'up) (update-doc))
				(217 (menu 'down) (update-doc))
				(216 (doc 'up))
				(215 (doc 'down))
				((or 13 10) 'enter (setf exit t) (setf lastkey nil))
				((or 3 17)  (setf exit t) (setf lastkey nil))
				((or 8 127) 
					(when (> (length search) 0) 
						(setf search (subseq search 0 (- (length search) 1)))
						(update-menu)
						(update-doc)))
				(t (when (printable lastkey)
				    (setf search (concatenate 'string search (string (code-char lastkey))))
					(update-menu)
					(update-doc)))
			)
		)
		(when exit (fill-screen) (return  (menu 'select)))
		
	)
))


;
; Helper functions
;
;

(defun split-string-to-list (delim str)
	(unless (or (eq str nil) (not (stringp str))) 
		(let* ((start 0)
          (end (search-str delim str))
          (lst nil))
			(loop
        (if (eq end nil) 
          (return (append lst (list (subseq str start))))
				  (setq lst (append lst (list (subseq str start end)))))
        (setq start (1+ end))
        (setq end (search-str delim str start))
      )
    )
  )
)


(defun printable (chr)
	(if (and (> chr 31) (< chr 127)) 
		t
		nil 
	)
)


)lisplibrary";