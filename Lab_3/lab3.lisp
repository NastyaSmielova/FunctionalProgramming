(load "C:/Users/pc/Documents/lisp/ltk/ltk.lisp")
(in-package :ltk)

;main class 
(defclass Vegetable()
	(
		(name :accessor name 
                      :initarg :name)
		(calory :accessor calory 
                        :initarg :calory)
		(price :accessor price 
                       :initarg :price)
	)
)
;defining basic class
(defclass Broccoli(Vegetable)())

(defclass Carrot(Vegetable)())

(defclass Pepper(Vegetable)())

(defclass Spinach(Vegetable)())


; salad as combination of vegetables
(defclass Salad()
	((vegetables :initform '() :accessor vegetables)
	)
)

:background :blue;find calory of salad
(defmethod get-calory ((s Salad))
   (sum-calories (vegetables s))
)

;add vegetable to the salad
(defmethod add ((s Salad) (v Vegetable))
	(setf (vegetables s) (cons v (vegetables s)))
)
;create new instance of salad
(defun create-salad()
     (defparameter s (make-instance 'Salad)) s
)

;calculate sum calories in the salad
(defun sum-calories (vegetables)
  (cond
   ((null vegetables) 0)
   (T(+ (calory (car vegetables)) (sum-calories (cdr vegetables))))
  )
)
;print vegetable
(defmethod print-veg((v Vegetable) out)
   (format out "~s calories ~s, price ~s; " (name v) (calory v) (price v) )
)
;find vegetable with calory in [bottom,top] from list
(defmethod find-vegetables-list (v bottom top)
  (cond
	((null v) NIL)

	(
                 (and (>= (calory (car v))bottom )(<= (calory (car v)) top))

		 (cons (car v) (find-vegetables-list (cdr v)  bottom top))
	)

	(t(find-vegetables-list (cdr v) bottom top))
    )
)
;find vegetable with calory in [bottom,top] from salad
(defmethod find-vegetables-salad ((s Salad) bottom top)
	(find-vegetables-list (vegetables s) bottom top )
)



;update total caloricity
(defun update(label-calority sld)
     (setf (text label-calority) (concatenate 'string "caloricity : " (write-to-string (get-calory sld))))
)

(defun gui() 
	(with-ltk()
		(setq sld (make-instance 'Salad))
		(let* (
		(myframe (make-instance 'frame ))
;create lables and buttons for vegetables
	            (label-broccoli (make-instance 'label :master myframe  :font "Sans-Serif"  :text " 0" ))
                    (label-carrot (make-instance 'label :master myframe  :font "Sans-Serif"  :text "0"))
                    (label-pepper (make-instance 'label :master myframe  :font "Sans-Serif" :text "0"))
	            (label-spinach (make-instance 'label :master myframe  :font "Sans-Serif" :text "0"))

                    (label-calority (make-instance 'label :master myframe  :font "Sans-Serif" :text "caloricity"))

	            (button-broccoli (make-instance 'button :master myframe :text "Broccoli"
                            	:command (lambda() 
                            			 	(add sld (make-instance 'Broccoli :name "Broccoli" :calory 150 :price 20))
                            			 	(setf (text label-broccoli) (+(parse-integer (text label-broccoli)) 1))
                                                        (update label-calority sld)

                                         )
                            )
	            )
	            (button-carrot (make-instance 'button :master myframe :text "Carrot" 
                            	:command (lambda() 
                            			 	(add sld (make-instance 'Carrot :name "Carrot" :calory 200  :price 10))
                            			 	(setf (text label-carrot) (+ (parse-integer (text label-carrot)) 1))
                                                        (update label-calority sld)

                                         )
                            )
	            )
	            (button-pepper (make-instance 'button :master myframe :text "Pepper"
                            	:command (lambda() 
                            			 	(add sld (make-instance 'Pepper :name "Pepper" :calory 20  :price 15))
                            			 	(setf (text label-pepper) (+(parse-integer (text label-pepper)) 1))
                                                        (update label-calority sld)

                                         )
                            )
	            )
	            (button-spinach (make-instance 'button :master myframe :text "Spinach"  
                            	:command (lambda() 
                            			 	(add sld (make-instance 'Spinach :name "Spinach" :calory 15  :price 45))
                            			 	(setf (text label-spinach) (+(parse-integer (text label-spinach)) 1))
                                                        (update label-calority sld)
                                         )
                            )
	            )
                    
	            
	            
	          ;  (label-sorted (make-instance 'label :master myframe ;:font "monospaced" :background "#aea79f" ;:multi-line? true :wrap-lines? true
; :text "__"))
                    (label-sorted (make-instance 'label :master myframe :text "__"))
	            (label-bounds (make-instance 'label :master myframe :text "__") )
	            (entry-bottom (make-instance 'entry :master myframe :text "0"))
	            (entry-top (make-instance 'entry :master myframe :text "250"))


	            (button-sorted (make-instance 'button :master myframe :text "sort"

	            				:command (lambda() 

	            							(setf (text label-sorted) (sort (vegetables sld) #'> :key #'price))
                                                                       
	            						 )
	            			)
	            )

	            (button-bounds (make-instance 'button :master myframe :text "bound"
	            				:command (lambda()
	            							(setf (text label-bounds) (find-vegetables-salad sld (read-from-string (text entry-bottom)) 
                                                                                                                              (read-from-string (text entry-top))))
	            						 )

	            			)
	            )
            )
		(pack myframe :padx  100 :pady 100  )

	       ; add buttons, lables  to the frame
                (pack label-calority :side :top)

	        (pack button-broccoli :side :left )
	        (pack button-carrot :side :left)
	        (pack button-pepper  :side :left)
	        (pack button-spinach :side :left )
()
                (pack label-broccoli)
         
	        (pack label-carrot )

	        (pack label-pepper )
	        (pack label-spinach)
	        (pack label-sorted :side :bottom)
	        (pack label-bounds :side :bottom)
	        (pack entry-bottom)
	        (pack entry-top)


	        (pack button-sorted :side :bottom  )
	        (pack button-bounds  :side :left)
                
;(configure myframe :background :blue)

	        ()
		)
	)
)

(gui)