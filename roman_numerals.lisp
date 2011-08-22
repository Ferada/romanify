;;; in general, if you want need this in production code, there is already
;;; a FORMAT code for printing roman numerals:
;;; http://www.lispworks.com/documentation/HyperSpec/Body/22_cba.htm

;;; literal syntax for list construction
(defparameter *numerals* '((1000 . "M") (500 . "D") (100 . "C") (50 . "L") (10 . "X") (5 . "V") (1 . "I")))

(defun roman-for (num)
  (cdr (assoc num *numerals*)))

(defun base-numbers ()
  (mapcar #'car *numerals*))

;;; since you are using CONCATENATE with STRING quite often, you should
;;; probably define a helper function (which exists already in some
;;; packages, but a library dependency for that is overkill :)
(defun conc (&rest strings)
  "Concatenates a number of STRINGS."
  (apply #'concatenate 'string strings))

;;; i think for production code (and longer strings) using a STRING-STREAM
;;; like a StringBuffer in java is preferably performance-wise to multiple
;;; CONCs like below, if possible

;;; as far as i know, many/most lispers indent with emacs and i'm doing
;;; it here too.  in any case this gives consistent results ... but
;;; disregard it if you don't like it

(defun romanify (arabic-num)
  (labels ((add-roman (num str base)
	     ;; compare numbers with = or EQL (or up, i.e. EQUAL, EQUALP), EQ is
	     ;; undefined on numbers:
	     ;; http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm
	     (if (= 0 num)
		 str
		 ;; for short functionality there are often shortcuts, i.e. 1- or 1+
		 (add-roman (1- num) (conc str (roman-for base)) base)))
	   (build (bases num str)
	     (let ((base (car bases)))
	       (if (= 0 num)
		   str                                                      ; base case
		   ;; depending on your compiler, common subexpressions
		   ;; won't be eliminated, so one has to do this manually,
		   ;; if it's necessary performance-wise, that is
		   (multiple-value-bind (quotient remainder) (floor num base)
		     (cond ((and (> quotient 0) (= remainder 0))            ; base goes in evenly, append base(s)
			    (build (cdr bases) remainder (add-roman quotient str base)))
			   ((and (> quotient 0) (= remainder (1- base)))    ; base goes in evenly, append bases(s) & wierd leftover
			    (conc (add-roman quotient str base) str "I" (roman-for base)))
			   ((and (> quotient 0) (< remainder (1- base)))    ; base goes in evenly, handle leftover
			    (build (cdr bases) remainder (add-roman quotient str base)))
			   ;; also ZEROP, but this is better to read imo
			   ((and (= quotient 0) (= remainder (1- base)))    ; wierd case (ie IV)
			    (conc str "I" (roman-for base)))
			   ;; i'd indent every case the same, even here,
			   ;; so it's clear that T is the else-branch
			   (T                                               ; bottom out
			    (build (cdr bases) remainder str))))))))
    (build (base-numbers) arabic-num "")))
