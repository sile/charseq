(defpackage charseq
  (:use :common-lisp)
  (:shadow length subseq = < > <= >= /=)
  (:export invalid-index-error bounding-indices-bad-error
	   charseq index
	   make with-dynamic-extent
	   ref length each sub 
	   = < > <= >= /=
	   to-string as-string))
(in-package :charseq)

;;;;;;;;;;;
;;; declaim
(declaim (inline to-simple make-charseq make ref length to-string sub = < > <= >= /=))

;;;;;;;;
;;; type
(deftype index () '(integer 0 #.array-total-size-limit))

;;;;;;;;;;;;;
;;; condition
(define-condition bounding-indices-bad-error (error)
  ((length :initarg :length)
   (start  :initarg :start)
   (end    :initarg :end))
  (:report (lambda (condition stream)
	     (with-slots (length start end) condition
	       (format stream "The bounding indices ~A and ~A are bad for a string of length ~A."
		       start end length)))))

(define-condition invalid-index-error (error)
  ((index  :initarg :index)
   (length :initarg :length))
  (:report (lambda (condition stream)
             (with-slots (length index) condition
               (format stream "Index ~A out of bounds of (~A ~A), should be nonnegative and ~:*<~A."
                       index 'charseq length)))))
;;;;;;;;;; 
;;; struct
(defstruct (charseq (:conc-name ""))
  (str "" :type (simple-array character) :read-only t)
  (beg 0  :type index                    :read-only t)
  (end 0  :type index                    :read-only t))

;;;;;;;;;;;;;;;;;;;;;
;;; internal function
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun safety-optimize-quantity (env)
    #+SBCL (cdr (assoc 'common-lisp:safety (sb-c::lexenv-policy env)))
    #+CLOZURE (ccl::safety-optimize-quantity env)
    #-(or SBCL CLOZURE) 1)

  (defun mksym (&rest args)
    (intern (with-output-to-string (out)
              (dolist (a args) (princ a out))))))

(defmacro assert-when-safety>0 (test-form datum &rest arguments &environment env)
  (if (eql 0 (safety-optimize-quantity env))
      '()
    `(unless ,test-form
       (error ,datum ,@arguments))))

(defmacro def-charseq-cmp (name)
  `(defun ,name (#1=charseq1 #2=charseq2)
     (declare (charseq #1# #2#))
     (,(mksym 'string name) (str #1#) (str #2#) 
                            :start1 (beg #1#) :end1 (end #1#)
			    :start2 (beg #2#) :end2 (end #2#))))

(defun to-simple (source start end)
  (declare (string source)
	   (index start end)
	   #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((dest (make-array (- end start) :element-type 'character)))
    (loop FOR i OF-TYPE index FROM 0
	  FOR j OF-TYPE index FROM start BELOW end DO
      (setf (char dest i) (char source j)))
    dest))

;;;;;;;;;;;;;;;;;;;;;
;;; exported function
(defun make (string &key (start 0) (end (common-lisp:length string)))
  (declare (string string)
	   (index start end))
  (assert-when-safety>0 (common-lisp:<= start end (common-lisp:length string))
			'bounding-indices-bad-error
			:length (common-lisp:length string) :start start :end end)
  (if (typep string '(simple-array character))
      (make-charseq :str string :beg start :end end)
    (make-charseq :str (to-simple string start end) :beg 0 :end (- end start))))

(defmacro with-dynamic-extent ((charseq string &key (start 0) (end `(common-lisp:length ,string)))
			       &body body)
  (multiple-value-bind (str-var beg-var end-var) (values #1=(gensym)#1##1#)
    `(let ((,str-var ,string)
	   (,beg-var ,start)
	   (,end-var ,end))
       (declare (string ,str-var)
		(index ,beg-var ,end-var))
       (assert-when-safety>0 (common-lisp:<= ,beg-var ,end-var (common-lisp:length ,str-var))
			     'bounding-indices-bad-error
			     :start ,beg-var :end ,end-var :length (common-lisp:length ,str-var))
       (multiple-value-bind (,str-var ,beg-var ,end-var)
           (if (typep ,str-var '(simple-array character))
	       (values ,str-var ,beg-var ,end-var)
	     (values (to-simple ,str-var ,beg-var ,end-var) 0 (- ,end-var ,beg-var)))
         (let ((,charseq (make-charseq :str ,str-var :beg ,beg-var :end ,end-var)))
	   (declare (dynamic-extent ,charseq))
	   ,@body)))))

(defun length (#1=charseq)
  (declare (#1# #1#))
  (- (end #1#) (beg #1#)))

(defun ref (#1=charseq index)
  (declare (#1# #1#) (index index))
  (assert-when-safety>0 (common-lisp:< index (charseq:length #1#))
			'invalid-index-error :length (charseq:length #1#) :index index)
  (char (str #1#) (+ (beg #1#) index)))

(defun sub (#1=charseq start &optional (end (length #1#)))
  (declare (#1# #1#)
	   (index start end))
  (let ((new-beg (+ (beg #1#) start))
	(new-end (+ (beg #1#) end)))
    (assert-when-safety>0 (common-lisp:<= new-beg new-end (end #1#))
			  'bounding-indices-bad-error :length (length #1#) :start start :end end)
    (make (str #1#) :start new-beg :end new-end)))

(defun to-string (#1=charseq)
  (declare (#1# #1#))
  (common-lisp:subseq (str #1#) (beg #1#) (end #1#)))

(defmacro as-string ((string start end) charseq &body body)
  `(let* ((#1=#:cs ,charseq)
	  (,string (str #1#))
	  (,start  (beg #1#))
	  (,end    (end #1#)))
     (declare (ignorable ,string ,start ,end))
     ,@body))

(defmacro each ((char charseq &optional result) &body body)
  (multiple-value-bind (str beg end i) (values #1=(gensym)#1##1##1#)
    `(as-string (,str ,beg ,end) ,charseq
       #+SBCL (assert (the T (common-lisp:<= ,beg ,end (common-lisp:length ,str))))
       (loop FOR ,i FROM ,beg BELOW ,end
	     FOR ,char = (char ,str ,i)
         DO ,@body
	 FINALLY (return ,result)))))

(def-charseq-cmp =)
(def-charseq-cmp /=)
(def-charseq-cmp <)
(def-charseq-cmp >)
(def-charseq-cmp <=)
(def-charseq-cmp >=)