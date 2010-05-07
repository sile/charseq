(defpackage charseq
  (:use :common-lisp)
  (:shadow length subseq = < > <= >= /=)
  (:export bounding-indices-bad-error
       invalid-index-error
       index
       charseq
      
       make
       ref
       length
       each
       sub
       
       = < > <= >= /=
       to-string
       as-string))
(in-package :charseq)

(declaim (inline to-simple make-charseq make ref length to-string sub = < > <= >= /=))
     
(deftype index () '(integer 0 #.array-total-size-limit))

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
               (format stream "Index ~A out of bounds of (~A ~A), should be nonnegative and <~A."
                       index 'charseq length length)))))
 
(defstruct (charseq (:conc-name ""))
  (str "" :type (simple-array character) :read-only t)
  (beg 0  :type index              :read-only t)
  (end 0  :type index              :read-only t))

(defun to-simple (source start end)
  (declare (string source)
       (index start end))
  (let ((dest (make-array (- end start) :element-type 'character)))
    (loop FOR i OF-TYPE index FROM 0
      FOR j OF-TYPE index FROM start BELOW end DO
      (setf (char dest i) (char source j)))
    dest))

(defun make (string &key (start 0) (end (common-lisp:length string)))
  (declare (string string)
       (index start end))
  (unless (common-lisp:<= start end (common-lisp:length string))
    (error 'bounding-indices-bad-error
       :length (common-lisp:length string) :start start :end end))
 
  (if (typep string '(simple-array character))
      (make-charseq :str string :beg start :end end)
    (make-charseq :str (to-simple string start end) :beg 0 :end (- end start))))

(defun length (#1=charseq)
  (declare (#1# #1#))
  (- (end #1#) (beg #1#)))

(defmacro with-check ((test-form datum &rest arguments) &body body &environment env)
  ;; TODO: ccl
  (if #+SBCL (eql 0 (cdr (assoc 'common-lisp:safety (sb-c::lexenv-policy env))))
      #-SBCL nil
      `(locally ,@body)
    `(progn 
       (unless ,test-form
         (error ,datum ,@arguments))
       (locally
	,@body))))
    
(defun ref (#1=charseq index)
  (declare (#1# #1#) (index index))
  (char (str #1#) (+ (beg #1#) index)))

(define-compiler-macro ref (charseq index)
  (let ((cs (gensym))
	(i  (gensym)))
    `(let ((,cs ,charseq)
           (,i  ,index))
       (declare (charseq:charseq ,cs) (charseq:index ,i))
       (with-check ((common-lisp:< ,i (charseq:length ,cs))
                    'invalid-index-error :length (charseq:length ,cs) :index ,i)
         (char (str ,cs) (+ (beg ,cs) ,i))))))

(defmacro each ((char charseq &optional result) &body body)
  (let ((tmp (gensym))
    (str (gensym))
    (i   (gensym)))
    `(let* ((,tmp ,charseq)
        (,str (str ,tmp)))
       (loop FOR ,i FROM (beg ,tmp) BELOW (end ,tmp)
         FOR ,char = (char ,str ,i)
        DO ,@body
    FINALLY (return ,result)))))

(defun sub (#1=charseq start &optional end)
  (declare (#1# #1#)
       (index start)
       ((or null index) end))
  (let ((new-beg (+ (beg #1#) start))
    (new-end (+ (beg #1#) (or end (length #1#)))))
    (unless (common-lisp:<= new-beg new-end (end #1#))
      (error 'bounding-indices-bad-error :length (length #1#) :start start :end end))
    (make (str #1#) :start new-beg :end new-end)))

(defun to-string (#1=charseq)
  (declare (#1# #1#))
  (common-lisp:subseq (str #1#) (beg #1#) (end #1#)))

(defmacro as-string ((string start end) charseq &body body)
  (let ((tmp (gensym)))
    `(let* ((,tmp ,charseq)
        (,string (str ,tmp))
        (,start  (beg ,tmp))
        (,end    (end ,tmp)))
       (declare (ignorable ,string ,start ,end))
       ,@body)))

(eval-when (:compile-toplevel :load-toplevel)
  (defun mksym (&rest args)
    (intern (with-output-to-string (out)
              (dolist (a args) (princ a out))))))

(defmacro def-string-cmp (name)
  `(defun ,name (#1=charseq1 #2=charseq2)
     (declare (charseq #1# #2#))
     (,(mksym 'string name)
      (str #1#) (str #2#) 
      :start1 (beg #1#) :end1 (end #1#)
      :start2 (beg #2#) :end2 (end #2#))))

(def-string-cmp =)
(def-string-cmp /=)
(def-string-cmp <)
(def-string-cmp >)
(def-string-cmp <=)
(def-string-cmp >=)
