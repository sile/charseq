(defpackage charseq
  (:use :common-lisp)
  (:shadow length subseq)
  (:export bounding-indices-bad-error
       index
       charseq
      
       make
       ref
       length
       each
       sub
       to-string
       as-string))
(in-package :charseq)

(declaim (inline to-simple make-charseq make ref length to-string sub))
     
(deftype index () '(integer 0 #.array-total-size-limit))

(define-condition bounding-indices-bad-error (error)
  ((length :initarg :length)
   (start  :initarg :start)
   (end    :initarg :end))
  (:report (lambda (condition stream)
         (with-slots (length start end) condition
           (format stream "The bounding indices ~A and ~A are bad for a string of length ~A."
               start end length)))))
 
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
  (unless (<= start end (common-lisp:length string))
    (error 'bounding-indices-bad-error
       :length (common-lisp:length string) :start start :end end))
 
  (if (typep string '(simple-array character))
      (make-charseq :str string :beg start :end end)
    (make-charseq :str (to-simple string start end) :beg 0 :end (- end start))))

(defun length (#1=charseq)
  (declare (#1# #1#))
  (- (end #1#) (beg #1#)))
    
(defun ref (#1=charseq index)
  (declare (#1# #1#)
       (index index))
  (char (str #1#) (+ (beg #1#) index)))

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
    (unless (<= new-beg new-end (end #1#))
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