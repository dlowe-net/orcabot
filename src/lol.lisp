(in-package #:orca)

(defvar *lol-db* (make-hash-table :test 'equalp))

(defun load-lol-db (path)
  (clrhash *lol-db*)
  (with-open-file (inf path :direction :input)
    (loop
       for tuple = (read inf nil)
       while tuple
       do (setf (gethash (first tuple) *lol-db*) (second tuple)))))

(defun lol-translate (text)
  (cl-ppcre:regex-replace-all
   "(\\s)\\1+"
   (cl-ppcre:regex-replace-all
    "\\w+" text
    (lambda (target start end match-start match-end reg-starts reg-ends)
      (declare (ignore start end reg-starts reg-ends))
      (let ((match (make-array (list (- match-end match-start)) :element-type 'character :displaced-to target :displaced-index-offset match-start)))
        (gethash match *lol-db* match))))
   "\\1"))

(defcommand lolsay (message directp &rest text)
  (reply-to message (lol-translate (join-string #\space text))))

(defcommand lolize (message directp nick)
  (let ((last-said (gethash nick *last-said*)))
    (if last-said
        (reply-to message "<~a> ~a" nick (lol-translate (fourth last-said))
        (reply-to message "~a: I no haz teh werds of ~a" nick)))))