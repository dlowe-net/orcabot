(in-package #:orcabot)

;; The abbreviation db has two parts - WORDS and PHRASES
;;   WORDS are a list of (kind . word) pairs.
;;   PHRASES are a list of phrases, and a phrase is a list of word kinds.

(defclass abbrev-db ()
  (words phrases))

(defun load-abbrev-db (path)
  (let ((*package* (find-package '#:orcabot)))
    (with-open-file (inf path :direction :input)
      (let ((result (make-instance 'abbrev-db )))
        (with-slots (words phrases) result
          (setf words nil
                phrases nil)
          (loop
             for term = (read inf nil)
             while term
             do
               (case (car term)
                 (phrase
                  (push (cdr term) phrases))
                 (t
                  (dolist (word (cdr term))
                    (push (cons (car term) word)
                          words)))))
          result)))))

(defun count-syms (list)
  (count-if #'symbolp list))

(defun potential-phrases (db wanted-len)
  (with-slots (phrases) db
    (alexandria:shuffle (remove wanted-len phrases :key #'count-syms :test-not #'=))))

(defun select-word (db kind letter used)
  (let ((candidates (with-slots (words) db
                      (loop
                         for word in words
                         when (and (eql (car word) kind)
                                   (char-equal (char (cdr word) 0) letter)
                                   (not (gethash (cdr word) used)))
                         collect (cdr word)))))
    (cond
      (candidates
       (alexandria:random-elt candidates))
      (t
       (log:log-message :error "No unused word for letter '~a' and type ~a~%" letter kind)
       nil))))

(defun render-phrase-template (db template abbrev)
  (let ((result nil)
        (used (make-hash-table :test 'equal)))
    (loop
       with letter-idx = 0
       for word-kind in template
       for letter = (char abbrev letter-idx)
       do
         (etypecase word-kind
           (string
            (push word-kind result))
           (symbol
            (let ((word (select-word db word-kind letter used)))
              (unless word
                (return nil))
              (push (string-capitalize word :end 1) result)
              (setf (gethash word used) t)
              (incf letter-idx))))
       finally (return (nreverse result)))))

(defun expand-abbrev (db abbrev)
  (loop
     for phrase-template in (potential-phrases db (length abbrev))
     as phrase = (render-phrase-template db phrase-template abbrev)
     until phrase
     finally (return (and phrase
                          (join-to-string #\space phrase)))))

(defmodule abbrev abbrev-module ("robot")
  (robot-db :accessor robot-db-of))

(defmethod initialize-module ((module abbrev-module) config)
  (setf (robot-db-of module) (load-abbrev-db (static-path "robots.lisp"))))

(defmethod handle-command ((module abbrev-module)
                           (cmd (eql 'robot))
                           message args)
  "robot <word> - generate robot name from word (max 11 chars)"
  (cond
    ((null args)
     (reply-to message "Usage: .robot <word>"))
    ((> (length (first args)) 11)
     (reply-to message "The word must be less than 11 characters."))
    (t
     (reply-to message "~a: ~a~%"
               (first args)
               (expand-abbrev (robot-db-of module) (first args))))))