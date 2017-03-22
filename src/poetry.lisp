(in-package #:orcabot)

(defmodule poetry poetry-module ()
  (syllables :accessor syllables-of))

(defmethod initialize-module ((module poetry-module) config)
  (setf (syllables-of module) (read-syllable-dict (static-path "syllable-dict.txt"))))

(defparameter +tmnt-pattern+ "10101010")
(defparameter +camptown-pattern+ "1010101")

(defun read-syllable-dict (path)
  (let ((dict (make-hash-table :test 'equal)))
    (with-open-file (inf path)
      (loop
        for line = (read-line inf nil)
        while line
        unless (or (string= "" line)
                   (string= "#" line :end2 1))
          do
             (let ((word-pronounciation (ppcre:split " " line :limit 2)))
               (setf (gethash (string-downcase (first word-pronounciation)) dict)
                     (map 'string (lambda (c) (if (char= c #\2) #\. c)) ; secondary stress can be either
                          (remove-if-not #'digit-char-p ; remove pronounciation
                                         (second word-pronounciation)))))
        finally (return dict)))))

(defun lookup-syllables (dict word)
  (gethash (remove-if-not (lambda (c)
                            (or (alpha-char-p c)
                                (find c "'")))
                          (string-downcase word))
           dict))

(defun string-to-pattern (dict s)
  (join-to-string ""
                  (loop for word in (ppcre:split "\\s" s)
                        as pattern = (lookup-syllables dict word)
                        collect (cond
                                  ((null pattern)
                                   ;; fail if a word can't be found.  We don't
                                   ;; want any false positives.
                                   (return-from string-to-pattern nil))
                                  ((= (length pattern) 1)
                                   ;; single words can match stressed or unstressed parts
                                   ".")
                                  (t
                                   pattern)))))

(defun pattern-matches-p (pattern text)
  (when (= (length pattern) (length text))
    (loop for pattern-char across pattern
          for text-char across text
          when (and (not (char= pattern-char #\.))
                    (not (char= text-char #\.))
                    (char/= pattern-char text-char))
            do (return nil)
          finally (return t))))

(defun consume-n-syllables (tuples n)
  (let* ((line (loop
                for tuple on tuples
                as total = (cdr (first tuple)) then (+ total (cdr (first tuple)))
                while (< total n)
                collect (car (pop tuples)) into consumed
                finally (return (and (= total n)
                                     (append consumed (list (car (pop tuples)))))))))
    (values line tuples)))

(defun try-haiku (dict text)
  "Returns a string displaying the haiku if the text has words adding
  to 5 syllables, then 7, then 5."
  (let ((counts (loop for word in (ppcre:split "\\s" text)
                      collect (cons word (length (lookup-syllables dict word))))))
    (cond
      ((some (lambda (c) (zerop (cdr c))) counts) 
       ;; no false positives
       (return-from try-haiku nil))
      ((/= (reduce '+ counts :key 'cdr) 17)
       ;; All haiku have 17 syllables
       (return-from try-haiku nil))
      (t
       (with-output-to-string (result)
         (let (words-done words-left)
           (multiple-value-setq (words-done words-left)
             (consume-n-syllables counts 5))
           (unless words-done
             (return-from try-haiku nil))
           (write-string (join-to-string " " words-done) result)
           (write-string " / " result)
           (multiple-value-setq (words-done words-left)
             (consume-n-syllables words-left 7))
           (unless words-done
             (return-from try-haiku nil))
           (write-string (join-to-string " " words-done) result)
           (write-string " / " result)
           (multiple-value-setq (words-done words-left)
             (consume-n-syllables words-left 5))
           (unless words-done
             (return-from try-haiku nil))
           (write-string (join-to-string " " words-done) result)))))))

(defmethod examine-message ((module poetry-module) (message irc:irc-privmsg-message))
  ;; Haikus get first dibs.
  (let ((haiku (try-haiku (syllables-of module) (second (arguments message)))))
    (when haiku
      (reply-to message "~a made a haiku!  ~a" (source message) haiku)
      (return-from examine-message)))
  
  (dolist (sentence (ppcre:split " *[.?!] *" (second (arguments message))))
    (let ((pattern (string-to-pattern (syllables-of module) sentence)))
      (when pattern
        (cond
          ((pattern-matches-p +tmnt-pattern+ pattern)
           ;; TEENAGE MUTANT NINJA TURTLES
           (reply-to message "~a" (string-upcase sentence)))
          ((pattern-matches-p +camptown-pattern+ pattern)
           ;; Camptown ladies sing this song
           (reply-to message "doo-dah doo-dah")))))))
