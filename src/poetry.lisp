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

(defun string-to-pattern (dict s)
  (join-to-string ""
                  (loop for word in (ppcre:split "\\s" s)
                        as pattern = (gethash (string-downcase word) dict)
                        collect (cond
                                  ((null pattern)
                                   ;; fail if a word can't be found.  We don't
                                   ;; want any false positives.
                                   (log:log-message :info "~a not found" word)
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

(defmethod examine-message ((module poetry-module) (message irc:irc-privmsg-message))
  (dolist (sentence (ppcre:split " *[.?!] *" (second (arguments message))))
    (let ((pattern (string-to-pattern (syllables-of module) sentence)))
      (when pattern
        (cond
          ((pattern-matches-p +tmnt-pattern+ pattern)
           ;; TEENAGE MUTANT NINJA TURTLES
           (reply-to message "~a" (string-upcase sentence)))
          ((pattern-matches-p +camptown-pattern+ pattern)
           ;; Camptown ladies sing this song
           (reply-to message "doo-dah doo-dah"))
          ((= (length pattern) 17)
           ;; Haiku
           (reply-to message "~a said a haiku!" (source message))))))))
