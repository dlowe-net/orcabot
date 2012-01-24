;;; Copyright 2012 Daniel Lowe All Rights Reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package #:orca)

;; Stats need to be stored on the following triple:
;;    time    source    predicate   value

;; A total sum needs to be stored across dates.  We should be able to
;; remove ranges of times and have the total sum updated accordingly.
;;
;; The structure is an alist (keyed by date) of hash tables (keyed by
;; source) of counter blocks.  Each field in a counter block has a
;; predicate (should it be counted?) and a value (the amount to
;; increment).

(defmodule stats stats-module ("stat")
  (dated-stats :accessor dated-stats-of :initform nil)
  (counter-slots :accessor counter-slots-of)
  (curse-words :accessor curse-words-of))

(defclass stat-counters ()
  ((lines :initform 0)
   (words :initform 0)
   (happy-lines :initform 0)
   (sad-lines :initform 0)
   (curse-lines :initform 0)
   (ping-lines :initform 0)))

(defvar *curse-words* (make-hash-table :test 'equalp))

(defun write-stats (module path)
  (with-open-file (ouf path :direction :output
                       :if-exists :supersede)
    (loop for (date . date-block) in (reverse (dated-stats-of module)) do
         (loop for source being the hash-keys of date-block
              as counters = (gethash source date-block) do
              (loop for slot in (counter-slots-of module) do
                   (write (list date source slot (slot-value counters slot))
                          :stream ouf)
                   (terpri ouf))))))

(defun load-stats (module path)
  (let ((*package* (find-package '#:orca)))
    (setf (dated-stats-of module) nil)
    (with-open-file (inf path :direction :input)
      (loop for tuple = (read inf nil)
         while tuple do
           (setf (slot-value (get-stat-counter module
                                               (first tuple)
                                               (second tuple))
                             (third tuple))
                 (fourth tuple))))))

(defun load-curse-words (path)
  (clrhash *curse-words*)
  (with-open-file (inf path :direction :input)
    (loop for word = (read-line inf nil)
         while word do
         (setf (gethash word *curse-words*) t))))

(defun expire-old-stats (module)
  (let ((oldest-date (universal-to-date-int (- (get-universal-time)
                                               (* 60 60 24 7)))))
    (setf (dated-stats-of module)
          (remove oldest-date (dated-stats-of module)
                  :key #'car
                  :test #'>))))

(defmethod initialize-module ((module stats-module)
                              config)
  (declare (ignore config))
  (make-instance 'stat-counters)
  (setf (counter-slots-of module)
        (mapcar #'sb-mop:slot-definition-name
                (sb-mop:class-slots (find-class 'stat-counters))))
  (load-stats module (orca-path "data/irc-stats.lisp"))
  (load-curse-words (orca-path "data/cursewords.txt"))
  (expire-old-stats module))

(defmethod deinitialize-module ((module stats-module))
  (expire-old-stats module)
  (write-stats module (orca-path "data/irc-stats.lisp")))

(defgeneric counter-value (counter slot text))

(defmethod counter-value ((counter stat-counters)
                          (slot (eql 'lines))
                          text)
  1)

(defmethod counter-value ((counter stat-counters)
                          (slot (eql 'words))
                          text)
  (let ((word-count 0))
    (ppcre:do-matches (match-start match-end "\\w+" text)
      (incf word-count))
    word-count))

(defmethod counter-value ((counter stat-counters)
                                 (slot (eql 'happy-lines))
                                 text)
  (if (ppcre:scan "[:;X=]-?[)D]" text)
      1
      0))

(defmethod counter-value ((counter stat-counters)
                                 (slot (eql 'sad-lines))
                                 text)
  (if (ppcre:scan "[:;X=]-?[(XP]" text)
      1
      0))

(defmethod counter-value ((counter stat-counters)
                          (slot (eql 'curse-lines))
                          text)
  (ppcre:do-matches-as-strings (word "\\w+" text nil :sharedp t)
    (when (gethash word *curse-words*)
      (return-from counter-value 1)))
  0)

(defmethod counter-value ((counter stat-counters)
                          (slot (eql 'ping-lines))
                          text)
  (if (ppcre:scan "\\bping\\b" text) 1 0))

(defun universal-to-date-int (time)
  "Return the current date in integer coded form, so that numeric
comparisons hold true."
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time time)
    (declare (ignore second minute hour day daylight-p zone))
    (+ (* year 10000)
       (* month 100)
       date)))

(defun get-stat-counter (module date source)
  "Returns the stat counter associated with the date and source.  If
none has been allocated, one is created."
  (let ((date-block (cdr (assoc date (dated-stats-of module)))))
    (unless date-block
      (setf date-block (make-hash-table :test 'equal))
      (setf (dated-stats-of module)
            (sort (acons date date-block (dated-stats-of module))
                  #'> :key #'car)))
    (let ((counter (gethash source date-block)))
      (or counter
        (let ((new-counter (make-instance 'stat-counters)))
          (setf (gethash source date-block) new-counter)
          new-counter)))))

(defun analyze-sample (module date source text)
  (let ((counter (get-stat-counter module date source)))
    (dolist (slot (counter-slots-of module))
      (let ((val (counter-value counter slot text)))
        (incf (slot-value counter slot) val)))))

(defun sum-counters (module counters)
  (let ((dest (make-instance 'stat-counters)))
    (dolist (counter counters)
      (dolist (slot (counter-slots-of module))
        (incf (slot-value dest slot)
              (slot-value counter slot))))
    dest))

(defun all-source-counters (module source)
  (loop
     for (date . date-block) in (dated-stats-of module)
     as counter = (gethash source date-block)
     when counter
     collect counter))

(defun report-stats-from-counter (source counter)
  (with-slots (words lines happy-lines sad-lines curse-lines ping-lines)
      counter
    (format nil "~a - Lines: ~a, Words: ~d, Avg Words/Line: ~,1f, Happy: ~d, Sad: ~d, Cursing: ~d, ~
Pings: ~d"
            source
            lines
            words
            (float (/ words lines))
            happy-lines
            sad-lines
            curse-lines
            ping-lines)))


(defun stats-report (module source)
  (let ((all-counters (all-source-counters module source)))
    (if all-counters
        (report-stats-from-counter source (sum-counters module all-counters))
        (format nil "No stats available for '~a'." source))))


(defun analyze-logfile (module date path)
  (with-open-file (inf path :direction :input)
    (loop for line = (read-line inf nil)
         while line do
         (ppcre:register-groups-bind (nick message)
             ("^\\[ ?\\d+: ?\\d+\\] <([^_[>-]+)[^>]*> (.*)" line)
           (analyze-sample module date nick message)
           (analyze-sample module date "#ars" message)))))


(defmethod examine-message ((module stats-module)
                            (type (eql 'irc:irc-privmsg-message))
                            message)
  (let ((now (universal-to-date-int (get-universal-time)))
        (nick (ppcre:regex-replace "[_[-].*" (source message) "")))
    (analyze-sample module now nick (second (arguments message)))
    (analyze-sample module now (first (arguments message)) (second (arguments message)))))

(defmethod handle-command ((module stats-module)
                           (cmd (eql 'stat))
                           message args)
  "stat [<nick>] - show stats for the past seven days"
  (reply-to message "~a"
            (stats-report module
                          (or (first args)
                              (first (arguments message))))))