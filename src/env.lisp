(in-package #:orca)

(defvar *pss-environments* nil
  "List containing the current known environments")
(defvar *pss-leases* nil
  "Alist containing the leases people have done (NICK ENV ACTIVITY EXPIRE-TIME)")
(defvar *pss-env-statuses* nil
  "Alist of environment/status")

(defun save-env-data ()
  (with-open-file (ouf (orca-path "data/pss-envs.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-line ";; Environments" ouf)
    (write *pss-environments* :stream ouf)
    (terpri ouf)
    (write-line ";; Leases" ouf)
    (write *pss-leases* :stream ouf)
    (terpri ouf)
    (write-line ";; Statuses" ouf)
    (write *pss-env-statuses* :stream ouf)
    (terpri ouf)))

(defun load-env-data ()
  (with-open-file (inf (orca-path "data/pss-envs.lisp") :direction :input)
    (setf *pss-environments* (read inf nil nil))
    (setf *pss-leases* (read inf nil nil))
    (setf *pss-env-statuses* (read inf nil nil))))

(defun expire-env-leases ()
  (let ((now (get-universal-time)))
    (setf *pss-leases* (delete-if (lambda (lease)
                                    (<= (fourth lease) now))
                                  *pss-leases*))))

(defun describe-time-left (expire-time)
  (let ((span (- expire-time (get-universal-time))))
    (cond
      ((> span 86400)
       (format nil "~ad" (floor span 86400)))
      ((> span 3600)
       (format nil "~ah" (floor span 3600)))
      ((> span 60)
       (format nil "~am" (floor span 60)))
      (t
       (format nil "~as" span)))))

(defun parse-expire-time (str)
  (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings "^(\\d+)([dhms]?)$" str)
    (when match
      (let ((num (parse-integer (aref regs 0))))
        (+ (get-universal-time)
           (if (zerop (length (aref regs 1)))
               (* num 60)
               (ecase (char (aref regs 1) 0)
                 (#\d (* num 86400))
                 (#\h (* num 3600))
                 (#\m (* num 60))
                 (#\s num))))))))

(defun query-leases (env-name leases)
  (let ((matching (remove env-name leases :test-not #'string-equal :key #'cadr))
        (status (cdr (assoc env-name *pss-env-statuses* :test #'string-equal))))
    (if matching
        (with-output-to-string (str)
          (format str "~a~@[(~a)~] used by: " env-name status)
          (loop
             with end = (last matching)
             for match-el on matching
             for match = (car match-el)
             do
             (format str "~a (~@[~a/~]~a)" (first match)
                     (third match)
                     (describe-time-left (fourth match)))
             (unless (eq match-el end)
               (format str ", "))))
        (format nil "~a~@[(~a)~] is currently free" env-name status))))

(defun remove-all-env-leases (env-name)
  (setf *pss-leases* (remove env-name *pss-leases* :test #'string-equal :key #'second)))

(defun remove-env-lease (env-name nick)
  (setf *pss-leases* (remove-if (lambda (lease)
                                  (and (string-equal (second lease) env-name)
                                       (string-equal (first lease) nick)))
                                *pss-leases*)))

(defun create-lease (env-name nick time-str activity)
  (let ((expire-time (parse-expire-time time-str)))
    (cond
      ((not (member env-name *pss-environments* :test #'string-equal))
       (format nil "Can't find environment ~a" env-name))
      ((null expire-time)
       (format nil "Sorry, didn't understand ~a as an amount of time" time-str))
      (t
       (remove-env-lease env-name nick)
       (push (list nick env-name activity expire-time) *pss-leases*)
       (save-env-data)
       (format nil "~a taken by ~a for ~a"
               env-name
               nick
               (describe-time-left expire-time))))))

(defun release-lease (env-name nick)
  (cond
    ((not (member env-name *pss-environments* :test #'string-equal))
     (format nil "Can't find environment ~a" env-name))
    (t
     (remove-env-lease env-name nick)
     (save-env-data)
     (format nil "~a released by ~a" env-name nick))))

(defun set-env-status (env-name status)
  (cond
    ((not (member env-name *pss-environments* :test #'string-equal))
     (format nil "Can't find environment ~a" env-name))
    ((zerop (length status))
     (setf *pss-env-statuses* (remove (assoc env-name *pss-env-statuses* :test #'string-equal)
                                      *pss-env-statuses*))
     (save-env-data)
     (format nil "~a status removed" env-name))
    (t
     (let ((tuple (assoc env-name *pss-env-statuses* :test #'string-equal)))
       (if tuple
           (setf (cdr tuple) status)
           (setf *pss-env-statuses* (acons env-name status *pss-env-statuses*))))
     (save-env-data)
     (format nil "~a status is now: ~a" env-name status))))

(define-serious-command env (message directp &rest args)
  (expire-env-leases)
  (let ((env-name (first args))
        (subcmd (second args)))
    (cond
      ((null args)
       (reply-to message "~{~a~^, ~}" *pss-environments*))
      ((string-equal env-name "help")
       (flet ((send (text)
                (irc:notice (connection message) (source message) text)))
         (send "~env                                    - list known environments")
         (send "~env <envname>                          - display environment info")
         (send "~env <envname> take <time> [<activity>] - lease the environment")
         (send "~env <envname> release                  - release your lease on environment")
         (send "~env <envname> add                      - add a new environment")
         (send "~env <envname> remove                   - remove an environment")
         (send "~env <envname> status [<status>]        - set the environment status")
         (send "~env <envname> help                     - display this help")))
      ((null (cdr args))
       ;; query leases
       (if (member env-name *pss-environments* :test #'string-equal)
           (reply-to message (query-leases (first args) *pss-leases*))
           (reply-to message "Can't find environment ~a" env-name)))
      ((string-equal subcmd "take")
       (reply-to message (create-lease env-name (source message)
                                       (third args)
                                       (join-string #\space (cdddr args)))))
      ((string-equal subcmd "release")
       (reply-to message (release-lease env-name (source message))))
      ((string-equal subcmd "status")
       (reply-to message (set-env-status env-name (join-string #\space (cddr args)))))
      ((string-equal subcmd "add")
       (cond
         ((member env-name *pss-environments* :test #'string-equal)
          (reply-to message "That environment is already known."))
         (t
          (push env-name *pss-environments*)
          (setf *pss-environments* (sort *pss-environments* #'string<))
          (save-env-data)
          (reply-to message "Added ~a." env-name))))
      ((string-equal subcmd "remove")
       (cond
         ((not (member env-name *pss-environments* :test #'string-equal))
          (reply-to message "Can't find environment ~a" env-name))
         (t
          (setf *pss-environments* (remove env-name *pss-environments* :test #'string-equal))
          (save-env-data)
          (reply-to message "Removed ~a from environments." env-name)))))))