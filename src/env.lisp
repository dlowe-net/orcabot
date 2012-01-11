(in-package #:orca)

(defmodule env env-module ("env")
  (environments :accessor environments-of :initform nil)
  (leases :accessor leases-of :initform nil)
  (statuses :accessor statuses-of :initform nil))

(defmethod initialize-module ((module env-module) config)
  (load-env-data module))

(defun save-env-data (module)
  (with-open-file (ouf (orca-path "data/pss-envs.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-line ";; Environments" ouf)
    (write (environments-of module) :stream ouf)
    (terpri ouf)
    (write-line ";; Leases" ouf)
    (write (leases-of module) :stream ouf)
    (terpri ouf)
    (write-line ";; Statuses" ouf)
    (write (statuses-of module) :stream ouf)
    (terpri ouf)))

(defun load-env-data (module)
  (with-open-file (inf (orca-path "data/pss-envs.lisp") :direction :input)
    (setf (environments-of module) (read inf nil nil))
    (setf (leases-of module) (read inf nil nil))
    (setf (statuses-of module) (read inf nil nil))))

(defun expire-env-leases (module)
  (let ((now (get-universal-time)))
    (setf (leases-of module) (delete-if (lambda (lease)
                                    (<= (fourth lease) now))
                                  (leases-of module)))))

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

(defun query-leases (module env-name leases)
  (let ((matching (remove env-name leases :test-not #'string-equal :key #'cadr))
        (status (cdr (assoc env-name (statuses-of module) :test #'string-equal))))
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

(defun remove-all-env-leases (module env-name)
  (setf (leases-of module) (remove env-name (leases-of module) :test #'string-equal :key #'second)))

(defun remove-env-lease (module env-name nick)
  (setf (leases-of module) (remove-if (lambda (lease)
                                  (and (string-equal (second lease) env-name)
                                       (string-equal (first lease) nick)))
                                (leases-of module))))

(defun create-lease (module env-name nick time-str activity)
  (let ((expire-time (parse-expire-time time-str)))
    (cond
      ((not (member env-name (environments-of module) :test #'string-equal))
       (format nil "Can't find environment ~a" env-name))
      ((null expire-time)
       (format nil "Sorry, didn't understand ~a as an amount of time" time-str))
      (t
       (remove-env-lease module env-name nick)
       (push (list nick env-name activity expire-time) (leases-of module))
       (save-env-data module)
       (format nil "~a taken by ~a for ~a"
               env-name
               nick
               (describe-time-left expire-time))))))

(defun release-lease (module env-name nick)
  (cond
    ((not (member env-name (environments-of module) :test #'string-equal))
     (format nil "Can't find environment ~a" env-name))
    (t
     (remove-env-lease module env-name nick)
     (save-env-data module)
     (format nil "~a released by ~a" env-name nick))))

(defun set-env-status (module env-name status)
  (cond
    ((not (member env-name (environments-of module) :test #'string-equal))
     (format nil "Can't find environment ~a" env-name))
    ((zerop (length status))
     (setf (statuses-of module) (remove (assoc env-name (statuses-of module) :test #'string-equal)
                                      (statuses-of module)))
     (save-env-data module)
     (format nil "~a status removed" env-name))
    (t
     (let ((tuple (assoc env-name (statuses-of module) :test #'string-equal)))
       (if tuple
           (setf (cdr tuple) status)
           (setf (statuses-of module) (acons env-name status (statuses-of module)))))
     (save-env-data module)
     (format nil "~a status is now: ~a" env-name status))))

(defmethod handle-command ((module env-module) (cmd (eql 'env))
                           message args)
  (expire-env-leases module)
  (let ((env-name (first args))
        (subcmd (second args)))
    (cond
      ((null args)
       (reply-to message "~{~a~^, ~}" (environments-of module)))
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
       (if (member env-name (environments-of module) :test #'string-equal)
           (reply-to message (query-leases module (first args) (leases-of module)))
           (reply-to message "Can't find environment ~a" env-name)))
      ((string-equal subcmd "take")
       (reply-to message (create-lease module env-name (source message)
                                       (third args)
                                       (join-string #\space (cdddr args)))))
      ((string-equal subcmd "release")
       (reply-to message (release-lease module env-name (source message))))
      ((string-equal subcmd "status")
       (reply-to message (set-env-status module env-name (join-string #\space (cddr args)))))
      ((string-equal subcmd "add")
       (cond
         ((member env-name (environments-of module) :test #'string-equal)
          (reply-to message "That environment is already known."))
         (t
          (push env-name (environments-of module))
          (setf (environments-of module) (sort (environments-of module) #'string<))
          (save-env-data module)
          (reply-to message "Added ~a." env-name))))
      ((string-equal subcmd "remove")
       (cond
         ((not (member env-name (environments-of module) :test #'string-equal))
          (reply-to message "Can't find environment ~a" env-name))
         (t
          (setf (environments-of module) (remove env-name (environments-of module) :test #'string-equal))
          (save-env-data module)
          (reply-to message "Removed ~a from environments." env-name)))))))
