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

(in-package #:orcabot)


(defparameter *enscript-languages* '("bash" "cpp" "c" "diff"
                                     "elisp" "html" "javascript" "java"
                                     "mail" "makefile" "perl"
                                     "python" "rfc" "ruby" "scheme" "sh"
                                     "sql" "tcl" "zsh"))
(defparameter *pastes* (make-array '(0) :adjustable t :fill-pointer t))
(defvar *paste-mutex* (sb-thread:make-mutex :name "paste lock"))

;; GET /paste - home page
;; POST /paste - add paste and redirect to new paste
;; GET /paste/# - display paste
(defun make-new-paste (nick channel lang summary text)
  (sb-thread:with-mutex (*paste-mutex* :wait-p t)
    (vector-push-extend (list (local-time:now) nick channel lang summary text)
                        *pastes*)))

(defun colorize (text lang)
  (let ((str
         (with-input-from-string (str text)
           (with-output-to-string (result)
             (sb-ext:run-program "/usr/bin/enscript"
                          (list
                           (format nil "--highlight=~a" lang)
                           "--language=html"
                           "--output=-"
                           "--no-header"
                           "--silent"
                           "--color")
                          :input str
                          :output result
                          :wait t)))))
    (subseq str
            (+ (search "<PRE>" str) 5)
            (search "</PRE>" str :from-end t))))

(defun display-paste (paste-id)
  (cond
    ((not (< -1 paste-id (length *pastes*)))
     "Invalid paste id")
    (t
     (destructuring-bind (time nick channel lang summary text)
         (aref *pastes* paste-id)
       (format nil "<div class=\"pastebox\"><h2>~a</h2><h3>Posted by ~a~:[ to ~a~;~*~]<br>~a</h3><div class=\"pastebody\"><pre>~a</pre></div></div>"
               (html-entities:encode-entities summary)
               (html-entities:encode-entities nick)
               (string= "None" channel)
               (html-entities:encode-entities channel)
               (local-time:format-timestring nil time
                     :format local-time:+rfc-1123-format+)
               (if (string= lang "Plain")
                   (html-entities:encode-entities text)
                   (colorize text lang)))))))

(defun display-main-page (nick channel lang summary text message)
  (with-output-to-string (output)
    (write-string "<div class=\"interface\">
<form method=\"POST\" action=\"/paste\">" output)
    (format output "<label for=\"nick\">IRC Nick</label><input type=\"text\" name=\"nick\" value=\"~a\"><br>~%" (or nick ""))
    (format output "<label for=\"channel\">IRC Channel</label><select name=\"channel\">~%~:{<option~[~; selected=\"selected\"~]>~a</option>~%~}</select><br>~%"
            (mapcar (lambda (chan)
                      (list (string= channel chan) chan))
                    '("None" "#ars" "#deploys" "#pounder" "#restools")))
    (format output "<label for=\"lang\">Display as </label><select name=\"lang\">~%~:{<option~[~; selected=\"selected\"~]>~a</option>~%~}</select><br>~%"
            (mapcar (lambda (l)
                      (list (string= lang l) l))
                    (cons "Plain" *enscript-languages*)))
    (format output "<label for=\"summary\">Paste Summary</label>
<input style=\"width: 50%\" type=\"text\" name=\"summary\" value=\"~a\"><br>
<label for=\"text\">Paste Text</label><br>
<textarea name=\"text\" rows=\"24\" cols=\"80\">~a</textarea><br>
<input style=\"margin-left: auto\" type=\"submit\" name=\"submit\" value=\"Submit Paste\">&nbsp;&nbsp;~@[<span class=\"alert\">~a~]
</div>"
            (or summary "") (or text "") message)
    (when (plusp (length *pastes*))
      (let ((now (local-time:now)))
        (format output "<div class=\"interface\"><h2>Recent Pastes</h2><ul>~%~:{<li>~a - <a href=\"http://deng-dlowe:8080/paste/~a\">~a</a><br>~a</li>~%~}</ul></div>~%"
                (loop
                   for idx from (max 0 (- (length *pastes*) 10)) upto (1- (length *pastes*))
                   as entry = (aref *pastes* idx)
                   collect (list
                            (html-entities:encode-entities (second entry))
                            idx
                            (html-entities:encode-entities (fifth entry))
                            (describe-time-difference now (first entry)))))))))

(defun describe-time-difference (now time)
  (let ((diff (timestamp-difference now time)))
    (cond
      ((> diff (* 60 60 24 365))
       (format nil "Over ~d year~:p ago" (floor diff (* 60 60 24 365))))
      ((> diff (* 60 60 24 14))
       (format nil "About ~d week~:p ago" (floor diff (* 60 60 24 7))))
      ((> diff (* 60 60 24))
       (format nil "About ~d day~:p ago" (floor diff (* 60 60 24))))
      ((> diff (* 60 60))
       (format nil "About ~d hour~:p ago" (floor diff (* 60 60))))
      ((> diff 60)
       (format nil "About ~d minute~:p ago" (floor diff 60)))
      (t
       (format nil "Less than a minute ago")))))

(defun wrap-output (output)
  (when (stringp output)
    (with-output-to-string (result)
      (write-string "<html>
<head><title>orcabot paste</title>
<meta name=\"ROBOTS\" content=\"NOARCHIVE\">
<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\" href=\"/paste.css\">
</head><body><div id=\"header\">Orcabot Pastebin</div>
<div id=\"content\">" result)
      (write-string output result)
      (write-string "</div><div id=\"footer\"><a href=\"/paste\">New Paste</a></div></body></html>" result))))

(defun handle-paste-submission (nick channel lang summary text)
  (cond
    ((or (null nick)
         (string= nick "")
         (notany #'alphanumericp nick))
     (display-main-page nick channel lang summary text
                        "You cannot post without an IRC nick"))
    ((or (null channel)
         (not (or (string= "None" channel)
                  (member channel *autojoin-channels* :test #'string=))))
     (display-main-page nick channel lang summary text
                        "You cannot post without an IRC channel"))
    ((or (null lang)
         (not (or (string= "Plain" lang)
                  (member lang *enscript-languages* :test #'string=))))
     (display-main-page nick channel lang summary text
                        "You cannot post without a language option"))
    ((or (null summary)
         (string= summary "")
         (notany #'alphanumericp summary))
     (display-main-page nick channel lang summary text
                        "You cannot post without a paste summary"))
    ((or (null text)
         (string= text "")
         (notany #'alphanumericp text))
     (display-main-page nick channel lang summary text
                        "You cannot post an empty paste"))
    (t
     (let ((paste-id (make-new-paste nick channel lang summary text)))
       (when (string= "#" channel :end2 1)
         (irc:privmsg *connection* channel (format nil "~a pasted \"~a\" [http://deng-dlowe:8080/paste/~a]" nick summary paste-id)))
       (hunchentoot:set-cookie "pastebin-nick" :value nick)
       (hunchentoot:set-cookie "pastebin-channel" :value channel)
       (hunchentoot:redirect (format nil "/paste/~a" paste-id))))))

(defun snarf-file (path)
  "Returns a string with the contents of the file at PATH."
  (with-open-file (inf path :direction :input)
    (let ((result (make-string (file-length inf))))
      (read-sequence result inf :end (file-length inf))
      result)))

(hunchentoot:define-easy-handler (paste-css :uri "/paste.css") ()
  (snarf-file (static-path "paste.css")))

(defun pasteview-uri-p (request)
  (cl-ppcre:scan "/paste/\\d+" (hunchentoot:request-uri request)))

(hunchentoot:define-easy-handler (paste-view :uri 'pasteview-uri-p) ()
  (setf (hunchentoot:content-type*) "text/html")
  (wrap-output
   (display-paste (parse-integer (hunchentoot:script-name*)
                                 :junk-allowed t :start 7))))

(hunchentoot:define-easy-handler (paste-home :uri "/paste")
    (nick channel lang summary text submit)
  (setf (hunchentoot:content-type*) "text/html")
  (wrap-output
   (cond
     (submit
      (handle-paste-submission nick channel lang summary text))
     (t
      (display-main-page (or nick (hunchentoot:cookie-in "pastebin-nick") "")
                         (or channel
                             (hunchentoot:cookie-in "pastebin-channel")
                             "")
                         lang summary text nil)))))