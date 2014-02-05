(in-package #:orcabot)

(defclass lojban-word ()
  ((text :reader text-of :initarg :text)
   (category :reader category-of :initarg :category)
   (definition :reader definition-of :initarg :definition)
   (notes :reader notes-of :initarg :notes)
   (rafsi :reader rafsi-of :initarg :rafsi)))

(defclass jbovlaste-sax-handler (sax:abstract-handler)
  ((table :reader table-of :initarg :table)
   text-handler
   last-word last-type last-def last-notes last-rafsi))

(defmethod sax:start-document ((handler jbovlaste-sax-handler))
  (with-slots (text-handler last-word last-type last-def last-notes last-rafsi) handler
    (setf last-word nil
          last-type nil
          text-handler nil
          last-def nil
          last-notes nil
          last-rafsi nil)))

(defmethod sax:end-document ((handler jbovlaste-sax-handler))
  nil)

(defmethod sax:start-element ((handler jbovlaste-sax-handler) namespace-uri local-name qname attributes)
  (with-slots (table text-handler last-word last-type last-def last-notes last-rafsi) handler
    (cond
      ((string= local-name "valsi")
       (setf last-word (sax:attribute-value (sax:find-attribute "word" attributes))
             last-type (sax:attribute-value (sax:find-attribute "type" attributes))
             text-handler nil
             last-def nil
             last-notes nil
             last-rafsi nil))
      ((string= local-name "definition")
       (setf text-handler 'def))
      ((string= local-name "notes")
       (setf text-handler 'notes))
      ((string= local-name "rafsi")
       (setf text-handler 'rafsi))
      ((string= local-name "nlword")
       (setf (gethash (sax:attribute-value (sax:find-attribute "word" attributes)) table)
             (gethash (sax:attribute-value (sax:find-attribute "valsi" attributes)) table))))))

(defmethod sax:characters ((handler jbovlaste-sax-handler) data)
  (with-slots (text-handler last-def last-notes last-rafsi)
      handler
    (case text-handler
      (def
       (setf last-def (ppcre:regex-replace-all "\\$?([a-z])_\\{?(\\d+)\\}?\\$?" data "\\1\\2")
             text-handler nil))
      (notes
       (setf last-notes data
             text-handler nil))
      (rafsi
       (push data last-rafsi)
       (setf text-handler nil)))))

(defmethod sax:end-element ((handler jbovlaste-sax-handler) namespace-uri local-name qname)
  (cond
    ((string= local-name "valsi")
     (with-slots (table last-word last-type last-def last-notes last-rafsi) handler
       (push (make-instance 'lojban-word
                            :text last-word
                            :category last-type
                            :definition last-def
                            :notes last-notes
                            :rafsi last-rafsi)
             (gethash last-word table))))))

(defun load-lojban-dictionary (path)
  (let ((table (make-hash-table :test 'equalp)))
    (with-open-file (inf path :element-type '(unsigned-byte 8))
      (cxml:parse inf (make-instance 'jbovlaste-sax-handler :table table)))
    (maphash (lambda (k v)
               (declare (ignore k))
               (dolist (word v)
                 (dolist (one-rafsi (rafsi-of word))
                   (unless (gethash one-rafsi table)
                     (pushnew word (gethash one-rafsi table))))))
             table)
    table))

(defun lojban-lookup (table term)
  (gethash term table))

(defmodule lojban lojban-module ("valsi" "gerna")
  (dict :accessor dict-of))

(defmethod initialize-module ((module lojban-module)
                              config)
  (declare (ignore config))
  (unless (probe-file (static-path "jbovlaste-en.xml"))
    (http-to-file "http://jbovlaste.lojban.org/export/xml-export.html?lang=en"
                  (static-path "jbovlaste-en.xml")))
  (setf (dict-of module) (load-lojban-dictionary (static-path "jbovlaste-en.xml"))))

(defmethod handle-command ((module lojban-module)
                           (cmd (eql 'valsi))
                           message args)
  "valsi - translate between lojban and english"
  (let ((results (lojban-lookup (dict-of module) (first args))))
    (cond
      ((null results)
       (reply-to message "~a not found." (first args)))
      ((endp (cdr results))
       (reply-to message "~a = ~a" (text-of (first results))
                 (definition-of (first results))))
      (t
       (reply-to message "Results for ~a: ~{~a~^, ~}" (first args)
                 (mapcar #'text-of results))))))

(defun jbofihe (input)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (with-input-from-string (in input)
      (sb-ext:run-program "/usr/bin/jbofihe" '("-ie" "-cr")
                          :input in :output out :error err :wait t))
    (values (substitute #\space #\newline (get-output-stream-string out))
            (get-output-stream-string err))))

(defmethod handle-command ((module lojban-module)
                           (cmd (eql 'gerna))
                           message args)
  "gerna - parse lojban jufra"
  (multiple-value-bind (result err)
      (jbofihe (join-to-string " " args))
    (cond 
      ((ppcre:scan "^Unrecognizable word" err)
       (reply-to message "Not grammatical: ~a" (join-to-string " " args)))
      ((ppcre:scan "^Misparsed token" err)
       (reply-to message "Not grammatical: ~a" (join-to-string " " args)))
      ((string/= err "")
       (reply-to message "Not grammatical: ~a" (join-to-string " " args)))
      (t
       (reply-to message "~a" result)))))