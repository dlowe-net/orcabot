(in-package #:orcabot)

(defclass lojban-word ()
  ((text :reader text-of :initarg :text)
   (category :reader category-of :initarg :category)
   (selmaho :reader selmaho-of :initarg :selmaho)
   (definition :reader definition-of :initarg :definition)
   (notes :reader notes-of :initarg :notes)
   (affixes :reader affixes-of :initarg :affixes)))

(defclass lojban-db ()
  ((valsi :reader valsi-of :initform (make-hash-table :test 'equal))
   (affixes :reader affixes-of :initform (make-hash-table :test 'equal))
   (glosses :reader glosses-of :initform (make-hash-table :test 'equal))))

(defclass jbovlaste-sax-handler (sax:abstract-handler)
  ((db :reader db-of :initarg :db)
   text-handler
   last-word last-type last-def last-notes last-selmaho last-affix
   affixes-list))

(defmethod sax:start-document ((handler jbovlaste-sax-handler))
  (with-slots (db text-handler last-word last-type last-def last-notes last-selmaho last-affix) handler
    (setf db (make-instance 'lojban-db)
          last-word nil
          last-type nil
          text-handler nil
          last-def nil
          last-notes nil
          last-selmaho nil
          last-affix nil)))

(defmethod sax:end-document ((handler jbovlaste-sax-handler))
  (slot-value handler 'db))

(defmethod sax:start-element ((handler jbovlaste-sax-handler) namespace-uri local-name qname attributes)
  (with-slots (db text-handler last-word last-type last-def last-notes last-selmaho last-affix affixes-list) handler
    (cond
      ((string= local-name "valsi")
       (setf last-word (sax:attribute-value (sax:find-attribute "word" attributes))
             last-type (sax:attribute-value (sax:find-attribute "type" attributes))
             text-handler nil
             last-def ""
             last-notes ""
             last-selmaho ""
             last-affix ""
             affixes-list nil))
      ((string= local-name "definition")
       (setf text-handler 'def))
      ((string= local-name "notes")
       (setf text-handler 'notes))
      ((string= local-name "selmaho")
       (setf text-handler 'selmaho))
      ((string= local-name "rafsi")
       (setf text-handler 'affixes))
      ((string= local-name "nlword")
       (setf (gethash (sax:attribute-value (sax:find-attribute "word" attributes)) (glosses-of db))
             (gethash (sax:attribute-value (sax:find-attribute "valsi" attributes)) (valsi-of db)))))))

(defmethod sax:characters ((handler jbovlaste-sax-handler) data)
  (flet ((append-and-format (str1 str2)
           (concatenate 'string
                        str1
                        (ppcre:regex-replace-all "\\$?([a-z]+)_\\{?(\\d+)\\}?\\$?"
                                                 str2
                                                 "\\1\\2"))))
    (with-slots (text-handler last-def last-notes last-selmaho last-affix)
        handler
      (case text-handler
        (def
         (setf last-def (append-and-format last-def data)))
        (notes
         (setf last-notes (append-and-format last-notes data)))
        (selmaho
         (setf last-selmaho (append-and-format last-selmaho data)))
        (affixes
         (setf last-affix
               (concatenate 'string last-affix data)))))))

(defmethod sax:end-element ((handler jbovlaste-sax-handler) namespace-uri local-name qname)
  (cond
    ((string= local-name "valsi")
     (with-slots (db last-word last-type last-def last-notes last-selmaho affixes-list) handler
       (let ((word (make-instance 'lojban-word
                                  :text last-word
                                  :category last-type
                                  :selmaho (when (string/= last-selmaho "") last-selmaho)
                                  :definition last-def
                                  :notes last-notes
                                  :affixes affixes-list)))
         (push word (gethash last-word (valsi-of db)))
         (dolist (affix affixes-list)
           (setf (gethash affix (affixes-of db)) word))
         (when (string= (category-of word) "gismu")
           (setf (gethash (text-of word) (affixes-of db)) word)
           (setf (gethash (subseq (text-of word) 0 4) (affixes-of db)) word)
           (setf (gethash (text-of word) (affixes-of db)) word)))))
    ((member local-name '("definition" "notes" "selmaho") :test #'string=)
     (with-slots (text-handler) handler
       (setf text-handler nil)))
    ((string= local-name "rafsi")
     (with-slots (last-affix affixes-list) handler
       (push last-affix affixes-list)
       (setf last-affix "")))))

(defun load-lojban-dictionary (path)
  (cxml:parse-file path (make-instance 'jbovlaste-sax-handler)))

(defun lojban-lookup (db raw-term)
  (let ((term (string-downcase raw-term)))
    (remove-duplicates
     (append
      (let ((result (gethash term (affixes-of db))))
        (when result (list result)))
      (gethash term (glosses-of db))
      (gethash term (valsi-of db))))))

(defmodule lojban lojban-module ("valsi" "gerna")
  (dict :accessor dict-of))

(defmethod initialize-module ((module lojban-module)
                              config)
  (declare (ignore config))
  (unless (probe-file (static-path "jbovlaste-en.xml"))
    (http-to-file "http://jbovlaste.lojban.org/export/xml-export.html?lang=en"
                  (static-path "jbovlaste-en.xml")))
  (setf (dict-of module) (load-lojban-dictionary (static-path "jbovlaste-en.xml"))))

(defun compound-to-affixes (term)
  (let* ((c "[bcdfgjklmnprstvxz]")
         (v "[aeiou]")
         (cc "(?:bl|br|
                cf|ck|cl|cm|cn|cp|cr|ct|
                dj|dr|dz|fl|fr|gl|gr|
                jb|jd|jg|jm|jv|kl|kr|
                ml|mr|pl|pr|
                sf|sk|sl|sm|sn|sp|sr|st|
                tc|tr|ts|vl|vr|xl|xr|
                zb|zd|zg|zm|zv)")
         (vv "(?:ai|ei|oi|au)")
         (affixes3v (concatenate 'string "(?:" cc v "|" c vv "|" c v "'" v ")"))
         (affixes3 (concatenate 'string "(?:" affixes3v "|" c v c ")"))
         (affixes4 (concatenate 'string "(?:" c v c c "|" cc  v  c ")"))
         (affixes5 (concatenate 'string affixes4 v)))

    (loop for i from 1 upto (1+ (floor (length term)
                                       3))
         as reg = (format nil "~v@{(?:(~a)([nry])?|(~a)(y))~1@*~}" i affixes3 affixes4)
         as reg2 = (concatenate 'string "^" reg "(" affixes3v "|" affixes5 ")$")
         as matches = (multiple-value-list
                       (re:scan-to-strings (re:create-scanner reg2 :extended-mode t)
                                           term
                                           :sharedp t))
         when (first matches)
         do (return-from compound-to-affixes
              (remove nil (second matches))))
    nil))

(defun find-lojban-components (dict term)
  (map 'list (lambda (aff)
               (when aff
                 (let ((word (gethash aff (affixes-of dict))))
                   (and word (text-of word)))))
       (compound-to-affixes term)))

(defun parse-valsi-args (raw-args)
  (let (opts args)
    (dolist (arg raw-args)
      (if (string= "-" arg :end2 1)
          (push arg opts)
          (push arg args)))
    (values (join-to-string " " args)
            (find "--affixes" opts :test #'string=)
            (find "--notes" opts :test #'string=)
            (find "--class" opts :test #'string=)
            (find "--type" opts :test #'string=)
            (find "--components" opts :test #'string=))))

(defmethod handle-command ((module lojban-module)
                           (cmd (eql 'valsi))
                           message raw-args)
  "valsi [--components|--affixes|--class|--type|--notes] - translate between lojban and english"
  (multiple-value-bind (term affixesp notesp selmahop typep componentsp)
      (parse-valsi-args raw-args)
    (if componentsp
        (let ((words (find-lojban-components (dict-of module) term)))
          (cond
            ((null words)
             (reply-to message "not found."))
            ((some #'null words)
             (reply-to message "not found."))
            (t
             (reply-to message "~a (components) = ~{~a~^ ~}" term words))))

        (let* ((results (lojban-lookup (dict-of module) term))
               (entry (first results)))
          (cond
            ((null results)
             (reply-to message "no results." term))
            ((cdr results)
             (reply-to message "~a result~:p for ~a: ~10{~a~^, ~}~a"
                       (length results)
                       term
                       (mapcar #'text-of results)
                       (if (> (length results) 10)
                           "..."
                           "")))
            (affixesp
             (let ((word entry))
               (reply-to message "~a (affixes) = ~{~a~^, ~}"
                         (text-of word)
                         (affixes-of word))))
            (selmahop
             (let ((word entry))
               (if (selmaho-of word)
                   (reply-to message "~a (class) = ~a"
                             (text-of word)
                             (selmaho-of word))
                   (reply-to message "none found."
                             (text-of word)
                             (selmaho-of word)))))
            (typep
             (let ((word entry))
               (reply-to message "~a (type) = ~a"
                         (text-of word)
                         (category-of word))))
            (notesp
             (let ((word entry))
               (reply-to message "~a (notes) = ~a"
                         (text-of word)
                         (notes-of word))))
            (t
             (let ((word entry))
               (reply-to message "~a = ~a"
                         (text-of word)
                         (definition-of word)))))))))

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