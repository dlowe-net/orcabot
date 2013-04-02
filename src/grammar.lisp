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

(defmodule grammar grammar-module ("manage" "insult" "solve" "plot" "food" "panic"))

(defun build-rule-expansions (body)
  "Returns a list of all the possible basic rules that result from the
expansion of the rule body."
  (let ((expansion (and body (build-rule-expansions (rest body)))))
    (cond
      ((null body)
       (list nil))
      ((not (consp (first body)))
       ;; basic clause - just append to all the possible lists
       (mapcar (lambda (expanded)
                 (cons (first body) expanded))
               expansion))
      ((eql (first (first body)) '?)
       ;; optional clause - expands into two bodies - one with, one without
       (mapcan (lambda (expanded)
                   (list*
                    expanded
                    (mapcar
                     (lambda (sub-expansion)
                        (append sub-expansion expanded))
                     (build-rule-expansions (reverse (rest (first body)))))))
               expansion))
      ((eql (first (first body)) 'or)
       ;; alternative clause - expands into n bodies, one of each
       (mapcan (lambda (expanded)
                 (mapcan
                  (lambda (alternative)
                    (if (consp alternative)
                        (mapcan (lambda (sub-expansion)
                                  (list
                                   (append sub-expansion expanded)))
                                (build-rule-expansions (reverse alternative)))
                        (list
                         (cons alternative expanded))))
                  (rest (first body))))
               expansion))
      ((eql (first (first body)) 'repeat)
       ;; repetition clause - expands into n identical bodies
       (mapcan (lambda (expanded)
                 (loop repeat (cadar body)
                      append (loop for sub-expansion
                           in (build-rule-expansions
                               (reverse (cddar body)))
                           collect (append sub-expansion expanded))))
               expansion))
      (t
       ;; generation-time clause
       (mapcan (lambda (expanded)
                 (loop for sub-expansion
                    in (build-rule-expansions
                        (cdar body))
                    collect (cons (cons (caar body)
                                          sub-expansion) expanded)))
               expansion)))))

(defun expand-grammar (grammar)
  (mapcan
   (lambda (rule)
     (let ((rule-name (first rule))
           (rule-body (cddr rule)))
       (mapcar
        (lambda (new-body)
          (list* rule-name '-> (reverse new-body)))
        (build-rule-expansions (reverse rule-body)))))
   grammar))

(defun hash-grammar (grammar)
  (let ((result (make-hash-table)))
    (dolist (rule (expand-grammar grammar))
      (push (cddr rule) (gethash (first rule) result)))
    result))

(defun load-grammar (path)
  (let ((*package* (find-package "ORCABOT")))
    (hash-grammar
     (with-open-file (inf path)
       (loop
          for rule = (read inf nil)
          while rule
          do
            (assert (symbolp (first rule)) nil "Invalid rule name ~s before ~d"
                    (first rule)
                    (file-position inf))
            (assert (eql (second rule) '->) nil "Invalid rule ~a before ~d"
                    (first rule)
                    (file-position inf))
            (assert (cddr rule) nil "Empty rule ~a before ~d"
                    (first rule)
                    (file-position inf))
          collect rule)))))

(defun separate-with-space (prev-el cur-el)
  (and prev-el
       (not (equal prev-el ""))
       (not (equal cur-el ""))
       (let ((prev-char (char prev-el (1- (length prev-el)))))
         (or (alphanumericp prev-char)
             (find prev-char ".?!,;")))
       (alphanumericp (char cur-el 0))))

(defun capitalize-el-p (prev-el expanded-el)
  (and prev-el
       (not (zerop (length prev-el)))
       (not (zerop (length expanded-el)))
       (let ((prev-char (or (find-if (lambda (c) (not (eql c #\Space))) prev-el :from-end t)
                            (char prev-el (1- (length prev-el))))))
         (find prev-char ".?!"))))

(defvar +plural-knowledge+
  '(
    ("ss$" . "sses")
    ("zz$" . "zzes") ;; Example: "buzzes".
    ("sh$" . "shes")
    ("tch$" . "tches")
    ("eaf$" . "eaves")
    ("ief$" . "ieves") ;; Example: "theives".
    ("roof$" . "roofs")
    ("oof$" . "ooves")
    ("ife$" . "ives")
    ("lf$" . "lves")
    ("[aeiou]y$" . "\\&s")
    ("ndum$" . "nda") ;; Example: "addendum".
    ("bum$" . "bums")
    ("um$" . "a") ;; Example: "media", "criteria", "symposia",
    ;; "crania", curriculum", "data".
    ("^die$" . "dice")
    ("dogma$" . "dogmas") ;; exception to -ma rule.
    ("lemma$" . "lemmas") ;; exception to -ma rule.
    ("schema$" . "schemas") ;; exception to -ma rule.
    ("ia$" . "ium") ;; Example: "bacteria".
    ("ma$" . "mata") ;; Example: "stigma".
    ("na$" . "nae") ;; Example: "antenna".
    ("ta$" . "tum") ;; Example: "strata".
    ("Atlas$" . "Atlantes") ;; Case-sensitive
    ("atlas$" . "atlases")
    ("Harry$" . "Harrys") ;; Case-sensitive
    ("aircraft$" . "aircraft")
    ("alga$" . "algae")
    ("alumna$" . "alumnae")
    ("alumnus$" . "alumni")
    ("ameoba$" . "ameobae")
    ("automaton$" . "automata")
    ("bacillus$" . "bacilli")
    ("banjo$" . "banjos")
    ("beau$" . "beaux")
    ("cactus$" . "cacti") ;; Or "cactuses".
    ("cannon$" . "cannon") ;; Or "cannons".
    ("canto$" . "cantos")
    ("cargo$" . "cargos")
    ("cattle$" . "cattle")
    ("child$" . "children")
    ("cod$" . "cod")
    ("corpus$" . "corpora")
    ("dwarf$" . "dwarves")
    ("cs$" . "csen") ;; Example: "emacsen".
    ("foot$" . "feet")
    ("formula$" . "formulae")
    ("graffito$" . "graffiti")
    ("rion$" . "ria") ;; Example: "criteria".
    ("deer$" . "deer")
    ("focus$" . "foci")
    ("genus$" . "genera")
    ("goose$" . "geese")
    ("hedron$" . "hedra") ;; Example: "polyhedron".
    ("hippopotamus$" . "hippopotami")
    ;;    ("index$" . "indices") ;; "indexes" is also acceptable.
    ("insigne$" . "insignia")
    ("life$" . "lives")
    ("louse$" . "lice")
    ("mackerel$" . "mackerel")
    ("man$" . "men")
    ("matrix$" . "matrices")
    ("moose$" . "moose")
    ("motto$" . "mottos")
    ("mouse$" . "mice")
    ("nucleus$" . "nuclei")
    ("octopus$" . "octopi") ;; Or "octopuses".
    ("offspring" . "offspring")
    ("opus$" . "opera")
    ("\\box$" . "oxen")
    ("panino$" . "panini")
    ("paparazzo$" . "paparazzi")
    ("phalanx$" . "phalanges")
    ("phenomenon$" . "phenomena")
    ("people$" . "people")
    ("perch$" . "perch") ;; Can certain uses of "perch" be plural?
    ("piano$" . "pianos")
    ("police$" . "police")
    ("portico$" . "porticos")
    ("quarto$" . "quartos")
    ("radius$" . "radii")
    ("rhinoceros$" . "rhinoceri") ;; Or "rhinoceroses".
    ;;    ("series$" . "series") ;; Already has an "s".
    ("sheep$" . "sheep")
    ;;    ("species$" . "species") ;; Already has an "s".
    ("solo$" . "solos")
    ("syllabus$" . "syllabi")
    ("terminus$" . "termini")
    ("ulus$" . "uli") ;; Example: "stimuli".
    ("trout$" . "trout")
    ("tooth$" . "teeth")
    ("uterus$" . "uteri") ;; Or "uteruses".
    ("virtuoso" . "virtuosi")
    ("viscus$" . "viscera")
    ;;    ("woman$" . "women") ;; See "man$".
    ;;    ("e$" . "es") ;; Fall-through to "[^s]$".
    ("is$" . "es") ;; Example: "axes", "crises", "testes".
    ("us$" . "uses") ;; Example: "campuses", "platypuses", "prospectuses".
    ("io$" . "ios")
    ("oo$" . "oos")
    ("o$" . "oes")
    ("y$" . "ies")
    ("[ei]x$" . "ices") ;; Example: "vertices".
    ("x$" . "xes")
    ("[^s]$" . "\\&s")) ;; Add an `s' if not an `s'.
  "Associative list with first element a regular expression
 for the suffix of nouns, and the second element is
 the replacement to make the word plural.

Matches are made in order of appearance.

Sorted by order of plural \"operation\", secondarily by case order,
then by alphabetical order.

Documentation on plural rules at:
 http://en.wikipedia.org/wiki/English_plural")

(defun pluralize (word)
  (dolist (tuple +plural-knowledge+)
    (multiple-value-bind (result matchp)
        (ppcre:regex-replace (first tuple) word (rest tuple))
      (when matchp
        (return-from pluralize result))))
  word)


(defun expand-grammar-element (grammar el)
  (cond
    ((stringp el)
     el)
    ((symbolp el)
     (text-from-grammar grammar el))
    ((and (consp el) (eql (first el) 'a))
     (let ((sub-expansion (reduce-from-grammar grammar (rest el))))
       (cond
         ((string= sub-expansion "")
          "")
         ((find (char sub-expansion 0) "aeiouAEIOU")
           (concatenate 'string "an " sub-expansion))
         (t
           (concatenate 'string "a " sub-expansion)))))
    ((and (consp el) (eql (first el) 'plural))
     (let* ((sub-expansion (reduce-from-grammar grammar (rest el))))
       (if (string= sub-expansion "")
           ""
           (pluralize sub-expansion))
       ))
    (t
     (write-to-string el))))

(defun reduce-from-grammar (grammar rule-body)
  (with-output-to-string (result)
    (loop
       for prev-el = nil then expanded-el
       for el on rule-body
       as expanded-el = (expand-grammar-element grammar (first el))
       do
         (when (separate-with-space prev-el expanded-el)
           (princ #\space result))
         (if (capitalize-el-p prev-el expanded-el)
             (princ (string-capitalize expanded-el :end 1) result)
             (princ expanded-el result)))))

(defun text-from-grammar (grammar rule-name)
  (let ((matching-rules (gethash rule-name grammar)))
    (if matching-rules
        (reduce-from-grammar grammar (random-elt matching-rules))
        ;; return just the symbol name if no definitions were found
        (symbol-name rule-name))))

(defun grammar-generate (grammar &optional (initial-term 'sentence))
  (let ((raw-result (text-from-grammar grammar initial-term)))
    (cl-ppcre:regex-replace-all "\\ba ([aeiouAEIOU])"
                                (string-capitalize raw-result :end 1)
                                "an \\1")))

(defun grammar-validate (grammar)
  (let ((reported (make-hash-table)))
    (maphash (lambda (key expansions)
               (declare (ignore key))
               (dolist (expansion expansions)
                 (dolist (term expansion)
                   (when (and (symbolp term)
                              (null (gethash term grammar))
                              (null (gethash term reported)))
                     (setf (gethash term reported) t)
                     (format t "~a has no expansion~%" term)))))
           grammar)))

(defmethod handle-command ((module grammar-module) (cmd (eql 'manage))
                           message args)
  "manage [<person>] - give some sage corporate advice"
  (let ((grammar (load-grammar (orcabot-path "data/manage-grammar.lisp"))))
    (setf (gethash 'person grammar)
          (list (list
           (cond
             (args
               (format nil "~{~a~^ ~}" args))
             ((char= #\# (char (first (arguments message)) 0))
              (random-elt (hash-table-keys (users (find-channel (connection message)
                                               (first (arguments message)))))))
             (t
              "someone else")))))
    (reply-to message (grammar-generate grammar))))

(defmethod handle-command ((module grammar-module) (cmd (eql 'insult))
                           message args)
  "insult [<person>] - let people know what you think, Elizabethian style"
  (let ((insult (grammar-generate (load-grammar (orcabot-path "data/insult-grammar.lisp")))))
    (reply-to message
              (if args
                  (format nil "~{~a~^ ~}: ~a" args insult)
                  insult))))

(defmethod handle-command ((module grammar-module) (cmd (eql 'solve))
                           message args)
  "solve [<problem>] - diagnose and solve any problem"
  (let ((grammar (load-grammar (orcabot-path "data/solve-grammar.lisp"))))
    (when args
      (setf (gethash 'problem grammar)
            (list (list (switch-person (format nil "~{~a~^ ~}" args))))))
    (reply-to message (grammar-generate grammar))))

(defmethod handle-command ((module grammar-module) (cmd (eql 'plot))
                           message args)
  "plot [<character>] - generate a story"
  (let ((grammar (load-grammar (orcabot-path "data/plots-grammar.lisp"))))
    (when args
      (setf (gethash 'the-main-character grammar)
            (list (list (switch-person (format nil "~{~a~^ ~}" args)))))
      (setf (gethash 'the-main-characters grammar)
            '((the-main-character "and" those-people))))
    (reply-to message (grammar-generate grammar))))

(defmethod handle-command ((module grammar-module) (cmd (eql 'panic))
                           message args)
  "panic - hit the panic button!"
  (let ((grammar (load-grammar (orcabot-path "data/panic-grammar.lisp"))))
    (when args
      (setf (gethash 'problem grammar)
            (list (list (switch-person (format nil "~{~a~^ ~}" args)))))
      (setf (gethash 'panic grammar)
            (append (gethash 'panic grammar)
                    (gethash 'panic-arg grammar))))
    (reply-to message (grammar-generate grammar))))

(defmethod handle-command ((module grammar-module) (cmd (eql 'food))
                           message args)
  "food [<character>] - throw food at an unsuspecting target"
  (let* ((grammar (load-grammar (orcabot-path "data/food-grammar.lisp")))
         (initial-term (if (equal (first args) (source message)) 'sentence-self 'sentence))
         (channel-users (and (message-target-is-channel-p message)
                             (hash-table-keys (users (find-channel (connection message)
                                                             (first (arguments message)))))))
         (target (cond
                   (args
                    (format nil "~{~a~^ ~}" args))
                   ((char= #\# (char (first (arguments message)) 0))
                    (random-elt channel-users))
                   (t
                    "someone"))))
    (setf (gethash 'nick grammar)
          (list (list (source message))))
    (setf (gethash 'target grammar)
          (list (list target)))
    (let* ((valid-bystanders (remove-if (lambda (user-nick)
                                          (or (equal user-nick (source message))
                                              (equal user-nick target)))
                                        channel-users)))
      (setf (gethash 'bystander grammar)
            (list (list
                   (cond
                     ((char= #\# (char (first (arguments message)) 0))
                      (random-elt valid-bystanders))
                     (t
                      "someone else"))))))
    (reply-to message "~a ~a" (source message) (string-downcase (grammar-generate grammar initial-term) :end 1))))
