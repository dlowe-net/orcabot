(in-package #:orca)

;;; Package for managing Magic: the Gathering tournaments
;;
;; Commands:
;;   mreset - resets the tournament to a new one
;;   madd   - adds a new member to the tournament
;;   mdrop  - drops a member from the tournament
;;   mmatch - add a match to the record, sets the title
;;   mscore - gives all scores
;;   mleft  - gives list of undone matches

(defvar *magic-players* nil)
(defvar *magic-matches* nil)
(defvar *magic-locked* nil)

(defun magic-wins-of (nick)
  (count-if (lambda (match)
              (string-equal nick (car match)))
            *magic-matches*))

(defun magic-losses-of (nick)
  (count-if (lambda (match)
              (string-equal nick (cdr match)))
            *magic-matches*))

(defun find-match (winner loser)
  (find-if (lambda (match)
             (and (string-equal winner (car match))
                  (string-equal loser (cdr match))))
           *magic-matches*))

(defun make-score-card ()
  (flet ((compare-players (a b)
           (cond
             ((/= (second a) (second b))
              (> (second a) (second b)))
             ((/= (third a) (third b))
              (< (third a) (third b)))
             (t
              (string-lessp (first a) (first b))))))
    (let ((players (mapcar (lambda (nick)
                             (list nick (magic-wins-of nick) (magic-losses-of nick)))
                           *magic-players*)))
      (format nil "~:{~a ~d-~d~:^, ~}"
              (sort players #'compare-players)))))

(defun unplayed-magic-matches (player-a players)
  (loop for player-b in players
     unless (or (string= player-a player-b)
                (find-match player-a player-b)
                (find-match player-b player-a))
     collect (list player-a player-b)))

(defun all-unplayed-magic-matches ()
  (loop
     for players on *magic-players*
     as player-a = (first players)
     append (unplayed-magic-matches player-a (rest players))))

(defun format-unplayed (unplayed-matches)
  (format nil "~:{~a-~a~:^, ~}" unplayed-matches))

(defun add-magic-players (nicks)
  (setf *magic-players* (sort (union *magic-players* nicks :test 'string-equal)
                              'string-lessp))
  (save-tournament))

(defun drop-magic-players (nicks)
  (setf *magic-players*
        (set-difference *magic-players* nicks :test 'string-equal))
  (save-tournament))

(defun reset-tournament ()
  (setf *magic-players* nil)
  (setf *magic-matches* nil)
  (setf *magic-locked* nil))

(defun load-tournament ()
  (reset-tournament)
  (when (probe-file (orca-path "data/mtg.lisp"))
    (with-open-file (inf (orca-path "data/mtg.lisp")
                         :direction :input)
      (setf *magic-players* (read inf nil))
      (setf *magic-matches* (read inf nil))
      (setf *magic-locked* (read inf nil)))))

(defun save-tournament ()
  (with-open-file (ouf (orca-path "data/mtg.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (print *magic-players* ouf)
    (print *magic-matches* ouf)
    (print *magic-locked* ouf)))

(defparameter *magic-cards* (make-hash-table :test 'equalp))
(defparameter *magic-card-lookup* nil)

(defun get-hash-with-lookup (key table lookup-table)
  (let ((result (remove-if-not (lambda (x)
                                 (search key x :test #'string-equal))
                               lookup-table)))
    (cond
      ((null result)
       (list (format nil "~a not found" key)))
      ((null (cdr result))
       ;; only one
       (gethash (car result) table))
      (t
       (list (format nil "Found ~10{~a~^; ~}~:[.~;and ~r others.~]"
                     result
                     (> (length result) 10)
                     (- (length result) 10)))))))

(defun load-oracle-db ()
  (let ((name nil)
        (desc nil))
    (clrhash *magic-cards*)
    (setf *magic-card-lookup* nil)
    (with-open-file (inf (orca-path "data/oracle.dat"))
      (loop
         for line = (read-line inf nil)
         while line do
           (cond
             ((string= "" line)
              nil)
             ((char= #\@ (char line 0))
              (setf name (subseq line 1)))
             ((char= #\% (char line 0))
              (setf (gethash name *magic-cards*) (reverse desc))
              (push name *magic-card-lookup*)
              (setf desc nil)
              (setf name nil))
             (t
              (push line desc)))))))

(define-fun-command mcard (message directp &rest card)
  (let* ((query (format nil "~{~a~^ ~}" card))
         (result (get-hash-with-lookup query *magic-cards* *magic-card-lookup*)))
    (if result
        (loop for line in result
           as indent = "" then "    " do
             (reply-to message "~a~a" indent line)))))

(define-fun-command mreset (message directp)
  (reset-tournament))

(define-fun-command madd (message directp &rest nicks)
  (let ((old-player-len (length *magic-players*)))
    (add-magic-players nicks)
    (reply-to message "added ~d player~:p" (- (length *magic-players*)
                                            old-player-len))))

(define-fun-command mdrop (message directp &rest nicks)
  (let ((old-player-len (length *magic-players*)))
    (drop-magic-players nicks)
    (reply-to message "dropped ~d player~:p" (- old-player-len
                                              (length *magic-players*)))))

(define-fun-command munlock (message directp)
  (setf *magic-locked* nil)
  (reply-to message "Tournament membership unlocked."))

(define-fun-command mlock (message directp)
  (setf *magic-locked* t)
  (reply-to message "Tournament membership locked."))

(define-fun-command mjoin (message directp)
  (let ((player-name (shorten-nick (source message))))
    (cond
      ((find player-name *magic-players* :test 'string-equal)
       (reply-to message "You've already joined the tournament."))
      (*magic-locked*
       (reply-to message "Sorry, the tournament membership is currently locked."))
      (t
       (add-magic-players (list player-name))
       (reply-to message "Okay, you're now a part of the tournament.")))))

(define-fun-command mleave (message directp)
  (let ((player-name (shorten-nick (source message))))
    (cond
      ((null (find player-name *magic-players* :test 'string-equal))
       (reply-to message "You're not even in the tournament."))
      (t
       (drop-magic-players (list player-name))
       (reply-to message "Okay, you've left the tournament.")))))

(define-fun-command mmatch (message directp winner loser)
  (cond
    ((null (find winner *magic-players* :test 'string-equal))
     (reply-to message "~a isn't playing" winner))
    ((null (find loser *magic-players* :test 'string-equal))
     (reply-to message "~a isn't playing" loser))
    ((string-equal winner loser)
     (reply-to message "Hah, hah.  Funny boy."))
    ((find-match winner loser)
     (reply-to message "result of match already entered."))
    ((find-match loser winner)
     (let ((match (find-match loser winner)))
       (setf (car match) winner)
       (setf (cdr match) loser)
       (save-tournament)
       (reply-to message "match altered.  now ~a is ~d-~d, ~a is ~d-~d"
                 winner
                 (magic-wins-of winner) (magic-losses-of winner)
                 loser
                 (magic-wins-of loser) (magic-losses-of loser))))
    (t
     (push (cons winner loser) *magic-matches*)
     (save-tournament)
     (reply-to message "match entered.  ~a is ~d-~d, ~a is ~d-~d"
               winner
               (magic-wins-of winner) (magic-losses-of winner)
               loser
               (magic-wins-of loser) (magic-losses-of loser)))))

(define-fun-command mscore (message directp &rest nicks)
  (if nicks
      (reply-to message "Wins: ~:[None~;~:*~{~a~^, ~}~]  Losses: ~:[None~;~:*~{~a~^, ~}~]"
                (mapcar 'cdr (remove (first nicks) *magic-matches* :test-not 'string-equal :key 'car))
                (mapcar 'car (remove (first nicks) *magic-matches* :test-not 'string-equal :key 'cdr)))
      (reply-to message "~a" (make-score-card))))

(define-fun-command mleft (message directp &rest nicks)
  (if nicks
      (reply-to message "~:[You don't have anyone left!~;~:*~{~a~^, ~}~]"
                (mapcar 'second (unplayed-magic-matches (first nicks) *magic-players*)))
      (reply-to message "~:[This tournament is complete!~;~:*~:{~a-~a~:^, ~}~]"
                (all-unplayed-magic-matches))))

(define-fun-command mhelp (message directp)
  (dolist (text '("mhelp                    - display this notice"
                  "mjoin                    - join the current tournament"
                  "mleave                   - leave the current tournament"
                  "mmatch <winner> <loser>  - add a match to the record, sets the title"
                  "mscore [<nick>]          - gives all scores"
                  "mleft [<nick>]           - gives list of undone matches"))
    (irc:notice (connection message) (source message) text)))

(defun uri-escape (str)
  (cl-ppcre:regex-replace-all "[^a-zA-Z0-9]" str
                              (lambda (target start end match-start match-end reg-starts reg-ends)
                                (declare (ignorable start end match-end reg-starts reg-ends))
                                (format nil "%~16r" (char-code (char target match-start))))))

(defun retrieve-magic-prices (name set)
  (let* ((uri (format nil "http://store.tcgplayer.com/Products.aspx?GameName=Magic&name=~a~@[&setName=~a~]"
                      (uri-escape name)
                      (when set (uri-escape set))))
         (page (drakma:http-request uri))
         (boundaries (ppcre:all-matches (ppcre:create-scanner "<div class=\"product_list_(?:alternate_)?row\">(.*?)view ALL Prices and Conditions for this card" :single-line-mode t :multi-line-mode t) page)))
    (loop
       for (start end) on boundaries by #'cddr
       as name = (aref (nth-value 1 (cl-ppcre:scan-to-strings "Title=\"Click to View More Info about ([^\"]+)\">"
                                                        page
                                                        :start start
                                                        :end end)) 0)
       as price = (aref (nth-value 1 (cl-ppcre:scan-to-strings "<td>\\$([0-9]+\\.[0-9]+)</td>"
                                                         page
                                                         :start start
                                                         :end end)) 0)
       as set = (aref (nth-value 1 (cl-ppcre:scan-to-strings "Set: <a[^>]+>([^<]+)"
                                                         page
                                                         :start start
                                                         :end end)) 0)
       as grade = (aref (nth-value 1 (cl-ppcre:scan-to-strings "Rarity: (.)"
                                                               page
                                                               :start start
                                                               :end end)) 0)
       as rarity = (or (second (assoc (char grade 0)
                                  '((#\M "Mythic Rare")
                                    (#\R "Rare")
                                    (#\U "Uncommon")
                                    (#\C "Common"))))
                       grade)
       collect (list name price set rarity))))

(define-fun-command mprice (message directp &rest card)
  (if card
      (let* ((query (format nil "~{~a~^ ~}" card))
             (match (ppcre:split "/" query))
             (name (first match))
             (set (second match))
             (results (retrieve-magic-prices name set))
             (exact-match (find name results :key 'first :test 'string-equal)))
        (cond
          ((and (cdr results) (not exact-match))
           (reply-to message "Found ~{~a~^, ~}" (mapcar 'first results)))
          ((null results)
           (reply-to message "couldn't find '~a'~@[ in set '~a'~]" name set))
          (t
           (destructuring-bind (name price set rarity)
               (or exact-match (first results))
             (reply-to message "'~a' is selling for $~a ~
                                [~a ~a] ~
                               (http://magic.tcgplayer.com/db/~
                                 magic_single_card.asp?cn=~a&sn=~a)"
                       name price set rarity
                       (uri-escape name)
                       (uri-escape set))))))
      (reply-to message "Usage: ~mprice <card>[/<set>]")))
