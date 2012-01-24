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

(defvar *last-fifa-id* 0)

(defun retrieve-fifa-updates (match &optional last-update)
  (cxml:parse
   (drakma:http-request
    (format nil "http://www.fifa.com/live/Competitions/worldcup/matchcentrelight/MatchDay=13/Day=1/Match=~a/matchIndex.xml~@[?ign=~a~]" match last-update))
   (cxml-xmls:make-xmls-builder)))

(defun node-attr (node attr)
  (second (assoc attr (cxml-xmls:node-attrs node)
                 :test #'string=)))

(defun node-named-child (node child-name)
  (assoc child-name
         (remove-if-not #'listp (cxml-xmls:node-children node))
         :test #'string=))

(defun event-id (e)
  (parse-integer (second (assoc "id" (cxml-xmls:node-attrs e)
                                   :test #'string=))))
(defun new-event-p (e)
  (> (event-id e) *last-fifa-id*))

(defun new-fifa-events (xml)
  (let* ((allevents (remove-if-not #'listp
                                   (cxml-xmls:node-children
                                    (assoc "allevents"
                                           (remove-if-not #'listp (cddr xml))
                                           :test #'string=))))
         (latest-events (remove-if-not 'new-event-p allevents)))
    #+nil (setf *last-fifa-id* (apply #'max *last-fifa-id*
                                (mapcar 'event-id latest-events)))
    latest-events))

(defun retrieve-team-info (xml team)
  (dolist (node (cxml-xmls:node-children
                 (node-named-child
                  (node-named-child xml "matchinfo")
                  "match")))
    (when (and (listp node) (string= (node-attr node "team")
                                     team))
      (return-from retrieve-team-info node)))
  nil)

(defun event-desc (home-team-code home-team-name away-team-name e)
  (case (parse-integer (or (node-attr e "code") "0") :junk-allowed t)
    (0 nil)                      ; No code
    (1 "Yellow carded")                      ; yellow carded
    (2 "Red carded")
    (4 "Substitution")                      ; substitution
    (6 "Blocked")                      ; blocked
    (7 "Shot Wide")                      ; shot wide
    (8 "Denied Goal")                      ; denied goal
    (9 "Offsides")                      ; offsides (1)
    (10 "Corner Kick")                     ; corner kick
    (11 "Foul")                     ; foul (1)
    (21 "Commentary")                     ; commentary
    (32 "Save")                     ; save
    (55 "No Idea")                     ; no idea
    (100 nil)                    ; promotion
    (3
     (format nil "~a has scored a goal at ~a - Score is ~a ~a- ~a ~a"
             (if (string= (node-attr e "t_id") home-team-code)
                 home-team-name
                 away-team-name)
             (node-attr e "timedisplay")
             home-team-name
             (node-attr e "hs")
             away-team-name
             (node-attr e "as")))
    (13
     "Start of play")
    (14
     "End of play")
    (t
     (format nil "Unknown event: ~a" e))))

(defun new-fifa-event-descs ()
  (let* ((xml (retrieve-fifa-updates "300061461"))
         (home-team-info (retrieve-team-info xml "home"))
         (home-team-code (node-attr home-team-info "code"))
         (home-team-name (node-attr home-team-info "name"))
         (away-team-name (node-attr (retrieve-team-info xml "away") "name"))
         (new-events (new-fifa-events xml)))
    (loop
       for e in new-events
       as desc = (event-desc home-team-code
                             home-team-name
                             away-team-name
                             e)
       when desc
       collect desc)))