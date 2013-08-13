(in-package #:orcabot)

(define-condition weather-error ()
  ((message :accessor message-of :initarg :message)))

(defmethod print-object ((object weather-error) stream)
  (print-unreadable-object (object stream)
    (format stream "~a" (message-of object))))

(defvar *weather-throttles* (make-hash-table :test 'equal)
  "Table of throttling records, indexed by API key.  Used to ensure
  compliance with wunderground rate limits.")

(defclass weather-throttle ()
  ((minute-queries :accessor minute-queries-of :initarg :minute-queries :initform 0)
   (day-queries :accessor day-queries-of :initarg :day-queries :initform 0)
   (last-minute :accessor last-minute-of :initarg :last-minute)
   (last-day :accessor last-day-of :initarg :last-day)
   (max-per-minute :accessor max-per-minute-of :initarg :max-per-minute)
   (max-per-day :accessor max-per-day-of :initarg :max-per-day)))

(defun throttle-time (current-time)
  "Returns a value unique to the minute and day of the given current
time.  Used for throttling connections to wunderground."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time current-time 0)
    (declare (ignore sec))
    (values
     (encode-universal-time 0 min hour day month year 0)
     (encode-universal-time 0 0 0 day month year 0))))

(defun register-weather-key (key current-time max-per-day max-per-minute)
  "Registers a wunderground API key and sets the maximum per-day and
per-minute request limits.  This must be used before any queries are
made with the key."
  (multiple-value-bind (minute day)
      (throttle-time current-time)
    (setf (gethash key *weather-throttles*)
          (make-instance 'weather-throttle
                         :last-minute minute
                         :last-day day
                         :max-per-day max-per-day
                         :max-per-minute max-per-minute))))

(defun save-weather-config (path)
  "Saves throttling information."
  (with-open-file (ouf path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (maphash (lambda (api-key throttle)
               (write 
                (list api-key
                      :minute-queries (minute-queries-of throttle)
                      :day-queries (day-queries-of throttle)
                      :last-minute (last-minute-of throttle)
                      :last-day (last-day-of throttle))
                :stream ouf)
               (terpri ouf))
             *weather-throttles*)))

(defun load-weather-config (path)
  "Loads throttling information into registered keys."
  (when (probe-file path)
    (with-open-file (inf path :direction :input)
      (loop
         for entry = (read inf nil)
         while entry
         do
           (let ((throttle (gethash (first entry) *weather-throttles*)))
             (when throttle
               (setf (minute-queries-of throttle) (getf (rest entry) :minute-queries)
                     (day-queries-of throttle) (getf (rest entry) :day-queries)
                     (last-minute-of throttle) (getf (rest entry) :last-minute)
                     (last-day-of throttle) (getf (rest entry) :last-day))))))))

(defun throttle-query (key current-time)
  "Signals a weather-error if the key has used too queries for the
  wunderground rate limit."
  (let ((throttle (gethash key *weather-throttles*)))
    (when (null throttle)
      (error 'weather-error :message "Unconfigured API key in weather query throttle"))

    (when (and (null (max-per-day-of throttle))
               (null (max-per-minute-of throttle)))
      (return-from throttle-query nil))

    (multiple-value-bind (minute day)
        (throttle-time current-time)
      (when (> minute (last-minute-of throttle))
        (setf (minute-queries-of throttle) 0
              (last-minute-of throttle) minute))
      (when (> day (last-day-of throttle))
        (setf (day-queries-of throttle) 0
              (last-day-of throttle) day))
      (when (>= (day-queries-of throttle) (max-per-day-of throttle))
        (error 'weather-error :message "Maximum daily queries reached.  Try again tomorrow!"))
      (when (>= (minute-queries-of throttle) (max-per-minute-of throttle))
        (error 'weather-error :message "Too many queries.  Try again in a minute."))
      (incf (day-queries-of throttle))
      (incf (minute-queries-of throttle)))))

(defun find-dom-node (node tag-names)
  "Returns the node found by traversing a DOM hierarchy by tag names."
  (let ((child-node (find (first tag-names)
                          (dom:child-nodes node)
                          :key #'dom:node-name
                          :test #'string=)))
    (cond
      ((null child-node)
       nil)
      ((endp (rest tag-names))
       child-node)
      (t
       (find-dom-node child-node (rest tag-names))))))

(defun get-dom-text (node &rest tag-names)
  "Returns the text contained in the node in the DOM hierarchy."
  (let ((node (find-dom-node node tag-names)))
    (when node
      (dom:data (dom:first-child node)))))

(defun retrieve-http-document (&key scheme host path)
  "Retrieves an XML document via HTTP."
  (let ((uri (make-instance 'puri:uri :scheme scheme :host host :path path)))
    (multiple-value-bind (response status headers)
        (drakma:http-request (puri:render-uri uri nil))
      (declare (ignore headers))
      (cond
        ((/= status 200)
         (format t "weather server status = ~a" status)
         nil)
        (t
         (cxml:parse response (cxml-dom:make-dom-builder)))))))

(defun query-wunderground (key feature location)
  "Queries wunderground via the API.  May raise a weather-error."
  (throttle-query key (get-universal-time))
  (let ((doc (retrieve-http-document
              :scheme :http
              :host "api.wunderground.com"
              :path (format nil "/api/~a/~a/q/~a.xml"
                            key
                            feature
                            (drakma::url-encode location drakma:*drakma-default-external-format*)))))
    (cond
     ((null doc)
      (error 'weather-error :message "Weather server could not be reached"))
     ((get-dom-text doc "response" "error" "description")
      (error 'weather-error :message (get-dom-text doc "response" "error" "description")))
     ((get-dom-text doc "response" "results")
      (error 'weather-error :message "More than one possible location. Please be more specific."))
     (t
      (dom:first-child doc)))))

(defun retrieve-current-weather (key location)
  "Retrieves the current weather via wunderground.  Returns its stats
in multiple values.  May raise a weather-error."
  (let* ((root (query-wunderground key "conditions/forecast" location))
         (current (find-dom-node root '("current_observation")))
         (forecast (find-dom-node root '("forecast" "simpleforecast" "forecastdays" "forecastday"))))
    (values-list
     (append (list (get-dom-text current "display_location" "full"))
             (mapcar (lambda (field)
                       (get-dom-text current field))
                     '("weather" "temp_f" "dewpoint_f" "heat_index_f" "windchill_f"
                       "relative_humidity" "pressure_in" "wind_dir"
                       "wind_mph"))
             (list 
              (get-dom-text forecast "conditions")
              (get-dom-text forecast "high" "fahrenheit")
              (get-dom-text forecast "low" "fahrenheit"))))))

;;; End of wunderground interface

(defmodule weather weather-module ("weather")
  (api-key :accessor api-key-of :initform nil)
  (location :accessor location-of :initform nil)
  (warning-poll :accessor warning-poll-of :initform nil))

(defmethod initialize-module ((module weather-module) config)
  (let ((module-conf (rest (assoc 'weather config))))
    (setf (api-key-of module) (getf module-conf :api-key))
    (setf (location-of module) (getf module-conf :location))
    (setf (warning-poll-of module) (getf module-conf :warning-poll))
    (register-weather-key (api-key-of module)
                          (get-universal-time)
                          (getf module-conf :max-per-day)
                          (getf module-conf :max-per-minute)))
  (load-weather-config (orcabot-path "data/weather-throttle.lisp"))
  
  (when (null (api-key-of module))
    (format t "WARNING: Missing API key for WEATHER module~%")))

(defmethod handle-command ((module weather-module) (cmd (eql 'weather))
                           message args)
  "weather <location> - show current conditions and the day's forecast"
  (let ((location (if args
                      (join-string " " args)
                      (location-of module))))
    (handler-case
        (multiple-value-bind (city weather temp-f dewpoint heat-index windchill
                                   humidity pressure wind-dir
                                   wind-mph forecast high low)
            (retrieve-current-weather (api-key-of module) location)

          (save-weather-config (orcabot-path "data/weather-throttle.lisp"))

          (reply-to message "Current weather for ~a" city)
          (reply-to message "~a, Temp: ~a, Dewpoint: ~a, ~
                           ~@[Heat Index: ~a, ~]~
                           ~@[Wind Chill: ~a, ~]~
                           Humidity: ~a, Pressure: ~a, ~
                           Wind: ~a ~amph"
                    weather temp-f dewpoint
                    (if (string= heat-index "NA") nil heat-index)
                    (if (string= windchill "NA") nil windchill)
                    humidity pressure
                    wind-dir wind-mph)
          (reply-to message "Forecast: ~a, High: ~a, Low: ~a" forecast high low))
      (weather-error (err)
        (save-weather-config (orcabot-path "data/weather-throttle.lisp"))
        (reply-to message "~a: ~a" (source message) (message-of err))))))
