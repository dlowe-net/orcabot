(in-package #:orcabot)

(define-condition weather-error ()
  ((message :accessor message-of :initarg :message)))

(defmethod print-object ((object weather-error) stream)
  (print-unreadable-object (object stream)
    (format stream "~a" (message-of object))))

(defvar *weather-cache-expiration* 300
  "Time in seconds for weather information to expire from the cache.")

(defvar *weather-cache* (make-hash-table :test 'equal)
  "Table of current weather information, indexed by universal time.
  The value is in the form (TIME . RESULTS), where TIME is the time of
  the query, and RESULTS is a list of the return values of
  retrieve-current-weather.")

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
         (log:log-message :warning "weather server status = ~a" status)
         nil)
        (t
         (cxml:parse response (cxml-dom:make-dom-builder)))))))

(defun query-wunderground (key feature location)
  "Queries wunderground via the API.  May raise a weather-error."
  (throttle-query key (get-universal-time))
  (log:log-message :info "querying wunderground feature: ~a, location: ~a" feature location)
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
                     '("observation_time" "weather" "icon"
                       "relative_humidity" "wind_dir"
                       "temp_f" "dewpoint_f" "heat_index_f" "windchill_f"
                       "pressure_in" "wind_mph" "wind_gust_mph"
                       "temp_c" "dewpoint_c" "heat_index_c" "windchill_c"
                       "pressure_mb" "wind_kph" "wind_gust_kph"))
             (list
              (get-dom-text forecast "conditions")
              (get-dom-text forecast "high" "fahrenheit")
              (get-dom-text forecast "low" "fahrenheit")
              (get-dom-text forecast "high" "celsius")
              (get-dom-text forecast "low" "celsius"))))))

;;; End of wunderground interface

(defun load-location-db (path)
  (let ((result (make-hash-table :test 'equal)))
    (with-open-file (inf path :if-does-not-exist nil)
      (when inf
        (loop
           for tuple = (read inf nil)
           while tuple
           do (setf (gethash (first tuple) result)
                    (second tuple)))))
    result))

(defun save-location-db (db path)
  (with-open-file (ouf path :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (maphash (lambda (k v)
               (print (list k v) ouf))
             db)))

(defmodule weather weather-module ("weather")
  (api-key :accessor api-key-of :initform nil)
  (default-location :accessor default-location-of :initform nil)
  (locations :accessor locations-of :initform (make-hash-table :test 'equal))
  (warning-poll :accessor warning-poll-of :initform nil))

(defmethod initialize-module ((module weather-module) config)
  (let ((module-conf (rest (assoc 'weather config))))
    (setf (api-key-of module) (getf module-conf :api-key))
    (setf (default-location-of module) (getf module-conf :default-location))
    (setf (warning-poll-of module) (getf module-conf :warning-poll))
    (register-weather-key (api-key-of module)
                          (get-universal-time)
                          (getf module-conf :max-per-day)
                          (getf module-conf :max-per-minute)))
  (setf (locations-of module)
        (load-location-db (data-path "weather-locations.lisp")))
  (load-weather-config (data-path "weather-throttle.lisp"))

  (when (null (api-key-of module))
    (log:log-message :warning "WARNING: Missing API key for WEATHER module~%")))

(defmethod about-module ((module weather-module) stream)
  (format stream "- Weather information provided by wunderground.com~%"))

(defun retrieve-cached-weather (key location)
  (let* ((now (get-universal-time))
         (cached-weather (gethash location *weather-cache*)))
    (if (or (null cached-weather)
            (> now (+ (car cached-weather) *weather-cache-expiration*)))
        (let ((weather (multiple-value-list (retrieve-current-weather key location))))
          (setf (gethash location *weather-cache*)
                (cons now weather))
          (values-list weather))
        (values-list (rest cached-weather)))))

(defun parse-weather-args (module message raw-args)
  (let (opts args)
    (dolist (arg raw-args)
      (if (string= "-" arg :end2 1)
          (push arg opts)
          (push arg args)))
    (values
     (find "--metric" opts :test #'string=)
     (find "--set" opts :test #'string=)
     (find "--fucking" opts :test #'string=)
     (find "--doge" opts :test #'string=)
     (cond
       (args
        (join-to-string " " (nreverse args)))
       ((gethash (source message) (locations-of module)))
       ((gethash (first (arguments message)) (locations-of module)))
       (t
        (default-location-of module))))))

(defun display-weather (module message metricp location)
  (handler-case
      (multiple-value-bind (city local-time weather icon humidity wind-dir
                                 temp-f dewpoint-f heat-index-f windchill-f
                                 pressure-in wind-mph wind-gust-mph
                                 temp-c dewpoint-c heat-index-c windchill-c
                                 pressure-mb wind-kph wind-gust-kph
                                 forecast high-f low-f high-c low-c)
          (retrieve-cached-weather (api-key-of module) location)
        (declare (ignore icon))
        (save-weather-config (data-path "weather-throttle.lisp"))
        (setf local-time (ppcre:regex-replace "^Last Updated on " local-time ""))

        (reply-to message "Current weather for ~a @ ~a" city local-time)
        (cond
          (metricp
           (Reply-to message "~a, Temp: ~aC, Dewpoint: ~aC, ~
                           ~@[Heat Index: ~aC, ~]~
                           ~@[Wind Chill: ~aC, ~]~
                           Humidity: ~a, Pressure: ~amb, ~
                           Wind: ~a ~a/~a kph"
                     weather temp-c dewpoint-c
                     (if (string= heat-index-c "NA") nil heat-index-c)
                     (if (string= windchill-c "NA") nil windchill-c)
                     humidity pressure-mb
                     wind-dir wind-kph wind-gust-kph)
           (reply-to message "Forecast: ~a, High: ~aC, Low: ~aC" forecast high-c low-c))
          (t
           (reply-to message "~a, Temp: ~aF, Dewpoint: ~aF, ~
                           ~@[Heat Index: ~aF, ~]~
                           ~@[Wind Chill: ~aF, ~]~
                           Humidity: ~a, Pressure: ~ain, ~
                           Wind: ~a ~a/~a mph"
                     weather temp-f dewpoint-f
                     (if (string= heat-index-f "NA") nil heat-index-f)
                     (if (string= windchill-f "NA") nil windchill-f)
                     humidity pressure-in
                     wind-dir wind-mph wind-gust-mph)
           (reply-to message "Forecast: ~a, High: ~aF, Low: ~aF" forecast high-f low-f))))
    (weather-error (err)
      (save-weather-config (data-path "weather-throttle.lisp"))
      (reply-to message "~a: ~a" (source message) (message-of err)))))

(defun display-fucking-weather (module message metricp location)
  (handler-case
      (multiple-value-bind (city local-time weather icon humidity wind-dir
                                 temp-f dewpoint-f heat-index-f windchill-f
                                 pressure-in wind-mph wind-gust-mph
                                 temp-c)
          (retrieve-cached-weather (api-key-of module) location)
        (declare (ignore icon humidity wind-dir
                         dewpoint-f heat-index-f windchill-f
                         pressure-in wind-mph wind-gust-mph))

        (save-weather-config (data-path "weather-throttle.lisp"))
        (setf local-time (ppcre:regex-replace "^Last Updated on " local-time ""))

        ;; values for temperature gotten from International Journal of
        ;; Biometeorology March 2010, Volume 54, Issue 2, pp 193-209
        ;; Qualitative and quantitative descriptions of temperature: a
        ;; study of the terminology used by local television weather
        ;; forecasters to describe thermal sensation
        ;; Jeffrey C. Brunskill
        (let* ((temp-f (read-from-string temp-f))
               (summary (cond
                         ((search "rain" weather :test #'char-equal) "raining")
                         ((search "sleet" weather :test #'char-equal) "sleeting")
                         ((search "snow" weather :test #'char-equal) "snowing")
                         ((search "hail" weather :test #'char-equal) "hailing")
                         ((> temp-f 100) "hell")
                         ((> temp-f 90) "hot")
                         ((> temp-f 80) "warm")
                         ((> temp-f 60) "nice")
                         ((> temp-f 50) "brisk")
                         ((> temp-f 40) "chilly")
                         ((> temp-f 32) "cold")
                         (t "freezing"))))
          (reply-to message "Current weather for ~a: ~a?!? It's fucking ~a."
                    city
                    (if metricp temp-c temp-f)
                    summary)))
    (weather-error (err)
      (save-weather-config (data-path "weather-throttle.lisp"))
      (reply-to message "~a: ~a" (source message) (message-of err)))))

;; :a - adjective
;; :v - verb
;; :n - countable noun (cups)
;; :m - mass noun (water)
(defparameter +weather-word-class+
  (alexandria:alist-hash-table '(("amaze" . :a)
                                 ("bake" . :v)
                                 ("balmy" . :a)
                                 ("blind" . :a)
                                 ("boiling" . :a)
                                 ("bolt" . :v)
                                 ("boring" . :a)
                                 ("brrr" . :a)
                                 ("brrrr" . :a)
                                 ("brrrrr" . :a)
                                 ("chill" . :a)
                                 ("chilly" . :a)
                                 ("clear" . :a)
                                 ("climate change" . :n)
                                 ("climate" . :m)
                                 ("cloud" . :n)
                                 ("clouds" . :n)
                                 ("cloudy" . :a)
                                 ("coat" . :n)
                                 ("cold" . :a)
                                 ("concern" . :v)
                                 ("cool" . :a)
                                 ("crash" . :v)
                                 ("creepy" . :a)
                                 ("crisp" . :a)
                                 ("cumulus" . :n)
                                 ("dark" . :a)
                                 ("darkness" . :a)
                                 ("depress" . :a)
                                 ("dying" . :a)
                                 ("festive" . :a)
                                 ("freeze" . :v)
                                 ("freezing" . :a)
                                 ("frost" . :m)
                                 ("frosty" . :a)
                                 ("global warming" . :m)
                                 ("gloomy" . :a)
                                 ("heat" . :m)
                                 ("hibernate" . :v)
                                 ("hide" . :v)
                                 ("ice" . :m)
                                 ("icy" . :a)
                                 ("joy" . :m)
                                 ("lightning" . :n)
                                 ("loud" . :a)
                                 ("lovely" . :a)
                                 ("medium" . :a)
                                 ("melt" . :v)
                                 ("mild" . :a)
                                 ("mist" . :m)
                                 ("moderate" . :a)
                                 ("moon" . :n)
                                 ("nice" . :a)
                                 ("night" . :m)
                                 ("numb" . :a)
                                 ("okay" . :a)
                                 ("overcast" . :a)
                                 ("polar vortex" . :n)
                                 ("powder" . :m)
                                 ("raindrops" . :n)
                                 ("ridiculous" . :a)
                                 ("scare" . :v)
                                 ("scary" . :a)
                                 ("scattered" . :a)
                                 ("shady" . :a)
                                 ("shiny" . :a)
                                 ("shiver" . :v)
                                 ("shower" . :v)
                                 ("sky" . :m)
                                 ("sleet" . :m)
                                 ("slippery" . :a)
                                 ("snow" . :m)
                                 ("snowflake" . :n)
                                 ("soak" . :v)
                                 ("soft" . :a)
                                 ("space" . :m)
                                 ("spook" . :v)
                                 ("star" . :n)
                                 ("stars" . :n)
                                 ("suffer" . :v)
                                 ("sun" . :m)
                                 ("sweating" . :a)
                                 ("terrible" . :a)
                                 ("thunder" . :n)
                                 ("vapor" . :m)
                                 ("warmth" . :m)
                                 ("weather" . :m)
                                 ("wet" . :a)
                                 ("whatever" . :a)
                                 ("white" . :a)
                                 ("winter" . :m)
                                 ("wonderful" . :a)
                                 ("yuck" . :a))
                               :test 'equal))

(defun generate-doge (count first-words amaze-words words)
  (with-output-to-string (s)
    (loop
       with wow-amaze = nil
       with words-left = (alexandria:shuffle (make-array (length words)
                                                         :initial-contents words
                                                         :fill-pointer t))
       repeat count do
         (cond
           ((and (zerop (random 6))
                 (not wow-amaze))
            (format s " ~a." (alexandria:random-elt amaze-words))
            (setf wow-amaze t))
           (t
            (let* ((selected-word (vector-pop words-left))
                   (selected-category (gethash selected-word +weather-word-class+))
                   (first-word (alexandria:random-elt (rest (assoc selected-category first-words)))))
              (format s " ~a ~a."
                      first-word
                      selected-word)))))))



(defun doge-weather (count temp-c icon prefix-flavor amaze-flavor)
  (let ((such-temp
         (cond
           ((<= temp-c -30) '("winter" "freeze" "polar vortex" "ridiculous" "hibernate" "climate change"))
           ((<= temp-c -15) '("cold" "freeze" "shiver" "ice" "yuck" "climate change"))
           ((<= temp-c -7) '("icy" "winter" "chill" "crisp" "brrrrr" "cool"))
           ((<= temp-c 0) '("icy" "frost" "numb" "shiver" "brrr" "chilly"))
           ((<= temp-c 10) '("chilly" "concern" "coat" "frosty" "brrrr"))
           ((<= temp-c 20) '("moderate" "mild" "okay" "medium" "cool" "whatever"))
           ((<= temp-c 30) '("heat" "warmth" "climate" "sweating" "balmy" "nice"))
           (t '("boiling" "bake" "melt" "dying" "suffer" "global warming"))))
        (wow-conditions
         (cond
           ((or (string= icon "clear") (string= icon "sunny"))
            '("clear" "sky" "lovely" "amaze" "wonderful""sun" "weather"))
           ((string= icon "cloudy")
            '("gloomy" "clouds" "shady" "boring" "weather"))
           ((or (string= icon "fog") (string= icon "hazy"))
            '("mist" "vapor" "creepy" "spook" "blind" "darkness" "gloomy" "depress" "weather"))
           ((or (string= icon "mostlycloudy") (string= icon "partlysunny"))
            '("cloudy" "scattered" "overcast" "weather"))
           ((or (string= icon "partlycloudy") (string= icon "mostlysunny"))
            '("cloud" "okay" "cumulus" "amaze" "sky" "weather"))
           ((string= icon "rain")
            '("raindrops" "soak" "wet" "slippery" "shower" "terrible" "weather"))
           ((string= icon "sleet")
            '("sleet" "freezing" "wet" "slippery" "icy" "terrible" "weather"))
           ((or (string= icon "snow") (string= icon "flurries"))
            '("snow" "white" "soft" "icy" "snowflake" "powder" "joy" "shiny" "festive" "weather"))
           ((string= icon "tstorms")
            '("thunder" "loud" "scare" "bolt" "lightning" "terrible" "hide" "weather"))

           ((or (string= icon "nt_clear") (string= icon "nt_sunny"))
            '("night" "amaze" "clear" "lovely" "wonderful" "sky" "stars" "moon" "dark" "weather"))
           ((string= icon "nt_cloudy")
            '("gloomy" "clouds" "shady" "boring" "weather"))
           ((or (string= icon "nt_fog") (string= icon "nt_haze"))
            '("mist" "vapor" "creepy" "spook" "blind" "darkness" "gloomy" "depress" "weather"))
           ((or (string= icon "nt_mostlycloudy") (string= icon "nt_partlysunny"))
            '("cloud" "scattered" "night" "weather"))
           ((or (string= icon "nt_partlycloudy") (string= icon "nt_mostlysunny"))
            '("dark" "cumulus" "amaze" "cloud" "star" "space" "weather"))
           ((string= icon "nt_rain")
            '("raindrops" "soak" "wet" "slippery" "shower" "terrible" "scary" "cloud" "night" "weather"))
           ((string= icon "nt_sleet")
            '("sleet" "freezing" "wet" "slippery" "icy" "terrible" "weather" "night" "scary"))
           ((or (string= icon "nt_snow") (string= icon "nt_flurries"))
            '("snow" "white" "night time" "slippery" "icy" "snowflake" "powder" "joy" "shiny" "festive" "weather"))
           ((string= icon "nt_tstorms")
            '("scary" "thunder" "loud" "crash" "bolt" "lightning" "terrible" "hide" "weather")))))
    (generate-doge count
                   prefix-flavor
                   amaze-flavor
                   (append such-temp wow-conditions))))

(defun display-doge-weather (module message metricp location prefix-flavor amaze-flavor)
  (handler-case
      (multiple-value-bind (city local-time weather icon humidity wind-dir
                                 temp-f dewpoint-f heat-index-f windchill-f
                                 pressure-in wind-mph wind-gust-mph
                                 temp-c)
          (retrieve-cached-weather (api-key-of module) location)
        (declare (ignore weather humidity wind-dir
                         dewpoint-f heat-index-f windchill-f
                         pressure-in wind-mph wind-gust-mph))

        (save-weather-config (data-path "weather-throttle.lisp"))
        (setf local-time (ppcre:regex-replace "^Last Updated on " local-time ""))

        (let* ((temp-c (read-from-string temp-c)))
          (reply-to message "~a... ~a.~a"
                    city
                    (if metricp temp-c temp-f)
                    (doge-weather 4 temp-c icon prefix-flavor amaze-flavor))))
    (weather-error (err)
      (save-weather-config (data-path "weather-throttle.lisp"))
      (reply-to message "~a: ~a" (source message) (message-of err)))))


(defmethod handle-command ((module weather-module) (cmd (eql 'weather))
                           message args)
  "weather [--metric] <location> - show current conditions and the day's forecast
weather [--set] <location> - set the channel default location
"
  (multiple-value-bind (metricp setp fuckingp dogep location)
      (parse-weather-args module message args)
    (if setp
        (cond
          ((null location)
           (reply-to message "You must specify a location to set."))
          ((message-target-is-channel-p message)
           (setf (gethash (first (arguments message)) (locations-of module)) location)
           (save-location-db (locations-of module)
                             (data-path "weather-locations.lisp"))
           (reply-to message "The default location for ~a is now ~a."
                     (first (arguments message))
                     location))
          (t
           (reply-to message "You cannot --set in a private message." location)))
        (cond
          ((null location)
           (reply-to message "You must supply a location."))
          ((and fuckingp dogep)
           (display-doge-weather module message metricp location
                                 '((:a "fucking" "goddamn" "crappy")
                                   (:v "crappy" "crap" "fucking" "wtf")
                                   (:n "fucking" "goddamn" "crappy" "stupid")
                                   (:m "stupid" "wtf" "goddamn" "fucking"))
                                 #("omg" "fuck" "crap" "dammit")))
          (fuckingp
           (display-fucking-weather module message metricp location))
          (dogep
           (display-doge-weather module message metricp location
                                 '((:a "much" "many" "such")
                                   (:v "much" "many" "so" "such" "very")
                                   (:n "much" "so" "such" "very")
                                   (:m "many" "so" "such" "very"))
                                 #("wow" "amaze" "excite")))
          (t
           (display-weather module message metricp location))))))
