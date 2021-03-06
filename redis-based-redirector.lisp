;;;; jofrli.lisp

(in-package #:jofrli)

(labels ((unicode-range (low high &rest sans)
           (loop for i from low to high
                 unless (member i sans)
                   collect (code-char i))))
 (defparameter *chars* (concatenate 'string
                                    "0123456789abcdefghijklmnopqrstuvwxyz-"
                                    (unicode-range 9728 9853)     ; misc
                                    (unicode-range 9632 9727)     ; geometric shapes
                                    (unicode-range 8592 8703)     ; arrows
                                    (unicode-range 9600 9631)     ; block elements
                                    (unicode-range 8448 8527)     ; letterlike symbols
                                    (unicode-range 10176 10219)   ; miscellaneous mathematical symbols
                                    (unicode-range 8704 8959)     ; mathematical operators
                                    (unicode-range 10240 10495)   ; braille patterns
                                    (unicode-range 8528 8591)     ; number forms (vulgar fractions, roman)
                                    (unicode-range 65104 65135)   ; small form variants (tiny ampersand!)
                                    (unicode-range 9216 9279)     ; control pictures
                                    )))

(defparameter *max-fill* 0.6)
(defparameter *initial-min-length* 2)

(defun redis-get-integer-or-initialize (name default)
  (or (when-let ((value (redis:get name)))
        (parse-integer value))
      (redis:incrby name default)))

(defun minimum-length ()
  (redis-get-integer-or-initialize :min_length *initial-min-length*))

(defun max-count-at-length (&optional (length (minimum-length)))
  (expt (length *chars*) length))

(defun degree-filled ()
  (let ((current-count (redis-get-integer-or-initialize :current_count_at_length 0)))
    (if (zerop current-count)
        0
        (/ (max-count-at-length) current-count))))

(defun increment-min-count ()
  (redis:incr :min_length)
  (redis:set :current_count_at_length 0))

(defun find-url (id)
  (redis:hget "url" id))

(defun collision-p (id url)
  (let ((current-value (find-url id)))
    (values (and (not (null current-value))
                 (not (string= url current-value)))
            (and current-value (puri:parse-uri current-value)))))

(defun resolve-alias (alias)
  (redis:hget "aliases" alias))

(defun visit-key (id)
  (format nil "visits/~a" id))

(defun store-url (id url normalized-url)
  (assert (null (nth-value 1 (collision-p id url))))
  (redis:hset "url" id url)
  (when-let ((idn (idna:to-ascii id)))
    (unless (string= idn id)
      (redis:hset "aliases" idn id)))
  (redis:hset "hashed-urls" normalized-url id)
  (redis:ltrim (visit-key id) 0 0)
  (redis:incrby :current_count_at_length 1)
  (redis:save)
  id)

(defun serialize-using-base (the-value base-chars)
  (with-output-to-string (s)
    (loop with base = (length base-chars)
          with last-digit = nil
          for value = the-value then (truncate value base)
          for index =  (rem value base)
          for current-digit = (aref base-chars index)
          until (zerop value)
          do (when (eql current-digit last-digit)
               (setf current-digit (aref base-chars (1+ index))))
          do (setf last-digit current-digit)
          do (write-char current-digit s))))

(defun hash-url (url &optional (rehash 0))
  "Returns a possible ID value for URL"
  (let* ((the-url (puri:parse-uri (if (zerop rehash)
                                      url
                                      (format nil "~a#~a" url rehash))))
         (hash (md5:md5sum-sequence (puri:render-uri the-url nil)))
         (hash-as-a-number (reduce (lambda (val aggregate) (+ val (* 255 aggregate))) hash))
         (hash-value (rem hash-as-a-number (max-count-at-length))))
    (serialize-using-base hash-value *chars*)))

(defun intern-url (url)
  (let ((normalized-url (puri:render-uri (puri:parse-uri url) nil)))
    (when-let ((previously (redis:hget "hashed-urls" normalized-url)))
      (return-from intern-url previously))
    (loop for rehash from 0
          for hash = (hash-url url rehash)
          when (not (collision-p hash url))
            do (catch 'rehash
                 (handler-bind ((error (lambda (c)
                                         (declare (ignore c))
                                         (throw 'rehash nil))))
                   (multiple-value-bind (collisionp found-url) (collision-p hash url)
                     (cond
                       ((and (not collisionp) (null found-url))
                        (return (store-url hash url normalized-url)))
                       ((not collisionp)
                        (return hash)))))))))

(defun redirect-to-url (hash)
  (when-let ((url (find-url hash)))
    (redis:lpush (visit-key hash) (prin1-to-string (get-universal-time)))
    (redis:bgsave)
    url))

;;; Introspection

(defun list-urls ()
  (let ((idns (make-hash-table :test #'equal)))
   (loop for (idn id) on (redis:hgetall "aliases") by #'cddr
         do (setf (gethash id idns) idn))
   (loop for (url id) on (redis:hgetall "hashed-urls") by #'cddr
         for visits = (redis:llen (visit-key id))
         collect (list :url url :id id :visits visits :idn (gethash id idns)))))

;;; API authorization

(defun authorized-p (authkey)
  (redis:sismember "authorization" authkey))

(defun generate-authkey (&optional (secret ""))
  ;; XXX: I am (obviously) not a cryptographer, but I'm lazy. This is
  ;; not an effective deterrent against somebody willing to guess your
  ;; secret and your uuid pattern:
  (let ((authkey (ironclad:byte-array-to-hex-string
                  (md5:md5sum-sequence (format nil "~a-~a" secret (uuid:make-v1-uuid))))))
    (redis:sadd "authorization" authkey)
    authkey))