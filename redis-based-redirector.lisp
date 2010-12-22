;;;; jofrli.lisp

(in-package #:jofrli)

(defparameter *chars* "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-x&/=y_~@")
(defparameter *max-fill* 0.6)
(defparameter *initial-min-length* 2)

(defvar *secret* "")

(defun redis-get-integer-or-initialize (name default)
  (or (when-let ((value (redis:red-get name)))
        (parse-integer value))
      (redis:red-incrby name default)))

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
  (redis:red-incr :min_length)
  (redis:red-set :current_count_at_length 0))

(defun find-url (id)
  (redis:red-hget "url" id))

(defun collision-p (id url)
  (let ((current-value (find-url id)))
    (values (and (not (null current-value))
                 (not (string= url current-value)))
            (and current-value (puri:parse-uri current-value)))))

(defun visit-key (id)
  (format nil "visits/~a" id))

(defun store-url (id url)
  (assert (null (nth-value 1 (collision-p id url))))
  (redis:red-hset "url" id url)
  (redis:red-ltrim (visit-key id) 0 0)
  (redis:red-incrby :current_count_at_length 1)
  (redis:red-save)
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
         ;; Note: we use MD5 only to disambiguate. Could just have hashed things myself.
         (stupid-repr (reduce (lambda (val aggregate) (+ val (* 255 aggregate))) hash))
         (hash-value (rem stupid-repr (max-count-at-length))))
    (serialize-using-base hash-value *chars*)))

(defun intern-url (url)
  (loop for rehash from 0
        for hash = (hash-url url rehash)
        when (not (collision-p hash url))
        do (multiple-value-bind (collisionp found-url) (collision-p hash url)
             (cond
               ((and (not collisionp) (null found-url))
                (return (store-url hash url)))
               ((not collisionp)
                (return hash))))))

(defun redirect-to-url (hash)
  (when-let ((url (find-url hash)))
    (redis:red-lpush (visit-key hash) (prin1-to-string (get-universal-time)))
    (redis:red-bgsave)
    url))

;;; API authorization

(defun authorized-p (authkey)
  (redis:red-sismember "authorization" authkey))

(defun generate-authkey (&optional (secret ""))
  ;; XXX: I am (obviously) not a cryptographer, and lazy. This is not
  ;; an effective deterrent:
  (let ((authkey (ironclad:byte-array-to-hex-string
                  (md5:md5sum-sequence (format nil "~a-~a" secret (uuid:make-v1-uuid))))))
    (redis:red-sadd "authorization" authkey)
    authkey))