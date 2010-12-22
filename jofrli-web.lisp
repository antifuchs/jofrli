(in-package #:jofrli)

(defparameter *base-url* (puri:parse-uri "http://jofr.li/"))

(defclass jofrli-acceptor (hunchentoot:acceptor)
     ())

(defun start (&key (api-port 6969) (redirect-port 6970))
  (hunchentoot:start (make-instance 'jofrli-acceptor :port api-port))
  (hunchentoot:start (make-instance 'jofrli-acceptor :port redirect-port
                                    :request-dispatcher 'dispatch-redirection)))

(defmethod hunchentoot:handle-request :around ((acceptor jofrli-acceptor) request)
  (redis:with-connection ()
    (call-next-method)))

(defun extract-hash (request-uri)
  (let* ((uri (puri:parse-uri request-uri))
         (path (puri:uri-parsed-path uri)))
    (car (last path))))

(defun dispatch-redirection (request)
  (let* ((hash (extract-hash (hunchentoot:request-uri request))))
    (labels ((send-404 ()
               (setf (hunchentoot:return-code*) 404)
               (sleep 1) ; this must be the worst rate-limiting scheme, ever.
               "NO FILEY."))
      (cond
        (hash
         (if-let ((url (redirect-to-url hash)))
           (progn (hunchentoot:redirect url :code hunchentoot:+http-moved-permanently+)
                  (hunchentoot:send-headers))
           (send-404)))
        (t
         (send-404))))))

(hunchentoot:define-easy-handler (shorten :uri "/shorten") (api-key url)
  (format *debug-io* "key: ~a, url: ~a~%" api-key url)
  (setf (hunchentoot:content-type*) "text/plain")    
  (cond
    ((authorized-p api-key)
     (let ((hash (intern-url url)))
       (puri:render-uri (puri:merge-uris hash *base-url*) nil)))
    (t
     (setf (hunchentoot:return-code*) 403))))

(hunchentoot:define-easy-handler (root :uri "/") ()
  (with-html-output-to-string (s)
    (:html (:head (:title "Jo Frly!"))
           (:body (:img :src "http://boinkor.net/misc/jofrli.jpg")
                  (:p "Enter URL:")
                  (:form :method "GET" :action "/shorten"
                         (:input :type "text"
                                 :name "url"
                                 :id "url")
                         (:input :type "text"
                                 :name "api-key")
                         (:input :type "submit" :name "Shorten"))
                  (:p "(Authorized users only, please)")))
    (values)))