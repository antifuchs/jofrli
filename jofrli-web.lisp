(in-package #:jofrli)

(defparameter *base-url* (puri:parse-uri "http://jofr.li/"))
q
(defclass jofrli-acceptor (hunchentoot:acceptor)
     ())

(defun start (&key (redirect-port 6969) (host nil))
  (hunchentoot:start (make-instance 'jofrli-acceptor :port redirect-port :address host
                                    :request-dispatcher 'dispatch-redirection)))

(defmethod hunchentoot:handle-request :around ((acceptor jofrli-acceptor) request)
  (redis:with-connection ()
    (call-next-method)))

(defun extract-hash (host request-uri)
  (let* ((subdomain-end (mismatch host (make-host-name) :from-end t))
         (subdomain (and subdomain-end (not (zerop subdomain-end))
                         (subseq host 0 (1- subdomain-end)))))
    (if subdomain
        (or (resolve-alias subdomain) subdomain)
        (let ((uri (puri:parse-uri request-uri)))
          ;; ok, this is a horrible fallback on paths: Since we send
          ;; unicode chars, and http paths come back from hunchentoot
          ;; decoded as latin-1, we convert to octets, then re-decode
          ;; as utf-8.
          (babel:octets-to-string (babel:string-to-octets (subseq (puri:uri-path uri) 1) :encoding :latin-1)
                                  :encoding :utf-8)))))

(defun dispatch-redirection (request)
  (let* ((hash (extract-hash (hunchentoot:host) (hunchentoot:request-uri*))))
    (labels ((send-404 ()
               (setf (hunchentoot:return-code*) 404)
               (sleep 1) ; this must be the worst rate-limiting scheme, ever.
               "NO FILEY."))
      (cond
        ((and (member hash '(nil "" "api" "shorten"))
              (hunchentoot:dispatch-easy-handlers request))
         (funcall (hunchentoot:dispatch-easy-handlers request)))
        (hash
         (if-let ((url (redirect-to-url hash)))
           (progn (hunchentoot:redirect url :code hunchentoot:+http-moved-permanently+)
                  (hunchentoot:send-headers))
           (send-404)))
        (t (send-404))))))

(defun make-host-name (&optional id)
  (format nil "~@[~a.~]~a~@[:~A~]" id (puri:uri-host *base-url*) (puri:uri-port *base-url*)))

(hunchentoot:define-easy-handler (shorten :uri "/shorten") (api-key url)
  (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")    
  (cond
    ((authorized-p api-key)
     (let* ((hash (intern-url url)))
       ;; (format *debug-io* "key: ~a, url: ~a => ~a~%" api-key url id)
       (write-sequence (babel:string-to-octets (format nil "http://~a" (make-host-name hash)) :encoding :utf-8)
                       (hunchentoot:send-headers))))
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
                  (:p "(Authorized users only, please)")
                  (:a :href "http://github.com/antifuchs/jofrli"
                      "(source code)")))
    (values)))