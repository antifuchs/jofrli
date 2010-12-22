;;;; jofrli.asd

(asdf:defsystem #:jofrli
  :serial t
  :depends-on (#:cl-redis
               #:hunchentoot
               #:md5
               #:alexandria
               #:puri
               #:uuid
               #:cl-who)
  :components ((:file "package")
               (:file "redis-based-redirector")
               (:file "jofrli-web")))

