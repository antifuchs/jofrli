;;;; package.lisp

(defpackage #:jofrli
  (:use #:cl #:alexandria #:cl-who)
  (:export #:start #:generate-authkey
           #:*initial-min-length* #:*max-fill* #:*base-url*))

