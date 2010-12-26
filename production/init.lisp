(defpackage #:jofrli-init
  (:use #:cl))

(in-package #:jofrli-init)

(defvar *swank-port* 7817)

(load "/opt/lisp/ql/quicklisp/setup.lisp")
(push (make-pathname :name nil :type nil :defaults (merge-pathnames #P"../" *load-truename*))
      asdf:*central-registry*)

;; while lredis isn't in ql, do this:
(push (merge-pathnames #p"thirdparty/lredis/" (user-homedir-pathname))
      asdf:*central-registry*)

;; same for idna:
(push (merge-pathnames #p"thirdparty/idna/" (user-homedir-pathname))
      asdf:*central-registry*)

(ql:quickload :swank)
(ql:quickload :jofrli)

(let ((swank::*loopback-interface* "10.0.9.1"))
  (swank:create-server :port *swank-port* :dont-close t))

(print (jofrli:start :host "localhost"))