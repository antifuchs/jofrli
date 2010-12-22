(defpackage #:jofrli-init
  (:use #:cl))

(in-package #:jofrli-init)

(defvar *swank-port* 7817)

(load "/opt/lisp/ql/quicklisp/setup.lisp")
(push (merge-pathnames #P"../" *load-truename*))

(ql:quickload :swank)
(ql:quickload :jofrli)

(let ((swank::*loopback-interface* "10.0.9.1"))
  (swank:create-server :port *swank-port* :dont-close t))

(print (jofrli:run-server))