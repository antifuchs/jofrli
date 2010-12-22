(defpackage #:jofrli-init
  (:use #:cl))

(in-package #:jofrli-init)

(defvar *swank-port* 7817)

(load "/opt/lisp/ql/quicklisp/setup.lisp")
(push (merge-pathnames #P"../" *load-truename*))

(ql:quickload :jofrli)