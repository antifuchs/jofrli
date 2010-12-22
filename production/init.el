(require 'cl)

;;; elisp setup

(defun xach-generic-code-setup ()
  (setq indent-tabs-mode nil)
  (local-set-key "\C-m" 'newline-and-indent))

(add-hook 'emacs-lisp-mode-hook
	  (defun xach-emacs-setup ()
	    (interactive)
	    (xach-generic-code-setup)
	    (eldoc-mode t)))

(setq indent-tabs-mode nil)

(setq make-backup-files nil)

;;; CL setup

(load (expand-file-name "/opt/lisp/ql/quicklisp/slime-helper.el"))
(require 'slime)

;;; RET on the REPL: Only send lines when at end of expression,
;;; newline and indent otherwise.
(defun asf-slime-repl-return (&optional end-of-input)
    (interactive "P")
      (if (= (point) (point-max))
                (slime-repl-return end-of-input)
              (paredit-newline)))

(define-key slime-repl-mode-map [(return)] 'asf-slime-repl-return)

(setq slime-lisp-host "10.0.9.1")
(setq slime-port 7817)

(add-hook 'lisp-mode-hook
          (defun xach-lisp-setup ()
            (interactive)
            (xach-generic-code-setup)))

(global-set-key "\C-cs" 'slime-selector)