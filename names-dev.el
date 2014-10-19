;;; names-dev.el --- Developer Functions to facilitate use of names.el on your package.

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/spaces
;; Prefix: names
;; Separator: -

;;; Commentary:
;;
;; This package has some convenient functions for developers working
;; with names.el.
;; This package is installed along with names.el, but to use its
;; features you must require it explicitly:
;;
;;     (require 'names-dev)

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Code:

(require 'names)


;;; ---------------------------------------------------------------
;;; Developer Utility Functions
(defmacro names-compare-forms (name form-a form-b)
  "Test if (namespace NAME FORM-A) is the same as FORM-B."
  (declare (indent (lambda (&rest x) 0))
           (debug (symbolp sexp form)))
  `(equal
    (macroexpand-all '(define-namespace ,name :global :verbose ,form-a))
    (macroexpand-all ',form-b)))

(defmacro names-compare-forms-assert (name form-a form-b)
  "Assert if (namespace NAME FORM-A) is the same as FORM-B."
  (declare (indent (lambda (&rest x) 0))
           (debug (symbolp sexp form)))
  (cl-assert
   (names-compare-forms name form-a form-b)
    t))

(defmacro names-print (name &rest forms)
  "Return the expanded results of (namespace NAME :global :verbose FORMS).
Ideal for determining why a specific form isn't being parsed
correctly."
  (declare (indent (lambda (&rest x) 0)) (debug 0))
  `(let ((eval-expression-print-level (max eval-expression-print-level 300))
         (eval-expression-print-length (max eval-expression-print-length 300)))
     (macroexpand '(define-namespace ,name :global :verbose ,@forms))))

(defvar names-font-lock
  '(("^:autoload\\_>" 0 'font-lock-warning-face prepend)
    ("(\\(\\_<define-namespace\\_>\\)[\t \n]+\\([^\t \n]+\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face))))

(setq lisp-el-font-lock-keywords-2
      (append
       names-font-lock
       (and (boundp 'lisp-el-font-lock-keywords-2)
            lisp-el-font-lock-keywords-2)))

(defun names-eval-defun (edebug-it)
  "Identical to `eval-defun', except it works for forms inside namespaces.
Argument EDEBUG-IT is the same as `eval-defun'."
  (interactive "P")
  (require 'font-lock) ; just in case
  ;; Get the namespace, if we're in one.
  (let ((body
         (save-excursion
           (when (progn
                   (end-of-defun)
                   (beginning-of-defun)
                   (condition-case nil
                       (progn (backward-up-list)
                              (names--looking-at-namespace))
                     (error nil)))
             (cdr (read (current-buffer))))))
        form b keylist spec)
    
    ;; If we're not in a namespace, call the regular `eval-defun'.
    (if (null body)
        (eval-defun edebug-it)
      ;; If we are, expand the function in a temp buffer
      (setq name (pop body))
      (while (setq spec (names--next-keyword body))
        (setq keylist
              (append keylist spec)))
      (setq form
            (save-excursion
              (end-of-defun)
              (beginning-of-defun)
              (read (current-buffer))))
      ;; Prepare the (possibly) temporary buffer.
      (setq b (names--generate-new-buffer name form))
      (with-current-buffer b
        (erase-buffer)
        (emacs-lisp-mode)
        (save-excursion
          ;; Print everything inside the `progn'.
          (mapc
           (lambda (it) (pp it (current-buffer)))
           (cdr (macroexpand
                 `(define-namespace ,name :global :clean-output ,@keylist ,form)))))
        (font-lock-ensure)
        (eval-defun edebug-it))
      ;; Kill the buffer if we won't need it.
      (unless edebug-it
        (kill-buffer b)))))

(defun names--looking-at-namespace ()
  "Non-nil if point is at a `define-namespace' form or an alias to it."
  (when (looking-at "(\\_<")
    (save-excursion
      (forward-char 1)
      (ignore-errors
        (equal (symbol-function (intern (thing-at-point 'symbol)))
               (symbol-function 'define-namespace))))))

(defun names--generate-new-buffer (name &optional form)
  "Generate and return a new buffer.
NAME is current namespace name.
If FORM is provided, also try to use it to decide an informative
buffer name."
  (get-buffer-create
   (concat
    " *names "
    (format "%s %s"
            (or (car-safe form) (random 10000))
            (or (car-safe (cdr-safe form)) (random 10000)))
    "*")))

(eval-after-load 'lisp-mode
  '(define-key emacs-lisp-mode-map [remap eval-defun] #'names-eval-defun))

(provide 'names-dev)
;;; names-dev.el ends here.
