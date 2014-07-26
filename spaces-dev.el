;;; spaces-dev.el --- Developer Functions to facilitate use of spaces.el on your package.

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/spaces
;; Prefix: spaces
;; Separator: -

;;; Commentary:
;;
;; This package has some convenient functions for developers working
;; with spaces.el.
;; This package is installed along with spaces.el, but to use its
;; features you must require it explicitly:
;;
;;     (require 'spaces-dev)

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

;;; Change Log:
;; 0.1a - 2014/07/18 - Created File.
;;; Code:

(require 'spaces)


;;; ---------------------------------------------------------------
;;; Developer Utility Functions
(defmacro space-compare-forms (name form-a form-b)
  "Test if (namespace NAME FORM-A) is the same as FORM-B."
  (declare (indent (lambda (&rest x) 0))
           (debug (symbolp sexp form)))
  (equal
   (let ((spaces--name name))
     (spaces-convert-form form-a))
   (macroexpand-all form-b)))

(defmacro spaces-compare-forms-assert (name form-a form-b)
  "Assert if (namespace NAME FORM-A) is the same as FORM-B."
  (declare (indent (lambda (&rest x) 0))
           (debug (symbolp sexp form)))
  (cl-assert
   (equal
    (let ((spaces--name name))
      (spaces-convert-form form-a))
    (macroexpand-all form-b)) t))

(defmacro spaces-print (name &rest forms)
  "Return the expanded results of (namespace NAME :global :verbose FORMS).
Ideal for determining why a specific form isn't being parsed
correctly."
  (declare (indent (lambda (&rest x) 0)) (debug 0))
  `(let ((eval-expression-print-level (max eval-expression-print-level 300))
         (eval-expression-print-length (max eval-expression-print-length 300)))
     (macroexpand '(defspace ,name :global :verbose ,@forms))))

(defvar spaces-font-lock
  '(("^:autoload\\_>" 0 'font-lock-warning-face prepend)
    ("(\\(\\_<defspace\\_>\\)[\t \n]+\\([^\t \n]+\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face))))

(setq lisp-el-font-lock-keywords-2
      (append
       spaces-font-lock
       lisp-el-font-lock-keywords-2))

(provide 'spaces-dev)
;;; spaces-dev.el ends here.
