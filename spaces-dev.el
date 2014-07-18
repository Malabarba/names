;;; spaces-dev.el --- 

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/spaces
;; Prefix: spaces
;; Separator: -

;;; Commentary:
;;
;; 

;;; Instructions:
;;
;; INSTALLATION
;;
;; This package is available fom Melpa, you may install it by calling
;; M-x package-install.
;;
;; Alternatively, you can download it manually, place it in your
;; `load-path' and require it with
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



;;; ---------------------------------------------------------------
;;; Developer Utility Functions
(defmacro namespace-compare-forms (name form-a form-b)
  "Test if (namespace NAME FORM-A) is the same as FORM-B."
  (declare (indent (lambda (&rest x) 0)))
  (equal
   (let ((namespace--name name))
     (namespace-convert-form form-a))
   (macroexpand-all form-b)))

(defmacro namespace-compare-forms-assert (name form-a form-b)
  "Assert if (namespace NAME FORM-A) is the same as FORM-B."
  (declare (indent (lambda (&rest x) 0)))
  (cl-assert
   (equal
    (let ((namespace--name name))
      (namespace-convert-form form-a))
    (macroexpand-all form-b)) t))

(defmacro namespace-print (name &rest forms)
  "Return the expanded results of (namespace NAME :global :verbose FORMS).
Ideal for determining why a specific form isn't being parsed
correctly."
  (declare (indent (lambda (&rest x) 0)) (debug (sexp body)))
  `(let ((eval-expression-print-level (max eval-expression-print-level 300))
         (eval-expression-print-length (max eval-expression-print-length 300)))
     (macroexpand '(namespace ,name :global :verbose ,@forms))))

(provide 'spaces-dev)
;;; spaces-dev.el ends here.
