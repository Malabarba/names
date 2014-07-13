;;; spaces.el --- Namespace for emacs-lisp. Works like C++ namespaces to avoid name clobbering.

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/namespace
;; Version: 0.1a
;; Keywords:
;; Prefix: namespace
;; Separator: -

;;; Commentary:
;;
;;

;;; Instructions:
;;
;; INSTALLATION
;;
;; This package is available fom Melpa, you may install it by calling
;; M-x package-install RET spaces.
;;
;; Alternatively, you can download it manually, place it in your
;; `load-path' and require it with
;;
;;     (require 'spaces)

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
;; 0.1a - 2014/05/20 - Created File.
;;; Code:

(require 'cl-lib)
;; (require 'dash)

(defconst namespace-version "0.1a" "Version of the spaces.el package.")

(defvar namespace--name nil)
(defvar namespace--regexp nil)

(defvar namespace--bound nil)
(defvar namespace--fbound nil)

(defvar namespace--keywords nil
  "Keywords that were passed to the current namespace.

:let-vars")

(defvar namespace--local-vars nil
  "Non-global vars that are let/lambda bound at the moment.
These won't be namespaced, as local takes priority over namespace.")

(defvar namespace--protection nil
  "Leading chars used to identify protected symbols.
Don't customise this.
Instead use the :protection keyword when defining the
namespace.")

(defmacro namespace--prepend (sbl)
  "Return namespace+SBL."
  `(intern (format "%s%s" namespace--name ,sbl)))

;;;###autoload
(defmacro namespace (name &rest body)
  "Execute BODY inside the namespace NAME.
NAME can be any symbol (not quoted), but it's highly recommended
to use some form of separator (such as on of : / -).

This has two main effects:

1. Any definitions inside BODY will have NAME prepended to the
symbol given. Ex:
    (namespace foo:
    (defvar bar 1 \"docs\")
    )
expands to
    (defvar foo:bar 1 \"docs\")


2. Any function calls and variable names get NAME prepended to
them if possible. Ex:
    (namespace foo:
    (message \"%s\" my-var)
    )
expands to
    (foo:message \"%s\" foo:my-var)
but only if `foo:message' has a function definition. Similarly,
`my-var' becomes `foo:my-var', but only if `foo:my-var' has
a variable definition.

If `foo:message' is not a defined function, the above would
expand instead to
    (message \"%s\" foo:my-var)

===============================

Immediately after NAME you may add keywords which customize this
behaviour:

1. :let-vars

   If this is present, variables defined in let forms become
   namespaced (just like defvars). If this is absent, they are
   preserved.

   For example, assuming `foo:mo' has a variable definition, the
   code
      (namespace foo-
      (let ((bar mo)) ...)
      )
   expands to
      (let ((bar foo-mo)) ...)
   while
      (namespace foo- :let-vars
      (let ((bar mo)) ...)
      )
   expands to
      (let ((foo-bar foo-mo)) ...)

\(fn NAME [KEYWORDS] BODY)"
  (declare (indent (lambda (&rest x) 0)))
  (namespace--error-if-using-vars)
  (let ((namespace--name name)
        (namespace--regexp
         (concat "\\`" (regexp-quote (symbol-name name))))
        (namespace--protection "\\`:")
        (namespace--bound
         (namespace--remove-namespace-from-list
          byte-compile-bound-variables
          byte-compile-constants byte-compile-variables))
        (namespace--fbound
         (namespace--remove-namespace-from-list
          (mapcar 'car byte-compile-macro-environment)
          (mapcar 'car byte-compile-function-environment)))
        namespace--keywords namespace--local-vars)
    ;; Read keywords
    (while (keywordp (car-safe body))
      (push (namespace--handle-keyword body) namespace--keywords)
      (setq body (cdr body)))
    ;; First have to populate the bound and fbound lists. So we read
    ;; the entire form (without evaluating it).
    (mapc 'namespace-convert-form body)
    ;; Then we go back and actually namespace the form, which we
    ;; return so that it can be evaluated.
    (cons 'progn (mapcar 'namespace-convert-form body))))

(defun namespace--error-if-using-vars ()
  "Remind the developer that variables are not customizable."
  (mapcar
   (lambda (x)
     (when (eval x) 
       (error "Global value of variable %s should be nil! %s"
              x "Set it using keywords instead")))
   '(namespace--name namespace--regexp namespace--bound
                     namespace--fbound namespace--keywords
                     namespace--local-vars namespace--protection)))

(defun namespace--remove-namespace-from-list (&rest lists)
  "Return a concatenated un-namespaced version of LISTS.
Symbols in LISTS that aren't namespaced are removed, symbols that
are namespaced become un-namespaced."
  (delq nil (mapcar 'namespace--remove-namespace (apply 'append lists))))

(defun namespace--remove-namespace (symbol)
  "Return SYMBOL with namespace removed, or nil if S wasn't namespaced."
  (namespace--remove-regexp symbol namespace--regexp))

(defun namespace--remove-protection (sym)
  "Remove the leading :: from SYM, if it has any."
  (namespace--remove-namespace namespace--protection sym))

(defun namespace--remove-regexp (s r)
  "Return S with regexp R removed, or nil if S didn't match."
  (let ((name (symbol-name s)))
    (if (string-match r name)
        (intern (replace-match "" nil nil name))
      s)))

(defun namespace--handle-keyword (body)
  "Call the function that handles the keyword at the car of BODY.
The function must be named `namespace--keyword-KEY' (including
the :), and must return whatever information is to be sored in
`namespace--keywords'. The car of BODY will be popped later, so
the function generally shouldn't do that. For simple keywords,
the function can simply be an alias for `car'.

However, if the keyword takes an argument, then this function
should indeed pop the car of BODY."
  (let ((func (fboundp (intern (format "namespace--keyword-%s" (car body))))))
    (if (fboundp func)
        (funcall func body)
      (error "keyword %s not recognized." (car body)))))

(defun namespace--keyword-:protection (body)
  "Return a cons with car and cadr of BODY and pop car."
  (let ((kw (car body))
        (val (symbol-name (cadr body))))
    (cl-assert (stringp val))
    (setq body (cdr body))
    (setq namespace--protection
          (format "\\`%s" (regexp-quote val)))
    (setq namespace--protection-length (length val))
    (cons kw val)))

(defalias 'namespace--keyword-:let-vars 'car)

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

;;;###autoload
(defun namespace-convert-form (form)
  "Do namespace conversion on FORM.
FORM is any legal elisp form.
Namespace name is defined by the global variable
`namespace--name'."
  (cond
   ((null form) form)
   ;; Function calls
   ((listp form)
    (let ((kar (car form)))
      (when (namespace--defvar-p kar)
        (add-to-list 'namespace--bound (cadr form)))
      (cond ;; Special forms:
       ;; Namespaced Functions
       ((namespace--fboundp kar)
        (let ((name (namespace--prepend kar)))
          (if (macrop name)
              (namespace-convert-form (macroexpand (cons name (cdr form))))
            (cons name
                  (mapcar 'namespace-convert-form (cdr form))))))
       ;; Functions-like forms that get special handling
       ;; That's anything with a namespace--convert-%s function defined
       ;; Currently they are quote/function, lambda, let, cond
       ((fboundp (intern (format "namespace--convert-%s" kar)))
        (message "%s" (format "namespace--convert-%s" kar))
        (funcall (intern (format "namespace--convert-%s" kar)) form))
       ;; Macros
       ((macrop kar)
        (namespace-convert-form (macroexpand form)))
       ;; General functions
       (t (cons (namespace--remove-protection kar)
                (mapcar 'namespace-convert-form (cdr form)))))))
   ;; Variables
   ((symbolp form)
    (namespace--remove-protection
     (if (namespace--boundp form)
         (namespace--prepend form)
       form)))
   ;; Values
   (t form)))

(defun namespace--convert-defalias (form)
  "Special treatment for `defalias' FORM."
  (let ((name (eval (cadr form)))) ;;ignore-errors
    (add-to-list 'namespace--fbound name)
    (list
     (car form)
     (list 'quote (namespace--prepend name))
     (namespace-convert-form (cadr (cdr form))))))

(defun namespace--convert-defvar (form)
  "Special treatment for `defvar' FORM."
  (let ((name (eval (cadr form)))) ;;ignore-errors
    (add-to-list 'namespace--bound name)
    (append
     (list
      (car form)
      (list 'quote (namespace--prepend name)))
     (mapcar 'namespace-convert-form (cdr (cdr form))))))

(defalias 'namespace--convert-defconst 'namespace--convert-defvar
  "Special treatment for `defconst' FORM.")

(defun namespace--convert-custom-declare-variable (form)
  "Special treatment for `custom-declare-variable' FORM."
  (let ((name (eval (cadr form))) ;;ignore-errors
        (val (cl-caddr form)))
    (add-to-list 'namespace--bound name)
    (append
     (list
      (car form)
      (list 'quote (namespace--prepend name)) ;cadr
      ;; The DEFAULT argument is explicitly evaluated by
      ;; `custom-declare-variable', so it should be safe to namespace
      ;; even when quoted. Plus, we need to do this because
      ;; defcustom quotes this part.
      (if (namespace--quote-p (car-safe val))
          (list (car val) (namespace-convert-form (cadr val)))
        (namespace-convert-form val))
      (namespace-convert-form        (car (cdr (cdr (cdr form))))))
     (mapcar 'namespace-convert-form (cdr (cdr (cdr (cdr form))))))))

(defun namespace--convert-quote (form)
  "Special treatment for `quote/function' FORM.
When FORM is (quote argument), argument is parsed for namespacing
only if it is a lambda form.

Anything else (a symbol or a general list) is too arbitrary to
be logically namespaced and will be preserved as-is.

Note, however, that the value of the NAME argument of a
\"definition-type\" forms is ALWAYS namespaced, regardless of
whether the form was a quote."
  (let ((kadr (cadr form)))
    (if (eq (car-safe kadr) 'lambda)
        (list (car form) (namespace-convert-form kadr))
      form)))

(defalias 'namespace--convert-function 'namespace--convert-quote)

(defun namespace--convert-\` (form)
  "Special treatment for backtick FORM.
Currently, we just return FORM without namespacing anything."
  form)

(defun namespace--convert-lambda (form)
  "Special treatment for `lambda' FORM."
  (let ((namespace--local-vars
         (append (remove '&rest (remove '&optional (cadr form)))
                 namespace--local-vars))
        (forms (cdr (cdr form))))
    (append
     (list (car form)
           (cadr form))
     (when (stringp (car forms))
       (let ((out (car forms)))
         (setq forms (cdr forms))
         (list out)))
     (when (eq 'interactive (car-safe (car forms)))
       (let ((out (car forms)))
         (setq forms (cdr forms))
         (list (cons (car out) (mapcar 'namespace-convert-form (cdr out))))))
     (mapcar 'namespace-convert-form forms))))

(defun namespace--let-var-convert-then-add (sym add)
  "Try to convert SYM if :let-vars is in use.
If ADD is non-nil, add resulting symbol to `namespace--local-vars'."
  (let ((name (if (memq :let-vars namespace--keywords)
                  (namespace-convert-form sym)
                sym)))
    (when add (add-to-list 'namespace--local-vars name))
    name))

(defun namespace--convert-let (form &optional star)
  "Special treatment for `let' FORM.
If STAR is non-nil, parse as a `let*'."
  (let* ((namespace--local-vars namespace--local-vars)
         (vars
          (mapcar
           (lambda (x)
             (if (car-safe x)
                 (list (namespace--let-var-convert-then-add (car x) star)
                       (namespace-convert-form (cadr x)))
               (namespace--let-var-convert-then-add x star)))
           (cadr form))))
    ;; Each var defined in a regular `let' only becomes protected after
    ;; all others have been defined.
    (unless star
      (setq namespace--local-vars
            (append
             (mapcar (lambda (x) (or (car-safe x) x)) vars)
             namespace--local-vars)))
    (append
     (list (car form) vars)
     (mapcar 'namespace-convert-form (cddr form)))))

(defun namespace--convert-let* (form)
  "Special treatment for `let' FORM."
  (namespace--convert-let form t))

(defun namespace--convert-cond (form)
  "Special treatment for `cond' FORM."
  (cons
   (car form)
   (mapcar
    (lambda (x)
      (cons (namespace-convert-form (car x))
            (mapcar 'namespace-convert-form (cdr x))))
    (cdr form))))

(defun namespace--quote-p (sbl)
  "Is SBL a function which quotes its argument?"
  (member sbl '(quote function)))

(defun namespace--defvar-p (sbl)
  "Is SBL a function which defines variables?"
  (member sbl '()))

(defun namespace--defun-p (sbl)
  "Is SBL a function which defines functions?"
  (member sbl '(defun defmacro defmacro* defalias defsubst defsubst*)))

(defun namespace--fboundp (sbl)
  "Is namespace+SBL a fboundp symbol?"
  (or (member sbl namespace--fbound)
      (fboundp (namespace--prepend sbl))))

(defun namespace--boundp (sbl)
  "Is namespace+SBL a boundp symbol?
If SBL has a let binding, that takes precendence so this also
returns nil."
  (and (null (member sbl namespace--local-vars))
       (or (member sbl namespace--bound)
           (boundp (namespace--prepend sbl)))))

;;; TODO: This isn't actually used.
(defun namespace--convert (sbl pred)
  "Convert SBL to namespace+SBL, if (PRED SBL) is non-nil."
  (if (funcall pred sbl)
      (namespace--prepend sbl)
    sbl))

(provide 'namespace)

;;; spaces.el ends here.
