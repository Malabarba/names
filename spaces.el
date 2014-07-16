;;; spaces.el --- Namespaces for emacs-lisp. Works like C++ namespaces to avoid name clobbering.

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/namespace
;; Version: 0.5
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

(require 'noflet)
(require 'edebug)
(require 'cl-lib)
;; (require 'dash)


;;; ---------------------------------------------------------------
;;; Variables
(defconst namespace-version "0.5" "Version of the spaces.el package.")

(defvar namespace--name nil
  "Name of the current namespace inside the `namespace' macro.")
(defvar namespace--regexp nil "Regexp matching `namespace--name'.")

(defvar namespace--bound nil
  "List of variables defined in this namespace.")
(defvar namespace--fbound nil
  "List of functions defined in this namespace.")
(defvar namespace--macro nil 
  "List of macros defined in this namespace.")

(defvar namespace--keywords nil
  "Keywords that were passed to the current namespace.
Current possible keywords are :let-vars :global :protection")

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
  (declare (debug (form)))
  `(intern (format "%s%s" namespace--name ,sbl)))


;;; ---------------------------------------------------------------
;;; The Main Macro and Main Function.
;;;###autoload
(defmacro namespace (name &rest body)
  "Inside the namespace NAME, execute BODY.
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
  (let* ((namespace--name name)
         (namespace--regexp
          (concat "\\`" (regexp-quote (symbol-name name))))
         (namespace--protection "\\`::")
         (namespace--bound
          (namespace--remove-namespace-from-list
           (namespace--filter-if-bound byte-compile-bound-variables)
           (namespace--filter-if-bound byte-compile-constants)
           (namespace--filter-if-bound byte-compile-variables)))
         (namespace--fbound
          (namespace--remove-namespace-from-list
           (namespace--filter-if-bound byte-compile-macro-environment 'macrop)
           (namespace--filter-if-bound byte-compile-function-environment 'macrop)))
         (namespace--macro
          (namespace--remove-namespace-from-list
           (namespace--filter-if-bound byte-compile-macro-environment (lambda (x) (not (macrop x))))
           (namespace--filter-if-bound byte-compile-function-environment (lambda (x) (not (macrop x))))))
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

(defmacro namespace--filter-if-bound (var &optional pred)
  "If VAR is bound and is a list, take the car of its elements which satify PRED."
  (declare (debug (symbolp function-form)))
  (when (boundp var)
    `(cl-remove-if
      ,pred (mapcar (lambda (x) (or (car-safe x) x)) ,var))))

;;;###autoload
(defun namespace-convert-form (form)
  "Do namespace conversion on FORM.
FORM is any legal elisp form.
Namespace name is defined by the global variable `namespace--name'.

See macro `namespace' for more information."
  (cond
   ((null form) form)
   ;; Function calls
   ((listp form)
    (let ((kar (car form))
          func)
      (cond
       ;; Namespaced Functions/Macros
       ((namespace--fboundp kar)
        (namespace--message "Namespaced: %s" kar)
        (namespace--args-of-function-or-macro
         (namespace--prepend kar) (cdr form) (namespace--macrop kar)))
       ;; Function-like forms that get special handling
       ;; That's anything with a namespace--convert-%s function defined.
       ((fboundp (setq func (intern (format "namespace--convert-%s" kar))))
        (namespace--message "Special handling: %s" func)
        (funcall func form))
       ;; General functions/macros
       (t
        (namespace--message "Regular handling: %s" kar)
        ;; If symbol is protected, clean it; otherwise, use it as-is.
        (let ((clean-kar (or (namespace--remove-protection kar) kar)))
          (namespace--args-of-function-or-macro
           clean-kar (cdr form) (macrop clean-kar)))))))
   ;; Variables
   ((symbolp form)
    (namespace--message "Symbol handling: %s" form)
    ;; If symbol is protected, clean it and don't namespace it.
    (or (namespace--remove-protection form)
        ;; Otherwise, namespace if possible.
        (if (namespace--boundp form)
            (namespace--prepend form)
          form)))
   ;; Values
   (t form)))

(defun namespace--message (f &rest rest)
  "If :verbose is on, pass F and REST to `message'."
  (when (namespace--keyword :verbose)
    (apply 'message (concat "[spaces] " f) rest)))

(defun namespace--warn (f &rest rest)
  "Pass F and REST to `message', unless byte-compiling."
  (unless (and (boundp 'byte-compile-function-environment)
               byte-compile-function-environment)
    (apply 'message (concat "[spaces] " f) rest)))


;;; ---------------------------------------------------------------
;;; Some auxiliary functions
(defun namespace--error-if-using-vars ()
  "Remind the developer that variables are not customizable."
  (mapcar
   (lambda (x)
     (when (eval x)
       (error "[spaces] Global value of variable %s should be nil! %s"
              x "Set it using keywords instead")))
   '(namespace--name namespace--regexp namespace--bound
                     namespace--macro
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

(defun namespace--remove-protection (symbol)
  "Remove the leading :: from SYMBOL if possible, otherwise return nil."
  (namespace--remove-regexp symbol namespace--protection))

(defun namespace--remove-regexp (s r)
  "Return S with regexp R removed, or nil if S didn't match."
  (let ((name (symbol-name s)))
    (when (string-match r name)
      (intern (replace-match "" nil nil name)))))

(defun namespace--quote-p (sbl)
  "Is SBL a function which quotes its argument?"
  (memq sbl '(quote function)))

(defun namespace--fboundp (sbl)
  "Is namespace+SBL a fboundp symbol?"
  (or (memq sbl namespace--fbound)
      (memq sbl namespace--macro)
      (and (namespace--keyword :global)
           (fboundp (namespace--prepend sbl)))))

(defun namespace--macrop (sbl)
  "Is namespace+SBL a fboundp symbol?"
  (or (memq sbl namespace--macro)
      (and (namespace--keyword :global)
           (macrop (namespace--prepend sbl)))))

(defun namespace--keyword (keyword)
  "Was KEYWORD one of the keywords passed to the `namespace' macro?"
  (memq keyword namespace--keywords))

(defun namespace--boundp (sbl)
  "Is namespace+SBL a boundp symbol?
If SBL has a let binding, that takes precendence so this also
returns nil."
  (and (null (memq sbl namespace--local-vars))
       (or (memq sbl namespace--bound)
           (and (namespace--keyword :global)
                (boundp (namespace--prepend sbl))))))

(defun namespace--args-of-function-or-macro (name args macro)
  "Check whether NAME is a function or a macro, and handle ARGS accordingly."
  (if macro
      ;; Macros are complicated.
      (namespace--macro-args-using-edebug (cons name args))
    ;; We just convert the arguments of functions.
    (cons name (mapcar 'namespace-convert-form args))))

(defvar namespace--is-inside-macro nil 
  "Auxiliary var used in `namespace--macro-args-using-edebug'.")

(defun namespace--macro-args-using-edebug (form)
  "Namespace the arguments of macro FORM by hacking into edebug.
This takes advantage of the fact that macros (should) declare a
`debug' specification which tells us which arguments are actually
lisp forms.

Ideally, we would read this specification ourselves and see how
it matches (cdr FORM), but that would take a lot of work and
we'd be reimplementing something that edebug already does
phenomenally. So we hack into edebug instead."
  (condition-case nil
   (car
    (with-temp-buffer
      (pp form 'insert)
      (goto-char (point-min))
      (let ((edebug-all-forms t)
            (edebug-all-defs t)
            (namespace--is-inside-macro 0))
        (noflet ((edebug-form (cursor) (namespace--edebug-form cursor))
                 (edebug-make-enter-wrapper
                  (forms)
                  (setq edebug-def-name
                        (or edebug-def-name
                            edebug-old-def-name
                            (cl-gensym "spaces-edebug-anon")))
                  ;; (caddr (cadr (car (last (funcall this-fn forms)))))
                  forms))
          (edebug-read-top-level-form)))))
   (invalid-read-syntax
    (namespace--warn "Couldn't namespace this macro using its (debug ...) declaration: %s"
                     form)
    form)))

(defun namespace--edebug-form (cursor)
  "Parse form given by CURSOR using edebug, and namespace it if necessary."
  (require 'edebug)
  ;; Return the instrumented form for the following form.
  ;; Add the point offsets to the edebug-offset-list for the form.
  (let* ((form (edebug-top-element-required cursor "Expected form"))
         (offset (edebug-top-offset cursor))
         ;; This variable equals the current `namespace--edebug-form' depth.

         ;; We don't want to convert the entire form that was passed
         ;; to `namespace--macro-args-using-edebug' (= level 0), since
         ;; the head of that was already converted and it would lead
         ;; to an infinite loop.

         ;; We DO want to convert the arguments that edebug identifies
         ;; as forms (= level 1).
         
         ;; We also don't want to do anything once we're inside these
         ;; level-1 arguments (>= level 2), because that will already
         ;; be done by our own recursion when we call
         ;; `namespace-convert-fore' on the level-1 forms.
         
         (func (if (= namespace--is-inside-macro 1) 'namespace-convert-form 'identity))
         (namespace--is-inside-macro (1+ namespace--is-inside-macro)))
    (namespace--message " [Edebug] ran into this: %S" form)
    (prog1
        (cond
         ((consp form) ;; The first offset for a list form is for the list form itself.
          (let* ((head (car form))
                 (spec (and (symbolp head) (get-edebug-spec head)))
                 (new-cursor (edebug-new-cursor form offset)))
            ;; Find out if this is a defining form from first symbol.
            ;; An indirect spec would not work here, yet.
            (if (and (consp spec) (eq '&define (car spec)))
                (edebug-defining-form
                 new-cursor
                 (car offset) ;; before the form
                 (edebug-after-offset cursor)
                 (cons (symbol-name head) (cdr spec)))
              ;; Wrap a regular form.
              (funcall func (edebug-list-form new-cursor)))))

         ((symbolp form)
          (funcall func form))

         ;; Anything else is self-evaluating.
         (t form))
      (edebug-move-cursor cursor))))


;;; ---------------------------------------------------------------
;;; Interpreting keywords passed to the main macro.
(defun namespace--handle-keyword (body)
  "Call the function that handles the keyword at the car of BODY.
The function must be named `namespace--keyword-KEY' (including
the :), and must return whatever information is to be stored in
`namespace--keywords'. The car of BODY will be popped later, so
the function generally shouldn't do that. For simple keywords,
the function can simply be an alias for `car'.

However, if the keyword takes one or more arguments, then this
function should indeed pop the car of BODY that many times."
  (let ((func (intern (format "namespace--keyword-%s" (car body)))))
    (if (fboundp func)
        (funcall func body)
      (error "[spaces] Keyword %s not recognized" (car body)))))

(defun namespace--keyword-:protection (body)
  "Return a cons with car and cadr of BODY and pop car."
  (let ((kw (car body))
        (val (symbol-name (cadr body))))
    (cl-assert (stringp val))
    (setq body (cdr body))
    (setq namespace--protection
          (format "\\`%s" (regexp-quote val)))
    (cons kw val)))

(defalias 'namespace--keyword-:let-vars 'car
  "The :let-vars keyword indicates variables assigned in let-bind are candidates for namespacing.")

(defalias 'namespace--keyword-:verbose 'car
  "The :verbose keyword causes a message to be called on each special form.")

(defalias 'namespace--keyword-:global 'car
  "The :global keyword is used to accept namespaced names from outside current namespace definition.
It will also be used when we implement something similar to
`eval-defun'." )


;;; ---------------------------------------------------------------
;;; Interpreting the actual forms found in BODY of the main macro.
;;
;; This is where the heavy work is done.
;;
;; If you'd like to implement support for some special form, simply
;; define a function called `namespace--convert-FORM-NAME' along the
;; lines of the functions defined below. It will be automatically used
;; whenever that form is found.
(defvar namespace--name-already-prefixed nil
  "Whether some outer function already prefixed the name of the current defalias.")

(defun namespace--convert-defalias (form)
  "Special treatment for `defalias' FORM."
  (let ((dont-prefix namespace--name-already-prefixed))
    (setq namespace--name-already-prefixed nil)
    (list
     (car form)
     (if dont-prefix
         (cadr form)
       (let ((name (eval (cadr form)))) ;;ignore-errors
         (add-to-list 'namespace--fbound name)
         (list 'quote (namespace--prepend name))))
     (namespace-convert-form (cadr (cdr form))))))

;;; Defun, defmacro, and defsubst macros are pretty predictable. So we
;;; expand them and handle them like defaliases, instead of handling
;;; as general macros.
(defun namespace--convert-defun (form)
  "Special treatment for `defun' FORM."
  (let ((namespace--name-already-prefixed t)
        (name (cadr form)))
    (add-to-list 'namespace--fbound name)
    (namespace-convert-form
     (macroexpand
      (cons
       (car form)
       (cons (namespace--prepend name)
             (cddr form)))))))
(defalias 'namespace--convert-defmacro 'namespace--convert-defun)
(defalias 'namespace--convert-defsubst 'namespace--convert-defun)

(defun namespace--convert-defvar (form)
  "Special treatment for `defvar' FORM."
  (let ((name (cadr form)))
    (add-to-list 'namespace--bound name)
    (append
     (list
      (car form)
      (namespace--prepend name))
     (mapcar 'namespace-convert-form (cdr (cdr form))))))

(defalias 'namespace--convert-defconst 'namespace--convert-defvar
  "Special treatment for `defconst' FORM.")

(defun namespace--convert-defvaralias (form)
  "Special treatment for `defvaralias' FORM."
  (let ((name (eval (cadr form)))
        (name2 (eval (caddr form))))
    (add-to-list 'namespace--bound name)
    (append
     (list
      (car form)
      (list 'quote (namespace--prepend name))
      (list 'quote (namespace-convert-form name2)))
     (mapcar 'namespace-convert-form (cdr (cdr form))))))

(defun namespace--convert-defcustom (form)
  "Special treatment for `defcustom' FORM."
  (namespace--convert-custom-declare-variable
   (macroexpand form)))

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
  (let ((name (if (namespace--keyword :let-vars)
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

(provide 'spaces)

;;; spaces.el ends here
