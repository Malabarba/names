;;; spaces.el --- Namespaces for emacs-lisp. Works like C++ namespaces to avoid name clobbering.

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/spaces
;; Version: 0.5
;; Package-Requires: ((emacs "24.1") (cl-lib "1.0") (noflet "0.0.11"))
;; Keywords:
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

(eval-when-compile '(require 'noflet))
(eval-when-compile '(require 'edebug))

;;; Support
(unless (fboundp 'function-get)
  (defun function-get (f prop &optional autoload)
    "Return the value of property PROP of function F.
If AUTOLOAD is non-nil and F is autoloaded, try to autoload it
in the hope that it will set PROP.  If AUTOLOAD is `macro', only do it
if it's an autoloaded macro."
    (let ((val nil))
      (while (and (symbolp f)
                  (null (setq val (get f prop)))
                  (fboundp f))
        (let ((fundef (symbol-function f)))
          (if (and autoload (autoloadp fundef)
                   (not (equal fundef
                               (autoload-do-load fundef f
                                                 (if (eq autoload 'macro)
                                                     'macro)))))
              nil                         ;Re-try `get' on the same `f'.
            (setq f fundef))))
      val)))


;;; ---------------------------------------------------------------
;;; Variables
(defconst spaces-version "0.5" "Version of the spaces.el package.")

(defvar spaces--name nil
  "Name of the current namespace inside the `namespace' macro.")
(defvar spaces--regexp nil "Regexp matching `spaces--name'.")

(defvar spaces--bound nil
  "List of variables defined in this namespace.")
(defvar spaces--fbound nil
  "List of functions defined in this namespace.")
(defvar spaces--macro nil 
  "List of macros defined in this namespace.")

(defvar spaces--keywords nil
  "Keywords that were passed to the current namespace.
Current possible keywords are :let-vars :global :protection")

(defvar spaces--local-vars nil
  "Non-global vars that are let/lambda bound at the moment.
These won't be namespaced, as local takes priority over namespace.")

(defvar spaces--protection nil
  "Leading chars used to identify protected symbols.
Don't customise this.
Instead use the :protection keyword when defining the
namespace.")

(defvar spaces--current-run nil 
  "Either 1 or 2, depending on which runthrough we're in.")

(defvar spaces--var-list
  '(spaces--name spaces--regexp spaces--bound
                 spaces--macro spaces--current-run
                 spaces--fbound spaces--keywords
                 spaces--local-vars spaces--protection) 
  "List of variables the user shouldn't touch.")

(defvar spaces--inside-make-autoload nil 
  "Used in `make-autolaod' to indicate to `defspace' that we're generating autoloads.")

(defmacro spaces--prepend (sbl)
  "Return namespace+SBL."
  (declare (debug (symbolp)))
  `(intern (format "%s%s" spaces--name ,sbl)))


(defmacro spaces--filter-if-bound (var &optional pred)
  "If VAR is bound and is a list, take the car of its elements which satify PRED."
  (declare (debug (symbolp function-form)))
  (when (boundp var)
    `(cl-remove-if
      ,pred (mapcar (lambda (x) (or (car-safe x) x)) ,var))))


;;; ---------------------------------------------------------------
;;; The Main Macro and Main Function.
;;;###autoload
(defmacro defspace (name &rest body)
  "Inside the namespace NAME, execute BODY.
NAME can be any symbol (not quoted), but it's highly recommended
to use some form of separator (such as :, /, or -).

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

AUTOLOAD

In order for `defspace' to work with ;;;###autoload comments just
replace all instances of ;;;###autoload inside your `defspace'
with :autoload, and then add an ;;;###autoload tag just above
your `defspace'.

===============================

KEYWORDS

Immediately after NAME you may add keywords which customize the
behaviour of `defspace'. For a description of these keywords, see
the manual on
http://github.com/Bruce-Connor/spaces

\(fn NAME [KEYWORDS] BODY)"
  (declare (indent (lambda (&rest x) 0))
           (debug (&define name body)))
  (spaces--error-if-using-vars)
  (unwind-protect
      (let* ((spaces--name name)
             (spaces--regexp
              (concat "\\`" (regexp-quote (symbol-name name))))
             (spaces--current-run 0)
             ;; Use the :protection keyword to change this.
             (spaces--protection "\\`::")
             (spaces--bound
              (spaces--remove-namespace-from-list
               (spaces--filter-if-bound byte-compile-bound-variables)
               (spaces--filter-if-bound byte-compile-constants)
               (spaces--filter-if-bound byte-compile-variables)))
             (spaces--fbound
              (spaces--remove-namespace-from-list
               (spaces--filter-if-bound byte-compile-macro-environment 'macrop)
               (spaces--filter-if-bound byte-compile-function-environment 'macrop)))
             (spaces--macro
              (spaces--remove-namespace-from-list
               (spaces--filter-if-bound byte-compile-macro-environment (lambda (x) (not (macrop x))))
               (spaces--filter-if-bound byte-compile-function-environment (lambda (x) (not (macrop x))))))
             spaces--keywords spaces--local-vars)
        ;; Read keywords
        (while (and (keywordp (car-safe body))
                    ;; :autoload is part of body and is handled below.
                    (null (eq (car-safe body) :autoload)))
          (push (spaces--handle-keyword body) spaces--keywords)
          (setq body (cdr body)))
        ;; First have to populate the bound and fbound lists. So we read
        ;; the entire form (without evaluating it).
        (mapc 'spaces-convert-form body)
        (incf spaces--current-run)
        ;; Then we go back and actually namespace the entire form, which
        ;; we return so that it can be evaluated.
        (cons 'progn (mapcar 'spaces-convert-form
                             ;; Unless we're in `make-autoload', then just return autoloads.
                             (if spaces--inside-make-autoload
                                 (spaces--extract-autoloads body)
                               body))))
    (mapc (lambda (x) (set x nil)) spaces--var-list)))

(defun spaces--extract-autoloads (body)
  "Return a list of the forms in BODY preceded by :autoload."
  (let (acons)
    (when (setq acons (memq :autoload body))
      (cons
       (cadr acons)
       (spaces--extract-autoloads (cdr (cdr acons)))))))

;;;###autoload
(defadvice make-autoload (before spaces-before-make-autoload-advice
                                 (form file &optional expansion) activate)
  "Make sure `make-autoload' understands defspace.
Use a letbind to indicate to `defspace' that we're generating autoloads."
  (let ((spaces--inside-make-autoload t)
        space)
    (when (eq (car-safe form) 'defspace)
      (setq space (macroexpand form))
      (ad-set-arg 0 space)
      (ad-set-arg 2 'expansion))))

(defun spaces-convert-form (form)
  "Do namespace conversion on FORM.
FORM is any legal elisp form.
Namespace name is defined by the global variable `spaces--name'.

See macro `namespace' for more information."
  (cond
   ((null form) form)
   ;; Function calls
   ((consp form)
    (let ((kar (car form))
          func)
      (cond
       ;; If symbol is protected, clean it.
       ((setq func (spaces--remove-protection kar))
        (spaces--message "Protected: %s" kar)
        ;; And decide what to do with it.
        (spaces--handle-args func (cdr form)))
       
       ;; If kar is a list, either 1) it's a lambda form, 2) it's a
       ;; macro we don't know about yet, 3) we have a bug.
       ((consp kar)
        (if (and (null (functionp kar))
                 (> spaces--current-run 1))
            (spaces--warn "Ran into the following strange form.
Either it's an undefined macro, a macro with a bad debug declaration, or we have a bug.\n%s" form)
          (mapcar 'spaces-convert-form form)))
       
       ;; Namespaced Functions/Macros
       ((spaces--fboundp kar)
        (spaces--message "Namespaced: %s" kar)
        (spaces--args-of-function-or-macro
         (spaces--prepend kar) (cdr form) (spaces--macrop kar)))
       
       ;; General functions/macros/special-forms
       (t (spaces--handle-args kar (cdr form))))))
   ;; Variables
   ((symbolp form)
    (spaces--message "Symbol handling: %s" form)
    ;; If symbol is protected, clean it and don't namespace it.
    (or (spaces--remove-protection form)
        ;; Otherwise, namespace if possible.
        (if (spaces--boundp form)
            (spaces--prepend form)
          form)))
   ;; Values
   (t form)))

(defun spaces--handle-args (func args)
  "Generic handling for the form (FUNC . ARGS), without namespacing FUNC."
  (let ((handler (intern (format "spaces--convert-%s" func))))
    ;; Some function-like forms get special handling.
    ;; That's anything with a spaces--convert-%s function defined.
    (if (fboundp handler)
        (progn (spaces--message "Special handling: %s" handler)
               (funcall handler (cons func args)))
      ;; If it isn't special, it's either a function or a macro.
      (spaces--args-of-function-or-macro func args (macrop func)))))

(defun spaces--message (f &rest rest)
  "If :verbose is on, pass F and REST to `message'."
  (when (spaces--keyword :verbose)
    (apply 'message (concat "[spaces] " f) rest)))

(defun spaces--warn (f &rest rest)
  "Pass F and REST to `message', unless byte-compiling."
  (unless (and (boundp 'byte-compile-function-environment)
               byte-compile-function-environment)
    (apply 'message (concat "[spaces] " f) rest)))


;;; ---------------------------------------------------------------
;;; Some auxiliary functions
(defun spaces--error-if-using-vars ()
  "Remind the developer that variables are not customizable."
  (mapcar
   (lambda (x)
     (when (eval x)
       (error "[spaces] Global value of variable %s should be nil! %s"
              x "Set it using keywords instead")))
   spaces--var-list))

(defun spaces--remove-namespace-from-list (&rest lists)
  "Return a concatenated un-namespaced version of LISTS.
Symbols in LISTS that aren't namespaced are removed, symbols that
are namespaced become un-namespaced."
  (delq nil (mapcar 'spaces--remove-namespace (apply 'append lists))))

(defun spaces--remove-namespace (symbol)
  "Return SYMBOL with namespace removed, or nil if S wasn't namespaced."
  (spaces--remove-regexp symbol spaces--regexp))

(defun spaces--remove-protection (symbol)
  "Remove the leading :: from SYMBOL if possible, otherwise return nil."
  (spaces--remove-regexp symbol spaces--protection))

(defun spaces--remove-regexp (s r)
  "Return S with regexp R removed, or nil if S didn't match."
  (let ((name (symbol-name s)))
    (when (string-match r name)
      (intern (replace-match "" nil nil name)))))

(defun spaces--quote-p (sbl)
  "Is SBL a function which quotes its argument?"
  (memq sbl '(quote function)))

(defun spaces--fboundp (sbl)
  "Is namespace+SBL a fboundp symbol?"
  (or (memq sbl spaces--fbound)
      (memq sbl spaces--macro)
      (and (spaces--keyword :global)
           (fboundp (spaces--prepend sbl)))))

(defun spaces--macrop (sbl)
  "Is namespace+SBL a fboundp symbol?"
  (or (memq sbl spaces--macro)
      (and (spaces--keyword :global)
           (macrop (spaces--prepend sbl)))))

(defun spaces--keyword (keyword)
  "Was KEYWORD one of the keywords passed to the `namespace' macro?"
  (memq keyword spaces--keywords))

(defun spaces--boundp (sbl)
  "Is namespace+SBL a boundp symbol?
If SBL has a let binding, that takes precendence so this also
returns nil."
  (and (null (memq sbl spaces--local-vars))
       (or (memq sbl spaces--bound)
           (and (spaces--keyword :global)
                (boundp (spaces--prepend sbl))))))

;;; This is calling edebug even on `when' and `unless'
(defun spaces--args-of-function-or-macro (name args macro)
  "Check whether NAME is a function or a macro, and handle ARGS accordingly."
  (if macro
      (cl-case (spaces--get-edebug-spec name)
        ;; Macros where we evaluate all arguments are like functions.
        ((t) (spaces--args-of-function-or-macro name args nil))
        ;; Macros where nothing is evaluated we can just return.
        (0 (cons name args))
        ;; Other macros are complicated. Ask edebug for help.
        (t (spaces--macro-args-using-edebug (cons name args))))
    ;; We just convert the arguments of functions.
    (cons name (mapcar 'spaces-convert-form args))))

(defun spaces--get-edebug-spec (name)
  "Get 'edebug-form-spec property of symbol NAME."
  ;; Get the spec of symbol resolving all indirection.
  (let ((spec nil)
        (indirect name))
    (while (progn
             (and (symbolp indirect)
                  (setq indirect
                        (function-get indirect 'edebug-form-spec 'macro))))
      ;; (edebug-trace "indirection: %s" edebug-form-spec)
      (setq spec indirect))
    spec))

(defvar spaces--is-inside-macro nil 
  "Auxiliary var used in `spaces--macro-args-using-edebug'.")

(defun spaces--macro-args-using-edebug (form)
  "Namespace the arguments of macro FORM by hacking into edebug.
This takes advantage of the fact that macros (should) declare a
`debug' specification which tells us which arguments are actually
lisp forms.

Ideally, we would read this specification ourselves and see how
it matches (cdr FORM), but that would take a lot of work and
we'd be reimplementing something that edebug already does
phenomenally. So we hack into edebug instead."
  (require 'edebug)
  (require 'noflet)
  (condition-case nil
      (with-temp-buffer
        (pp form 'insert)
        (goto-char (point-min))
        (let ((edebug-all-forms t)
              (edebug-all-defs t)
              (spaces--is-inside-macro 0))
          (noflet ((message
                    (&rest _)
                    (if (spaces--keyword :verbose)
                        (apply this-fn _)
                      (apply 'format _)))
                   (edebug-form (cursor) (spaces--edebug-form cursor))
                   (edebug-make-enter-wrapper
                    (forms)
                    (setq edebug-def-name
                          (or edebug-def-name
                              edebug-old-def-name
                              (cl-gensym "spaces-edebug-anon")))
                    (cons 'progn forms)))
            (edebug-read-top-level-form))))
    (invalid-read-syntax
     (spaces--warn "Couldn't namespace this macro using its (debug ...) declaration: %s"
                   form)
     form)))

(defun spaces--edebug-form (cursor)
  "Parse form given by CURSOR using edebug, and namespace it if necessary."
  (require 'edebug)
  ;; Return the instrumented form for the following form.
  ;; Add the point offsets to the edebug-offset-list for the form.
  (let* ((form (edebug-top-element-required cursor "Expected form"))
         (offset (edebug-top-offset cursor))
         ;; This variable equals the current `spaces--edebug-form' depth.

         ;; We don't want to convert the entire form that was passed
         ;; to `spaces--macro-args-using-edebug' (= level 0), since
         ;; the head of that was already converted and it would lead
         ;; to an infinite loop.

         ;; We DO want to convert the arguments that edebug identifies
         ;; as forms (= level 1).
         
         ;; We also don't want to do anything once we're inside these
         ;; level-1 arguments (>= level 2), because that will already
         ;; be done by our own recursion when we call
         ;; `spaces-convert-fore' on the level-1 forms.
         
         (func (if (= spaces--is-inside-macro 1) 'spaces-convert-form 'identity))
         (spaces--is-inside-macro (1+ spaces--is-inside-macro)))
    (spaces--message " [Edebug] ran into this: %S" form)
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
(defun spaces--handle-keyword (body)
  "Call the function that handles the keyword at the car of BODY.
The function must be named `spaces--keyword-KEY' (including the
colon), and must return whatever information is to be stored in
`spaces--keywords'. Usually, this information is at least the
keyword itself.

The car of BODY is the keyword itself. It will be popped later,
so the function generally shouldn't do that. For simple keywords,
the function can simply be an alias for `car'.

However, if the keyword takes one or more arguments, then this
function should indeed pop the car of BODY that many times."
  (let ((func (intern (format "spaces--keyword-%s" (car body)))))
    (if (fboundp func)
        (funcall func body)
      (error "[spaces] Keyword %s not recognized" (car body)))))

(defun spaces--keyword-:protection (body)
  "Return a cons with car and cadr of BODY and pop car."
  (let ((kw (car body))
        (val (symbol-name (cadr body))))
    (cl-assert (stringp val))
    (setq body (cdr body))
    (setq spaces--protection
          (format "\\`%s" (regexp-quote val)))
    (cons kw val)))

(defalias 'spaces--keyword-:let-vars 'car
  "The :let-vars keyword indicates variables assigned in let-bind are candidates for namespacing.")

(defalias 'spaces--keyword-:verbose 'car
  "The :verbose keyword causes a message to be called on each special form.")

(defalias 'spaces--keyword-:global 'car
  "The :global keyword is used to accept namespaced names from outside current namespace definition.
It will also be used when we implement something similar to
`eval-defun'." )

(defalias 'spaces--keyword-:assume-var-quote 'car
  "The :assume-var-quote keyword indicates symbols quoted with `quote' should be considered variable names." )

(defalias 'spaces--keyword-:dont-assume-function-quote 'car
  "The :dont-assume-function-quote keyword indicates symbols quoted with `function' should NOT be considered function names." )


;;; ---------------------------------------------------------------
;;; Interpreting the actual forms found in BODY of the main macro.
;;
;; This is where the heavy work is done.
;;
;; If you'd like to implement support for some special form, simply
;; define a function called `spaces--convert-FORM-NAME' along the
;; lines of the functions defined below. It will be automatically used
;; whenever that form is found.
(defvar spaces--name-already-prefixed nil
  "Whether some outer function already prefixed the name of the current defalias.")

(defun spaces--convert-defalias (form)
  "Special treatment for `defalias' FORM."
  (let ((dont-prefix spaces--name-already-prefixed))
    (setq spaces--name-already-prefixed nil)
    (list
     (car form)
     (if dont-prefix
         (cadr form)
       (let ((name (eval (cadr form)))) ;;ignore-errors
         (add-to-list 'spaces--fbound name)
         (list 'quote (spaces--prepend name))))
     (spaces-convert-form (cadr (cdr form))))))

;;; Defun, defmacro, and defsubst macros are pretty predictable. So we
;;; expand them and handle them like defaliases, instead of handling
;;; as general macros.
(defun spaces--convert-defmacro (form)
  "Special treatment for `defmacro' FORM."
  (let* ((spaces--name-already-prefixed t)
         (name (cadr form))
         (spaced-name (spaces--prepend name))
         decl)
    (add-to-list 'spaces--macro name)
    (add-to-list 'spaces--fbound name)
    ;; Set the macros debug spec if possible. It will be relevant on
    ;; the next run.
    (when (setq decl (ignore-errors (cond
                       ((eq (car-safe (nth 3 form)) 'declare)
                        (nth 3 form))
                       ((and (stringp (nth 3 form))
                             (eq (car-safe (nth 4 form)) 'declare))
                        (nth 4 form))
                       (t nil))))
      (setq decl (car (cdr-safe (assoc 'debug (cdr decl)))))
      (when decl (put spaced-name 'edebug-form-spec decl)))
    ;; Then convert the macro as a defalias.
    (spaces-convert-form
     (macroexpand
      (cons
       (car form)
       (cons spaced-name (cddr form)))))))
(defalias 'spaces--convert-defmacro* 'spaces--convert-defmacro)

(defun spaces--convert-defvar (form &optional dont-add)
  "Special treatment for `defvar' FORM."
  (let ((name (cadr form)))
    (unless dont-add
      (add-to-list 'spaces--bound name))
    (append
     (list
      (car form)
      (spaces--prepend name))
     (mapcar 'spaces-convert-form (cdr (cdr form))))))

(defalias 'spaces--convert-defconst 'spaces--convert-defvar
  "Special treatment for `defconst' FORM.")
(defalias 'spaces--convert-defcustom 'spaces--convert-defvar
  "Special treatment for `defcustom' FORM.")

(defun spaces--convert-defvaralias (form)
  "Special treatment for `defvaralias' FORM."
  (let ((name (eval (cadr form)))
        (name2 (eval (caddr form))))
    (add-to-list 'spaces--bound name)
    (append
     (list
      (car form)
      (list 'quote (spaces--prepend name))
      (list 'quote (spaces-convert-form name2)))
     (mapcar 'spaces-convert-form (cdr (cdr form))))))

(defun spaces--convert-custom-declare-variable (form)
  "Special treatment for `custom-declare-variable' FORM."
  (let ((name (eval (cadr form))) ;;ignore-errors
        (val (cl-caddr form)))
    (add-to-list 'spaces--bound name)
    (append
     (list
      (car form)
      (list 'quote (spaces--prepend name)) ;cadr
      ;; The DEFAULT argument is explicitly evaluated by
      ;; `custom-declare-variable', so it should be safe to namespace
      ;; even when quoted. Plus, we need to do this because
      ;; defcustom quotes this part.
      (if (spaces--quote-p (car-safe val))
          (list (car val) (spaces-convert-form (cadr val)))
        (spaces-convert-form val))
      (spaces-convert-form        (car (cdr (cdr (cdr form))))))
     (mapcar 'spaces-convert-form (cdr (cdr (cdr (cdr form))))))))

(defun spaces--convert-defface (form)
  "Special treatment for `defface' FORM.
Identical to defvar, just doesn't add the symbol to the boundp
list."
  (spaces--convert-defvar form :dont-add))

(defun spaces--convert-define-derived-mode (form)
  "Special treatment for `define-derived-mode' FORM."
  (let ((name (cadr form)))
    (add-to-list 'spaces--fbound name)
    (add-to-list 'spaces--bound name)
    (add-to-list 'spaces--bound
                 (intern (format "%s-map" name)))
    (add-to-list 'spaces--bound
                 (intern (format "%s-hook" name)))
    (spaces--macro-args-using-edebug
     (cons
      (car form)
      (cons (spaces--prepend name)
            (cddr form))))))

(defun spaces--convert-define-minor-mode (form)
  "Special treatment for `define-minor-mode' FORM."
  (let ((name (cadr form))
        (keymap (nth 5 form)))
    ;; Register the mode name
    (add-to-list 'spaces--fbound name)
    (add-to-list 'spaces--bound name)
    (add-to-list 'spaces--bound (intern (format "%s-hook" name)))
    ;; Register the keymap
    (if (null (symbolp keymap))
        (add-to-list 'spaces--bound (intern (format "%s-map" name)))
      (when (setq keymap (spaces--remove-namespace keymap))
        (add-to-list 'spaces--bound keymap)))
    ;; And here we namespace it.
    (spaces--macro-args-using-edebug
     (cons
      (car form)
      (cons (spaces--prepend name)
            (cddr form))))))

(defun spaces--convert-quote (form)
  "Special treatment for `quote' FORM.
When FORM is (quote argument), argument is parsed for namespacing
only if it is a lambda form.

Anything else (a symbol or a general list) is too arbitrary to
be logically namespaced and will be preserved as-is.

Note, however, that the value of the NAME argument of a
\"definition-type\" forms is ALWAYS namespaced, regardless of
whether the form was a quote."
  (let ((kadr (cadr form))
        func)
    (if (eq (car-safe kadr) 'lambda)
        (list (car form) (spaces-convert-form kadr))
      (if (symbolp kadr)
          (cond
           ;; A symbol inside a function quote should be a function,
           ;; unless the user disabled that.
           ((and (eq (car form) 'function)
                 (null (spaces--keyword :dont-assume-function-quote)))
            (list 'function
                  (or (spaces--remove-protection kadr)
                      (if (spaces--fboundp kadr)
                          (spaces--prepend kadr)
                        kadr))))
           
           ;; A symbol inside a regular quote should be a function, if
           ;; the user asked for that.
           ((and (eq (car form) 'quote)
                 (spaces--keyword :assume-var-quote))
            (list 'quote
                  (or (spaces--remove-protection kadr)
                      (if (spaces--boundp kadr)
                          (spaces--prepend kadr)
                        kadr))))

           (t form))
        form))))

(defalias 'spaces--convert-function 'spaces--convert-quote)

(defun spaces--convert-\` (form)
  "Special treatment for backtick FORM.
Currently, we just return FORM without namespacing anything."
  form)

(defun spaces--convert-lambda (form)
  "Special treatment for `lambda' FORM."
  (let ((spaces--local-vars
         (append (spaces--vars-from-arglist (cadr form))
                 spaces--local-vars))
        (forms (cdr (cdr form))))
    (append
     (list (car form)
           (cadr form))
     (when (stringp (car forms))
       (prog1
           (list (car forms))
         (setq forms (cdr forms))))
     (when (eq 'interactive (car-safe (car forms)))
       (prog1
           (list (list (car (car forms))
                       (spaces-convert-form (cadr (car forms)))))
         (setq forms (cdr forms))))
     (progn
       ;; (message "%S" forms)
       (mapcar 'spaces-convert-form forms)))))

(defun spaces--vars-from-arglist (args)
  "Get a list of local variables from a generalized arglist ARGS."
  (cl-remove-if
   (lambda (x) (string-match "^&" (symbol-name x)))
   (mapcar (lambda (x) (or (cdr-safe (car-safe x)) (car-safe x) x))
           args)))

(defun spaces--convert-defun (form)
  "Special treatment for `defun' FORM."
  (let* ((name (cadr form)))
    (add-to-list 'spaces--fbound name)
    (cons (car form)
          (spaces--convert-lambda
           (cons (spaces--prepend name) (cddr form))))))
(defalias 'spaces--convert-defun* 'spaces--convert-defun)
(defalias 'spaces--convert-defsubst 'spaces--convert-defun)
(defalias 'spaces--convert-defsubst* 'spaces--convert-defun)

(defun spaces--let-var-convert-then-add (sym add)
  "Try to convert SYM if :let-vars is in use.
If ADD is non-nil, add resulting symbol to `spaces--local-vars'."
  (let ((name (if (spaces--keyword :let-vars)
                  (spaces-convert-form sym)
                sym)))
    (when add (add-to-list 'spaces--local-vars name))
    name))

(defun spaces--convert-let (form &optional star)
  "Special treatment for `let' FORM.
If STAR is non-nil, parse as a `let*'."
  (let* ((spaces--local-vars spaces--local-vars)
         (vars
          (mapcar
           (lambda (x)
             (if (car-safe x)
                 (list (spaces--let-var-convert-then-add (car x) star)
                       (spaces-convert-form (cadr x)))
               (spaces--let-var-convert-then-add x star)))
           (cadr form))))
    ;; Each var defined in a regular `let' only becomes protected after
    ;; all others have been defined.
    (unless star
      (setq spaces--local-vars
            (append
             (mapcar (lambda (x) (or (car-safe x) x)) vars)
             spaces--local-vars)))
    (append
     (list (car form) vars)
     (mapcar 'spaces-convert-form (cddr form)))))

(defun spaces--convert-let* (form)
  "Special treatment for `let' FORM."
  (spaces--convert-let form t))

;; (defun spaces--convert-cond (form)
;;   "Special treatment for `cond' FORM."
;;   (cons
;;    (car form)
;;    (mapcar
;;     (lambda (x)
;;       (cons (spaces-convert-form (car x))
;;             (mapcar 'spaces-convert-form (cdr x))))
;;     (cdr form))))

(defun spaces--convert-condition-case (form)
  "Special treatment for `condition-case' FORM."
  (append
   (list
    (car form)
    (cadr form)
    (spaces-convert-form (cadr (cdr form))))
   (mapcar
    (lambda (x)
      (cons (car x)
            (mapcar 'spaces-convert-form (cdr x))))
    (cddr (cdr form)))))

(provide 'spaces)

;;; spaces.el ends here
