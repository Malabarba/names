(require 'names)

(defmacro names-deftest (name doc &rest body)
  "Test if (namespace NAME FORMS-A) is the same as FORM-B."
  (declare (indent defun)
           (debug (&rest sexp)))
  `(ert-deftest 
       ,(intern (format "names-%s" name)) () ,doc
       ,@(let (out)
           (while body 
             (push `(should (equal
                             (macroexpand-all '(define-namespace a- ,@(pop body)))
                             (macroexpand-all '(progn ,@(pop body)))))
                   out))
           out)))

(names-deftest rename-defuns
  "Test that definitions are namespaced."
  ((defun foo0 () 1))
  ((defun a-foo0 () 1))
  ((defmacro foo2 () 1))
  ((defmacro a-foo2 () 1))
  ((defalias 'foo4 'something-else))
  ((defalias 'foo4 'something-else)))

(names-deftest rename-defvars
  "Test that definitions are namespaced."
  ((defvar foo1 1))
  ((defvar a-foo1 1))
  ((defconst foo3 1))
  ((defconst a-foo3 1))
  ((defcustom foo4 1 "doc"))
  ((defcustom a-foo4 1 "doc"))
  ((defvaralias 'foo4 'something-else))
  ((defvaralias 'foo4 'something-else)))

(names-deftest defun-mass-rename
  "Test that definitions are namespaced."
  ((defun foo0 () 1)
   (defvar foo1 1)
   (defmacro foo2 () 1)
   (defconst foo3 1)
   (defcustom foo4 1 "doc")
   (defvaralias 'foo4 'something-else)
   (defalias 'foo0 'something-else)
   (defalias #'foo0 #'something-else))
  ((defun a-foo0 () 1)
   (defvar a-foo1 1)
   (defmacro a-foo2 () 1)
   (defconst a-foo3 1)
   (defcustom a-foo4 1 "doc")
   (defvaralias 'foo4 'something-else)
   (defalias 'foo0 'something-else)
   (defalias #'a-foo0 #'something-else)))

(names-deftest external-unchanged
  "Test that external function calls are not rewritten."
  ((defun foo () (message "hello world!"))) 
  ((defun a-foo () (message "hello world!"))))

(names-deftest reference-other-internal
  "Test that one function within a namespace can call another with qualifying the name."
  ((defun bar () (foo))
   (defun foo () (message "hello world!"))) 
  ((defun a-bar () (a-foo))
   (defun a-foo () (message "hello world!"))))

(names-deftest function-form
  "Test #' behaviour."
  ;; Undefined
  ((defalias #'foo0 #'foo1))
  ((defalias #'foo0 #'foo1))
  ;; Defined
  ((defun foo0 () 1)
   (defun foo1 () 1)
   (defalias #'foo0 #'foo1))
  ((defun a-foo0 () 1)
   (defun a-foo1 () 1)
   (defalias #'a-foo0 #'a-foo1))
  ;; And the keyword
  (:dont-assume-function-quote
   (defun foo0 () 1)
   (defun foo1 () 1)
   (defalias #'foo0 #'foo1))
  ((defun a-foo0 () 1)
   (defun a-foo1 () 1)
   (defalias #'foo0 #'foo1)))

(names-deftest quote-form
  "Test ' behaviour."
  ;; Undefined
  ((defvaralias 'foo0 'foo1))
  ((defvaralias 'foo0 'foo1))
  ;; Defined
  ((defvar foo0 1)
   (defvar foo1 1)
   (defvaralias 'foo0 'foo1)
   (defvaralias #'foo2 #'foo3))
  ((defvar a-foo0 1)
   (defvar a-foo1 1)
   (defvaralias 'foo0 'foo1)
   (defvaralias #'foo2 #'foo3))
  ;; And the keyword
  (:assume-var-quote
   (defun foo0 () 1)
   (defcustom foo1 1 "")
   (defvaralias 'foo0 'foo1)
   (defvaralias 'a-foo2 #'foo1))
  ((defun a-foo0 () 1)
   (defcustom a-foo1 1 "")
   (defvaralias 'foo0 'a-foo1)
   (defvaralias 'a-foo2 #'foo1)))

(names-deftest let-vars
  "Test letbound variables."
  ;; Neither a-c nor a-b exist
  (:let-vars 
   (defun foo () (let ((c b)) c)))
  ((defun a-foo () (let ((c b)) c)))
  ;; Both a-c and a-b exist
  (:let-vars
   (defvar c nil "")
   (defvar b nil "")
   (defun foo () (let ((c b)) c)))
  ((defvar a-c nil "")
   (defvar a-b nil "")
   (defun a-foo () (let ((a-c a-b)) a-c)))
  ;; Both a-c and a-b exist, but no keyword.
  ((defvar c nil "")
   (defvar b nil "")
   (defun foo () (let ((c b)) c)))
  ((defvar a-c nil "")
   (defvar a-b nil "")
   (defun a-foo () (let ((c a-b)) c))))

(names-deftest backtick
  "Test \` form."
  ;; Unfortunately, our edebug hack adds a progn around the form.
  ((defvar c nil "")
   (defun b nil "")
   (defun foo () `(a b c ,@(a (b) c b (c)) ,(b) ,b ,(c) ,c)))
  ((defvar a-c nil "")
   (defun a-b nil "")
   (defun a-foo () (progn `(a b c ,@(a (a-b) a-c b (c)) ,(a-b) ,b ,(c) ,a-c)))))

(defun a-baz () "")
(defvar a-bio nil "")

(names-deftest global
  "Test :global keyword."
  (:global
   (defun foo () (let ((c bio)) (baz))))
  ((defun a-foo () (let ((c a-bio)) (a-baz)))))

(unless (fboundp 'string-prefix-p)
  (defun stringp-prefix-p (x y)
    ""
    (string-match (concat "\\`" x) y)))

(add-to-list 'load-path (expand-file-name "./elnode/"))

(defun deep-search-debug (x)
  "Look for symbols starting with \"edebug-\"."
  (when x
    (cond
     ((consp x)
      (or (deep-search-debug (car x))
          (deep-search-debug (cdr x)))
      ;; (apply 'append
      ;;        (mapcar #'deep-search-debug x))
      )
     ((symbolp x)
      (when (string-prefix-p "edebug-" (symbol-name x))
        x))
     (t nil))))

(defun names--find-edebug-traces (sym)
  ""
  (let* ((fun (indirect-function sym))
         (symbol-vec))
    (when (macrop fun)
      (setq fun (cdr fun)))
    (condition-case er
        (cond
         ((subrp fun) nil)
         ((listp fun)
          (deep-search-debug fun))
         ((vectorp fun)
          (setq symbol-vec
                (aref (or (cdr-safe fun) fun) 2))
          (append
           (remove-if
            (lambda (x) (or (null (symbolp x))
                            (null (string-prefix-p "edebug-" (symbol-name sym)))))
            symbol-vec)
           nil)))
      (error
       (error "Symbol: %s\nFunction: %s\nError: %s"
              sym fun er)))))

(ert-deftest no-leftover-edebug ()
  "Test no edebug leftover in function definitions."
  (dolist (lib '((dash . "-")
                 (s . "s-")
                 (elnode . "elnode/")))
    (require (car lib))
    (should
     (equal (loop for x being the symbols
                  if (fboundp x)
                  if (string-prefix-p (cdr lib) (symbol-name x))
                  if (names--find-edebug-traces x)
                  collect x)
            nil))))

(defmacro !cdr (list)
  "Destructive: Set LIST to the cdr of LIST."
  `(setq ,list (cdr ,list)))

(ert-deftest macro-expansion ()
  "Test macros work."
  (byte-compile-file (expand-file-name "./macro-test-aux.el"))
  (require 'macro-test-aux)
  (should
   (equal (test-message) '("4" "3" "2" "1"))))
