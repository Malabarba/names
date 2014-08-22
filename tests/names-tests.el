(require 'ert)

(defmacro names-deftest (name doc &rest body)
  "Test if (namespace NAME FORMS-A) is the same as FORM-B."
  (declare (indent defun)
           (debug (&rest sexp)))
  (let* ((last-a (last body 2))
         (form-b (cadr last-a)))
    (setcdr last-a nil)
    `(ert-deftest ,name () ,doc
       (should (equal
                (macroexpand-all '(define-namespace ,@body))
                (macroexpand-all '(progn ,form-b)))))))

(names-deftest names-defun-rename
  "Test that `defun' functions are renamed."
  test-
  (defun foo () 1)
  (defun test-foo () 1))

(names-deftest names-external-unchanged
  "Test that external function calls are not rewritten."
  test-
  (defun foo () (message "hello world!"))
  (defun test-foo () (message "hello world!")))

(names-deftest names-reference-other-internal
  "Test that one function within a namespace can call another with qualifying the name."
  test-
  (progn (defun bar () (foo))
         (defun foo () (message "hello world!")))
  (progn (defun test-bar () (test-foo))
         (defun test-foo () (message "hello world!"))))

