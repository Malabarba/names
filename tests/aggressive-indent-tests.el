(byte-compile-file "aggressive-indent.el")
(require 'package)
(package-initialize)
(setq package-user-dir (expand-file-name "./elpa"))
(when (version< emacs-version "24.3")
  (package-install-file (expand-file-name "cl-lib.el")))

(package-install-file (expand-file-name "../names.el"))
(setq byte-compile-error-on-warn t)
(package-install-file (expand-file-name "aggressive-indent.el"))

(defun file-as-list (file)
  (let (out it)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (while (setq it (ignore-errors (read (current-buffer))))
        (push it out)))))

(ert-deftest compare-autoloads ()
  (let ((should-have (file-as-list "elpa/aggressive-indent-autoloads.el"))
        (do-have (file-as-list "elpa/aggressive-indent-0.1/aggressive-indent-autoloads.el")))
    (should (string= do-have should-have))))



