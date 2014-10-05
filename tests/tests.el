(progn
  (byte-compile-file (expand-file-name "../names.el"))
  (add-to-list 'load-path (expand-file-name "../"))
  (add-to-list 'load-path (expand-file-name "./"))
  (require 'ert)
  ;; (fset 'ert--print-backtrace 'ignore)
  ;; (setq debug-on-error t)
  ;; (setq byte-compile-debug t)
  )
