(progn
  (add-to-list 'load-path (expand-file-name "../"))
  (add-to-list 'load-path (expand-file-name "./"))
  (require 'ert)
  (setq debug-on-error t)
  (setq byte-compile-debug t))
