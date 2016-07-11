;;; sml-mode.el ---                                  -*- lexical-binding: t; -*-

(require 'sml-mode)
(require 'sml-mlton)

(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)

(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))
