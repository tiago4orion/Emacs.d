(load "dockerfile-mode.el")

(require 'dockerfile-mode)

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
