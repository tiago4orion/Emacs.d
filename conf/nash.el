;;; Configure nash-mode

(require 'nash-mode)

(add-to-list 'auto-mode-alist '("\\.sh\\'" . nash-mode))

(defun my-nash-mode-hook ()
  (setq nashfmt-command "nashfmt")
  ; Call nashfmt before saving
  (add-hook 'before-save-hook 'nashfmt-before-save))

(add-hook 'nash-mode-hook 'my-nash-mode-hook)
