;;; Nash-conf

(require 'nash-mode)

;; to use nash-mode for sh scripts (instead of sh-mode)
(add-to-list 'auto-mode-alist (cons "\\.sh" 'nash-mode))

(defun my-nash-mode-hook ()
  (setq nashfmt-command "nashfmt")

  ;;; Call Nashfmt before saving
  (add-hook 'before-save-hook 'nashfmt-before-save))

(add-hook 'nash-mode-hook 'my-nash-mode-hook)
