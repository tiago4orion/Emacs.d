;;; Nash-conf

(require 'nash-mode)

;; to use nash-mode for sh scripts (instead of sh-mode)
;(add-to-list 'auto-mode-alist (cons "\\.sh" 'nash-mode))

(add-hook 'nash-mode-hook 'nash-fmt-enable-on-save)
