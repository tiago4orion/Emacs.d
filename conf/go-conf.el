;;; go-conf.el ---                                   -*- lexical-binding: t; -*-

;;; Go emacs config


(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(setenv "PATH" "/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl")
(setenv "GOPATH" (expand-file-name "~/go-workspace"))

(setq exec-path (cons (expand-file-name "~/projects/personal/go") exec-path))
(add-to-list 'exec-path (expand-file-name "~/projects/go-workspace/src/github.com/nsf/gocode/bin"))

(defun auto-complete-for-go ()
  )
(add-hook 'go-mode-hook 'auto-complete-for-go)

(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  (auto-complete-mode 1)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

;;; go-conf.el ends here
