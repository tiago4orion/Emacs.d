;;; go-conf.el ---                                   -*- lexical-binding: t; -*-

;;; Go emacs config


(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(setq-local *go-version* "go1.5.3")
(setq-local *gopath* (expand-file-name (format "~/.gvm/pkgsets/%s" *go-version*)))
(setq-local *goroot* (expand-file-name (format "~/.gvm/gos/%s" *go-version*)))

(setenv "GOPATH" *gopath*)
(setenv "GOROOT" *goroot*)

(setenv "PATH" (format "%s/bin:/usr/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:%s/bin" *gopath* *goroot*))

(setq exec-path (cons *goroot* exec-path))
(setq exec-path (cons (format "%s/bin" *gopath*) exec-path))
(add-to-list 'exec-path (expand-file-name (format "%s/global/src/github.com/nsf/gocode/bin" *gopath*)))

(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c C-i") 'gofmt-before-save))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

;;; go-conf.el ends here
