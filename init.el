;; init file tips:
;; http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
;; http://sites.google.com/site/steveyegge2/effective-emacs

;; common lisp compatibility
(require 'cl)

(defvar *emacs-load-start* (current-time))

(defvar *my-default-lib* "~/.emacs.d/lib"
  "Vendor libraries that cannot be installed via the package system")
(defvar *my-conf* "~/.emacs.d/conf/"
  "My configurations")
(defvar *elpa-path* "~/.emacs.d/elpa/"
  "Elpa packages")
(defvar *org-mode-path* "~/.emacs.d/lib/org-mode/lisp"
  "Org Mode Git path")

(add-to-list 'load-path *my-default-lib*)
(add-to-list 'load-path *my-conf*)
(add-to-list 'load-path *org-mode-path*)

(load "backup.el")
(load "package-manager.el")
(load "elisp.el")
(load "org-conf.el")
(load "misc.el")
(load "keybindings.el")
(load "ido-conf.el")
(load "autocompletion.el")
(load "eshell-conf.el")
(load "spell-checking.el")
(load "lisp-conf.el")
(load "scheme-conf.el")
(load "sql-conf.el")
(load "docker-conf.el")
(load "go-autocomplete.el")
(load "go-conf.el")
(load "sml-conf.el")
(load "crontab-mode.el")
(load "nash-conf.el")

(setq custom-file (concat *my-conf* "custom.el"))
(load custom-file 'noerror)

;; find out the time your emacs took to load.
(message "My .emacs loaded in %ds"
         (destructuring-bind (hi lo ms _) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defalias 'calendar-absolute-from-iso 'calendar-iso-to-absolute)
