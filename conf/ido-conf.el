;; +-------------------------------------------------------+
;; |                                                       |
;; |                   ido-everywhere                      |
;; |                                                       |
;; +-------------------------------------------------------+
(ido-mode t)
(ido-ubiquitous-mode t)
(smex-initialize)

;; +----------------------------------------------------------------+
;; |              i-menu-mode, with ido support                     |
;; | http://www.emacswiki.org/emacs/ImenuMode#toc10                 |
;; +----------------------------------------------------------------+
(autoload 'idomenu "idomenu.el" t)
;; http://www.masteringemacs.org/articles/2011/01/14/effective-editing-movement/
(global-set-key (kbd "M-i") 'idomenu)
