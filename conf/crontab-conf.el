;;; crontab-conf.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author:  <i4k@stay-away>
;; Keywords:

(load "crontab-mode.el")

(add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))
