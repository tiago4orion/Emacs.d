;;; ob-go.el --- org-babel functions for template evaluation

;; Copyright (C) your name here

;; Author: your name here
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; TO BE DOCUMENTED

;;; Requirements:

;; Use this section to list the requirements of this language.  Most
;; languages will require that at least the language be installed on
;; the user's system, and the Emacs major mode relevant to the
;; language be installed as well.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(add-to-list 'org-babel-tangle-lang-exts '("go" . "go"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:template '())

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:go' function below.
(defun org-babel-expand-body:go (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  body)

(defun org-babel-execute:go (body params)
  "Execute a block of Go code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing Go source code block")
  (let* ((tmp-src-file (org-babel-temp-file "go-src-" ".go"))
         (processed-params (org-babel-process-params params))
         (vars (second processed-params))
         (result-params (third processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (fourth processed-params))
         (full-body (org-babel-expand-body:go
                     body params processed-params))
         (coding-system-for-read 'utf-8) ;; use utf-8 with subprocesses
         (coding-system-for-write 'utf-8))
    (progn
      (with-temp-file tmp-src-file (insert full-body))
      (let ((results
             (org-babel-trim
              (org-babel-eval
               (format "go run %s" (org-babel-process-file-name tmp-src-file))
               ""))))
        (org-babel-reassemble-table
         (org-babel-result-cond (cdr (assoc :result-params params))
           (org-babel-read results)
           (let ((tmp-file (org-babel-temp-file "go-")))
             (with-temp-file tmp-file (insert results))
             (org-babel-import-elisp-from-file tmp-file)))
         (org-babel-pick-name
          (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
         (org-babel-pick-name
          (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))
    ))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:template (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-template-var-to-template (var)
  "Convert an elisp var into a string of template source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-template-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-template-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (unless (string= session "none")
    ))

(provide 'ob-go)
;;; ob-go.el ends here
