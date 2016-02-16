;; Kernel dot emacs
;; Based on the prior elisp from Documentantion/CodingStyle
;;
;; January, 2016
;; Geyslan G. Bem <geyslan@gmail.com>

;; This elisp does use of emacs functionalities which deliver to the
;; user indentation, comments and white space highlighting.
;;
;; As known tabs are the higher law and the prior elisp code enforces
;; that law for any lineup indentation.
;;
;; However some trees have specific rules about line continuation
;; indentation. Even scripts/checkpatch.pl suggests the TABS+SPACES (lining up
;; under the open paren) for lineup the sequential lines.
;;
;; In addition to allowing the automatic setup by tree, this elisp can easily
;; be modified for new configurations. Eg.
;;
;;         (when (or (string-match (concat source-path "/net") filename)
;;                   (string-match (concat source-path "/drivers/net") filename))
;;           (setup-kernel-style 'extra-bottom-line t nil))
;;
;; The above model can be used for a new tree just changing the tree path
;; and parameters of setup-kernel-style function.
;;
;; setup-kernel-style function sets the kernel-comment-style,
;; kernel-lineup-tabs-only and kernel-lineup-maximum-tabs variables. They
;; are used by the functions kernel-comment-dwim and kernel-lineup-arglist.
;;
;; The kernel-lineup-arglist function detects the maximum tabs allowed to
;; lineup and if it must use tabs and spaces instead of only tabs.
;;
;; There are two cleanups for else and else if braces enabled when
;; auto-newline is on. These are comfortable and avoid wrong coding
;; style. For instance
;;
;; void spam(int i)
;; {
;;         if (i == 7) {
;;                 dosomething();
;;             ...
;;         }
;;         else
;;                         {
;; appears like this after the last open brace is typed:
;;
;; void spam(int i)
;; {
;;         if (i == 7) {
;;                 dosomething();
;;             ...
;;         } else {
;;
;; The same happens for else if opening braces.
;;
;; The function kernel-align-to-equals makes the aligning of variable
;; initializations/assignments easier.
;;
;;         int x = 10;
;;         char *str = "text";
;;         x = 100;
;;
;; After marked the region and pressed C-c a =
;;
;;         int x           = 10;
;;         char *str       = "text";
;;         x               = 100;
;;
;; Concerning to comments kernel-comment-dwim is an improved version of
;; comment-dwim. It comment/uncomment lines, regions or adds a new
;; indented comment after the code using the same key binding. Eg.
;;
;; void f(int x, int y);
;;
;; Press M-; in any position on the line for
;;
;; void f(int x, int y);   /*  */
;;
;; Press M-; again for
;;
;; /* void f(int x, int y); */
;;
;; Press M-; and again for
;;
;; void f(int x, int y);
;;
;; For multi-line comments the result is
;;
;; /*
;;  * void f1(int x, int y);
;;  * void f2(int x, int y);
;;  */
;;
;; Emacs doesn't provide yet a 'net' comment style by default. This code
;; also does the magic when kernel-comment-style is set as 'extra-bottom-line.
;;
;; /* void f1(int x, int y);
;;  * void f2(int x, int y);
;;  */
;;
;; Until now the comment-region doesn't tabify the comment, generating
;; spaces before the " * " comment-continue variable (this issue was
;; reported and patched in the to be released emacs-25) however this code
;; correctly tabify the commented result.
;;
;; kernel-comment-dwim also removes trailing white spaces created by the
;; comment-region.
;;
;; Finally the white space highlighting is a must to alert about long
;; lines, leading or trailing spaces and top or bottom empty lines.


(defconst kernel-column-limit 80
  "It is only 80, get over it.")

(defvar kernel-source-path "~/src/linux-trees"
  "The kernel source path.")

(defvar kernel-comment-style 'extra-lines
  "Default style is 'extra-lines.
The another option is 'extra-bottom-line")
(make-variable-buffer-local 'kernel-comment-style)

(defvar kernel-lineup-tabs-only nil
  "If it is non-nil the kernel lineup indentation will make use of tabs only.
When nil lineup indentation will use TABS + SPACES.")
(make-variable-buffer-local 'kernel-lineup-tabs-only)

(defvar kernel-lineup-maximum-tabs nil
  "If it is non-nil its value will be the maximum tabs steps in kernel lineup
indentation.
When nil there will not be such a limit.
In both cases there is also the maximum limit forced by the
`kernel-lineup-arglist' function in conjuction with the
`kernel-column-limit' constant.")

(defun setup-kernel-style (comment-style lineup-tabs-only lineup-maximum-tabs)
  (setq kernel-comment-style comment-style)
  (setq kernel-lineup-tabs-only lineup-tabs-only)
  (setq kernel-lineup-maximum-tabs lineup-maximum-tabs))

(defun kernel-comment-dwim ()
  "Comments or uncomments the region.
If there's no active region adds/indents an one line comment or
comments/uncomments the current line if the one line comment is
empty."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (progn
          (setq beg (region-beginning)
                end (region-end))
          (if (comment-only-p beg end)
              (uncomment-region beg end)
            (progn
              (comment-region beg end)
              (save-excursion
                (goto-char beg)
                ;; Remove extra top line
                (when (equal kernel-comment-style 'extra-bottom-line)
                  (re-search-forward "/\\*\\s-*\n\\s-*\\(\\*\\)" end t)
                  (replace-match "/\\1"))
                ;; Update end point
                (goto-char beg)
                (re-search-forward "\\*/" nil t)
                (setq end (point))
                ;; This error is fixed in version 25.
                (when (< emacs-major-version 25)
                  (tabify beg end))
                ;; Cleaning only trailing spaces inserted by comment-region.
                ;; Existing ones are not touched.
                (goto-char beg)
                (while (re-search-forward
                        "\\(/+\\|^\\s-+\\)\\(\\*\\)\\(\\s-+$\\)" end t nil)
                  (replace-match "\\1\\2")
                  (save-excursion
                    (re-search-forward "\\*/" nil t 1)
                    (setq end (point))))))))
      (progn
        (setq beg (line-beginning-position)
              end (line-end-position))
        (if (save-excursion
              (goto-char beg)
              (looking-at "\\s-*$"))
            (progn
              (comment-indent)
              (indent-according-to-mode))
          (if (comment-only-p beg end)
              (uncomment-region beg end)
            (if (save-excursion
                  (goto-char beg)
                  (re-search-forward "/\\*\\s-+\\*/" end t 1))
                (progn
                  (save-excursion
                    (goto-char beg)
                    (let (kill-ring)
                      (comment-kill nil)))
                  ;; Read end position directly
                  (comment-region beg (line-end-position)))
              (comment-indent))))))))

(defun kernel-align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 nil))

(defun kernel-lineup-arglist (langelem)
  ""
  (let* ((ret (c-lineup-arglist langelem))
         (anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (newcol (c-langelem-col langelem t))
         (steps (floor offset c-basic-offset)))
    (if (not kernel-lineup-tabs-only)
        ret
      (progn
        (when (>= (+ newcol (* c-basic-offset steps))
                  kernel-column-limit)
          (setq steps (1- steps)))
        (when kernel-lineup-maximum-tabs
          (setq steps (min steps
                           kernel-lineup-maximum-tabs)))
        (* (max steps 1) c-basic-offset)))))

(defun kernel-style-hook ()
  (let ((filename (buffer-file-name))
        (source-path (expand-file-name kernel-source-path)))
    ;; Enable kernel mode for the appropriate files
    (when (and filename
               (string-match source-path filename))
      ;; Setup style
      (c-set-style "linux-kernel")
      (setq tab-width 8
            comment-style 'extra-line
            indent-tabs-mode t
            backward-delete-char-untabify-method nil)
      (c-toggle-auto-newline t)

      ;; Setup tree paths here
      (when (or (string-match (concat source-path "/net") filename)
                (string-match (concat source-path "/drivers/net") filename))
        (setup-kernel-style 'extra-bottom-line t nil))
      (when (string-match (concat source-path "/drivers/usb/host") filename)
        (setup-kernel-style 'extra-lines t 2))

      ;; Set kernel style key bindings
      (local-set-key [remap comment-dwim] 'kernel-comment-dwim)
      (local-set-key (kbd "C-c a =") 'kernel-align-to-equals)
      ;; Setup white space highlighting
      (require 'whitespace)
      (setq whitespace-line-column kernel-column-limit
            whitespace-style '(face empty
                                    indentation::tab
                                    whitespace-space-before-tab
                                    space-before-tab::tab
                                    lines-tail
                                    trailing))
      (dolist (face '(whitespace-line
                      whitespace-indentation
                      whitespace-space
                      whitespace-space-before-tab
                      whitespace-empty
                      whitespace-trailing))
        (set-face-background face "red"))
      (set-face-attribute whitespace-line nil
                          :background "red"
                          :foreground "yellow"
                          :weight 'bold)
      (whitespace-mode t))))

(add-hook 'c-mode-hook 'kernel-style-hook)
(make-variable-buffer-local 'kernel-lineup-maximum-tabs)

(add-hook 'c-mode-common-hook
         (lambda ()
           (c-add-style
            "linux-kernel"
            '("linux" (c-offsets-alist
                       (arglist-cont-nonempty
                        c-lineup-gcc-asm-reg
                        kernel-lineup-arglist))
              (c-cleanup-list brace-else-brace
                              brace-elseif-brace)))))
