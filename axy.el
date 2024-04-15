;;; axy.el --- Adhoc expansion of yasnippets

;; Copyright (c) 2024 <naikvin@gmail.com>

;; Author: Vineet Naik <naikvin@gmail.com>
;; URL: ??
;; Version: 0.1.0
;; Keywords: Yasnippet
;; Package-Requires: ((dash "20240103.1301"))

;; This program is *not* a part of emacs and is provided under the MIT
;; License (MIT) <http://opensource.org/licenses/MIT>
;;
;; Copyright (c) 2024 Vineet Naik <naikvin@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; This file attempts to provide the following workflow:
;;
;;  1. quickly find a yasnippet
;;
;;  2. expand it in a temp buffer and allow user to make changes (like
;;  a regular yasnippet)
;;
;;  3. copy the output to clipboard and close the temp buffer
;;
;; Motivation: For a long time I have been using Dash snippets +
;; Alfred for snippet expansion. This works great but it's a mac
;; specific workflow, besides requiring paid versions of both dash and
;; alfred. This package was implemented as a POC to check if yasnippet
;; could be used to achieve a similar workflow.

;;; Code:

(require 'dash)

(defvar axy/all-tables nil)

(defvar axy/tmp-snippet-buffer nil)

;; Inspired by the `yas--subdirs` function in yasnippet.el
(defun axy/table-dirs (snippets-dir)
  (-remove (lambda (d)
             (or (string= (substring d 0 1) ".")
                 (string-match "\\`#.*#\\'" (file-name-nondirectory d))
                 (string-match "~\\'" (file-name-nondirectory d))))
           (condition-case nil
               (directory-files snippets-dir)
             (error nil))))

(defun axy/discover-table-names ()
  ;; To cover all dirs where snippets could be found,
  ;; `yas--default-user-snippets-dir` needs to be included as well
  (let ((dirs (cons yas--default-user-snippets-dir (yas-snippet-dirs))))
    (or axy/all-tables
        (let ((tables (-distinct (-mapcat 'axy/table-dirs dirs))))
          (setq axy/all-tables tables)
          tables))))

(defun axy/prompt-for-mode ()
  (intern (completing-read "Select a major mode: " (axy/discover-table-names) nil t)))

(defun axy/find-&-expand-snippet ()
  (interactive)
  (let ((selected-mode (axy/prompt-for-mode)))
    ;; Yasnippets are JIT loaded which means snippets for the selected
    ;; mode will not be loaded unless the mode itself has been
    ;; activated at least once. As a workaround for this, we create a
    ;; tmp buffer, activate the mode in it and immediately close
    ;; it. This avoids having to rely on any more interal yasnippet
    ;; functions OR duplication of code.
    (let ((tmp-buffer (get-buffer-create "*axy-tmp*")))
      (switch-to-buffer tmp-buffer)
      (yas-activate-extra-mode selected-mode)
      (kill-buffer tmp-buffer))
    (let* ((tables (yas--get-snippet-tables selected-mode))
           (template (yas--prompt-for-template (yas--all-templates tables)))
           (snippet-file (yas--template-load-file template))
           ;; Create the snippet buffer only if doesn't exist already
           (snippet-buffer (if-let ((buf (find-buffer-visiting snippet-file)))
                               (progn
                                 (switch-to-buffer buf)
                                 buf)
                             (progn
                               (yas--visit-snippet-file-1 template)
                               (setq buffer-read-only t)
                               ;; Populate the tmp-snippet-buffer val
                               ;; to indicate that the snippet buffer
                               ;; was specially created so that it
                               ;; could be closed by
                               ;; `axy/clipboard-copy-&-exit` fn
                               (setq axy/tmp-snippet-buffer (current-buffer))
                               (current-buffer)))))
      (yas-tryout-snippet)
      (axy-mode 1))))

(defun axy/clipboard-copy-&-exit ()
  (interactive)
  ;; Save the entire buffer to clipboard
  (clipboard-kill-ring-save (point-min) (point-max))
  ;; Kill the buffer created by `yas-tryout-snippet` fn
  (kill-buffer (current-buffer))
  ;; Kill the snippet buffer if it was specially created by the call
  ;; to `axy/find-&-expand-snippet` fn
  (when axy/tmp-snippet-buffer
    (kill-buffer axy/tmp-snippet-buffer)
    (setq axy/tmp-snippet-buffer nil)))

;; Define a simple minor mode so that we can map key-bindings for the
;; temporary snippet buffer that `yas-tryout-snippet` function will
;; create. Ref: https://emacs.stackexchange.com/a/524

(defvar axy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-;") 'axy/clipboard-copy-&-exit)
    map))

;;;###autoload
(define-minor-mode axy-mode
  "Minor mode that will be enabled in the buffer created by `'yas-tryout-snippet`'"
  :init-value nil
  :lighter " axy"
  :keymap axy-mode-map)

(provide 'axy)
