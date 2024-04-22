;;; axy.el --- Adhoc expansion of yasnippets

;; Copyright (c) 2024 Vineet Naik <naikvin@gmail.com>
;; Author: Vineet Naik <naikvin@gmail.com>
;; URL: https://github.com/naiquevin/axy
;; Version: 0.1.0
;; Keywords: Yasnippet
;; Package-Requires: ((yasnippet "20200604.246"))

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
;; Axy is short for *A*d hoc e*X*pansion of *Y*asnippets!
;;
;; This file attempts to provide the following workflow:
;;
;;  1. quickly find a yasnippet from anywhere (regardless of the major
;;  mode in the current buffer)
;;
;;  2. expand it in a temp buffer, allowing user to make changes (like
;;  a regular yasnippet)
;;
;;  3. copy the output to clipboard and close the temp buffer
;;
;; Installation:
;;
;; The package is not yet uploaded on any of the emacs package
;; repositories. For now, you can clone the repo in an accessible
;; location and somehow load this file. Then just set a global
;; keybinding to invoke the `axy/find-&-expand-snippet` fn.
;;
;; If you use `use-package`, you may add the following lines to your
;; emacs config:
;;
;;     (use-package axy
;;       :ensure nil
;;       :requires yasnippet
;;       :after (yasnippet)
;;       :load-path /location/to/axl/repo
;;       :config
;;       (global-set-key (kbd "C-c C-;") 'axy/find-&-expand-snippet))
;;
;; Usage:
;;
;; Assuming that the global keybinding mentioned above is set, you can
;; type `C-c ;` from anywhere to invoke the
;; `axy/find-&-expand-snippet` fn.
;;
;; It will first prompt for a major mode. Upon selecting the major
;; mode, it will prompt you to select the snippet from a list of
;; snippets eligible for that mode. On selecting a snippet, a new
;; temporary buffer will open, initiating the expansion of the
;; selected snippet. Once all the placeholders are entered, you can
;; use the `axy-mode` specific keybinding `C-;`, to copy the expanded
;; snippet to clipboard and also cleanup the temporary buffer.
;;
;; The code also provides a minor mode named `axy-mode` which is only
;; meant for the temporary buffer mentioned above. There's no need to
;; manually enable it in any other buffers.
;;
;; Known issues:
;;
;; The code depends on some of the internal functions from the
;; yasnippet package. So if the API of these fns changes in future
;; releases, it could break `axy`.

;;; Code:

(require 'cl-lib)

(defvar axy/all-tables nil)

(defvar axy/tmp-snippet-buffer nil)

;; Inspired by the `yas--subdirs` function in yasnippet.el
(defun axy/table-dirs (snippets-dir)
  (cl-remove-if (lambda (d)
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
        (let ((tables (cl-remove-duplicates (cl-mapcan 'axy/table-dirs dirs) :test 'string=)))
          (setq axy/all-tables tables)
          tables))))

(defun axy/prompt-for-mode ()
  (intern (completing-read "Select a major mode: " (axy/discover-table-names) nil t)))


(defun axy/get-snippet-tables (mode)
  "Get snippet tables for the given mode.

This function is a wrapper over `yas--get-snippet-tables` and
ensures that the snippet table for the mode is actually
loaded. This needs to be done because yasnippets are JIT loaded,
which means snippets for the given mode will not be loaded unless
the mode itself has been activated at least once."
  (let* ((tables (yas--get-snippet-tables mode))
         (is-mode-loaded (cl-some (lambda (table)
                                    (string= (yas--table-name table)
                                             (symbol-name mode)))
                                  (yas--get-snippet-tables 'sh-mode))))
    (if is-mode-loaded
        tables
      ;; Following is the workaround to ensure that snippet table for
      ;; the mode is loaded. We create a tmp buffer, activate the mode
      ;; in it and immediately close it. This avoids having to rely on
      ;; any more interal yasnippet functions OR duplication of code.
      (progn
        (with-temp-buffer
          (yas-activate-extra-mode mode))
        (yas--get-snippet-tables mode)))))


(defun axy/find-&-expand-snippet ()
  (interactive)
  (let* ((selected-mode (axy/prompt-for-mode))
         (tables (axy/get-snippet-tables selected-mode))
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
    (axy-mode 1)))


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
