;;; elispfl.el --- Extra font locks made your Elisp mode fancy  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Zhu Zihao

;; Author: Zhu Zihao <all_but_last@163.com>
;; URL: https://github.com/cireu/elispfl
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: lisp

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run `elispfl-mode', made your emacs-lisp mode much more fancy!

;;; Code:

(require 'advice)
(require 'font-lock)

(defvar elispfl-face nil
  "A variable to hold current face used to render.")

(defun elispfl--real-function (sym)
  "Unwinding function definition chain of SYM and return real definition.

Sign: (-> Sym Fn)

All aliases and advices will be removed."
  (let ((fn (indirect-function sym)))
    (while (let* ((unadvised (ad-get-orig-definition fn))
                  (unaliased (indirect-function unadvised)))
             (setq fn unaliased)
             (not (eq unaliased unadvised))))
    fn))

(defun elispfl--get-face (sym &optional subr-call?)
  "Get appropriate face for SYM.

Sign: (->* (Sym) (Bool) (Option (U 'font-lock-constant-face
                                   'font-lock-variable-name-face
                                   'font-lock-function-name-face)))

If SUBR-CALL?, means SYM is appeared in a subroutine call form.

Return a symbol that indicates a face or `nil'. If return nil, means this
symbol should be handled by other font-lock rules."
  (cond ((booleanp sym) nil)
        (subr-call?
         (when (fboundp sym)
           (let ((real-fn (elispfl--real-function sym)))
             (cond (
                    ;; Macro and special-form already had font lock.
                    (or (macrop real-fn)
                        (special-form-p real-fn))
                    nil)

                   ((subrp real-fn)
                    'font-lock-constant-face)
                   (t
                    'font-lock-function-name-face)))))
        ((special-variable-p sym)
         'font-lock-variable-name-face)))

(defsubst elispfl-inside-code? ()
  "Return t if current point not in comment or string.

Sign: (-> Bool)"
  (not (let ((ppss (syntax-ppss)))
         (or (nth 3 ppss) (nth 4 ppss)))))

(defun elispfl-extra-fontlock-matcher! (end)
  "Match defined variables and functions in current buffer limited to END.

Sign: (-> Long Bool)

Functions are differentiated into special forms, built-in functions and
library/userland functions."
  (catch 'stop
    (while (re-search-forward "\\_<.+?\\_>" end t)
      (when (elispfl-inside-code?)
        (let* ((sym (intern-soft (match-string-no-properties 0)))
               ;; NOTE: We treat symbol after left round bracket as subroutine.
               ;; May trigger false positive in list literal e.g. '(foo bar),
               ;; but it's suitable for most cases.
               ;; And another consideraion was that quotes were used
               ;; frequently in macros.
               (subr-call? (eq (char-before (match-beginning 0)) ?\())
               (face (elispfl--get-face sym subr-call?)))
          (when face
            (setq elispfl-face face)
            (throw 'stop t)))))
    nil))

;;;###autoload
(define-minor-mode elispfl-mode
  "Enhanced font lock for `emacs-lisp-mode'."
  :global t
  (let ((keywords-alist
         '((elispfl-extra-fontlock-matcher! . elispfl-face)))
        (executor (if elispfl-mode
                      #'font-lock-add-keywords
                    #'font-lock-remove-keywords)))
    (funcall executor 'emacs-lisp-mode keywords-alist)
    (font-lock-flush)))

(provide 'elispfl)
;;; elispfl.el ends here
