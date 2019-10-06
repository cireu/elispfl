;;; elispfl.el --- Extra font locks made your Elisp mode fancy  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Zhu Zihao

;; Author: Zhu Zihao <all_but_last@163.com>
;; URL: https://github.com/cireu/elispfl
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
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
(require 'lisp-mode)
(eval-when-compile (require 'cl-lib))

(defvar elispfl-face nil
  "A variable to hold current face used to render.")

(defalias 'elispfl--font-lock-flush (if (fboundp 'font-lock-flush)
                                        'font-lock-flush
                                      'font-lock-fontify-buffer)
  "A shim of `font-lock-fontify-buffer' for 24.3.")

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
                                   'font-lock-function-name-face
                                   'font-lock-keyword-face)))

If SUBR-CALL?, means SYM is appeared in a subroutine call form.

Return a symbol that indicates a face or `nil'. If return nil, means this
symbol should be handled by other font-lock rules."
  (cond ((booleanp sym) nil)
        (subr-call?
         (when (fboundp sym)
           (let ((real-fn (elispfl--real-function sym)))
             (cl-typecase real-fn
               ((or macro special-form) 'font-lock-keyword-face)
               (subr 'font-lock-constant-face)
               (otherwise 'font-lock-function-name-face)))))
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

(defvar elispfl--elisp-mode-extra-font-lock-keyword
  '((elispfl-extra-fontlock-matcher! . elispfl-face)))

;;;###autoload
(define-minor-mode elispfl-mode
  "Enhanced font lock for `emacs-lisp-mode'."
  :global t
  (let ((executor (if elispfl-mode
                      #'font-lock-add-keywords
                    #'font-lock-remove-keywords)))
    (funcall executor 'emacs-lisp-mode
             elispfl--elisp-mode-extra-font-lock-keyword)
    (elispfl--font-lock-flush)))

(defun elispfl--constrain-matcher-to-after-prompt (matcher)
  "Constrain a font-lock matcher only match the contents after comint prompt.

Sign: (-> (U Str (-> Long Bool)) (-> Long Bool))"
  (lambda (end)
    (and (cl-typecase matcher
           (string (re-search-forward matcher end t))
           (otherwise (funcall matcher end)))
         (let ((prompt-end (cdr comint-last-prompt))
               (start (match-beginning 0)))
           (> start prompt-end))
         ;; Some matcher don't handle this
         (elispfl-inside-code?))))

(defvar elispfl--ielm-extra-font-lock-keywords
  (cl-labels ((map-first-item (func list)
                (mapcar (cl-function
                         (lambda ((first . rest))
                           (cons (funcall func first) rest)))
                        list)))
    (map-first-item #'elispfl--constrain-matcher-to-after-prompt
                    `((elispfl-extra-fontlock-matcher! . elispfl-face)
                      ,@lisp-el-font-lock-keywords-1
                      ,@lisp-el-font-lock-keywords-2)))
  "A List of font-lock rules will be applied to `ielm'.")

;;;###autoload
(define-minor-mode elispfl-ielm-mode
  "Enhanced font lock for `ielm'."
  :global t
  (let ((executor (if elispfl-mode
                      #'font-lock-add-keywords
                    #'font-lock-remove-keywords)))
    (funcall executor 'inferior-emacs-lisp-mode
             elispfl--ielm-extra-font-lock-keywords)
    (elispfl--font-lock-flush)))

(provide 'elispfl)
;;; elispfl.el ends here
