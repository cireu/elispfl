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

;; Run `elispfl-local-mode' or `global-elispfl-mode', made your emacs lisp mode
;; much more fancy!

;;; Code:

(require 'advice)
(require 'font-lock)

(defvar elispfl-face nil
  "A variable to hold current face used to render.")

(defun elispfl--real-function (sym)
  "Unwinding function chain of SYM and return real function definition.

Sign: (-> Sym Fn)

All alias and advice will be remove."
  (let ((fn (indirect-function sym)))
    (while (let* ((unadvised (ad-get-orig-definition fn))
                  (unaliased (indirect-function unadvised)))
             (setq fn unaliased)
             (not (eq unaliased unadvised))))
    fn))

(defun elispfl--get-face (sym &optional head?)
  "Get appropriate face of SYM.

Sign: (->* (Sym) (Bool) (Option (U 'font-lock-constant-face
                                   'font-lock-variable-name-face
                                   'font-lock-function-name-face)))"
  (cond ((booleanp sym) nil)
        ((special-variable-p sym)
         'font-lock-variable-name-face)
        ((and (fboundp sym)
              head?)
         (let ((real-fn (elispfl--real-function sym)))
           ;; Macro and special-form already had font lock.
           (unless (or (macrop real-fn)
                       (special-form-p real-fn))
             (if (subrp real-fn)
                 'font-lock-constant-face
               'font-lock-function-name-face))))))

(defsubst elispfl-inside-code? ()
  "Return t if current point not in comment or string.

Sign: (-> Bool)"
  (not (save-excursion
         (let ((ppss (syntax-ppss)))
           (or (nth 3 ppss) (nth 4 ppss))))))

(defun elispfl-extra-fontlock-matcher! (end)
  "Match defined variables and functions.

Sign: (-> Long Bool)

Functions are differentiated into special forms, built-in functions and
library/userland functions"
  (catch 'stop
    (while (re-search-forward "\\_<.+?\\_>" end t)
      (when (elispfl-inside-code?)
        (let* ((sym (intern-soft (match-string-no-properties 0)))
               (head? (eq (char-before (match-beginning 0)) ?\())
               (face (elispfl--get-face sym head?)))
          (when face
            (setq elispfl-face face)
            (throw 'stop t)))))
    nil))

;;;###autoload
(define-minor-mode elispfl-local-mode
  "Enhanced font lock for `emacs-lisp-mode'."
  :global t
  (let ((keywords-alist
         '((elispfl-extra-fontlock-matcher! . elispfl-face)))
        (executor (if elispfl-local-mode
                      #'font-lock-add-keywords
                    #'font-lock-remove-keywords)))
    (funcall executor 'emacs-lisp-mode keywords-alist)))

(defun elispfl--turn-on ()
  (elispfl-local-mode))

;;;###autoload
(define-globalized-minor-mode global-elispfl-mode elispfl-local-mode elispfl--turn-on)

(provide 'elispfl)
;;; elispfl.el ends here
