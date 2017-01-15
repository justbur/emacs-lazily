;;; lazily.el --- lazily configure Emacs

;; Copyright (C) 2017 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; Homepage: https://github.com/justbur/emacs-lazily
;; Version: 0.0.0

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package simply provides the macro `lazily-do' for use in Emacs
;; configuration files.  It only does one thing.  You can wrap any configuration
;; steps in the macro without having to worry about void variable messages or
;; void variable messages.  In the example below good-list is declared, but
;; Emacs doesn't know about bad-list yet.  This will throw a void-variable
;; error.

;; (defvar good-list nil)
;; (lazily-do
;;  (add-to-list 'good-list 1)
;;  (add-to-list 'bad-list 1))

;; We have a couple of options here.  We could figure out which library defines
;; bad-list and use `with-eval-after-load' like this.

;; (with-eval-after-load 'bad-library
;;  (add-to-list 'bad-list 1))

;; We could also explicitly require bad-library, which might be slow, or
;; something else.  The way `lazily-do' works is it stores the forms in a list
;; and tries again to eval those forms (in order) every time a new library is
;; loaded.  It accomplishes this through using the `after-load-functions' hook.

;; To illustrate, suppose we had the original example in our config.  The form
;; with good-list will execute fine and there will be no difference to having
;; this form at the top level.  The one with bad-list throws a void-variable
;; error which is caught and the form is saved for later.  We do some work and
;; eventually something calls or loads bad-library.  This fires `lazily--redo'
;; to try the saved forms again, and now that bad-list is defined the original
;; error goes away and everything is dandy.

;;; Code:

(require 'pp)

(defgroup lazily nil
  "Lazily configure Emacs"
  :group 'startup)

(defcustom lazily-is-quiet t
  "If nil, log failed forms in message buffer."
  :type 'boolean
  :group 'lazily)

(defvar lazily--bad-forms nil
  "Forms currently with void variables or functions. This is a
list of lists of lists. Each sub list has the structure

\(\(FORM ERROR-DATA FILE\)
  \(FORM ERROR-DATA FILE\)
  ...\)

A sub list represents dependencies, since the latter forms in the
list may only execute if the ones before it can. Once one fails
we stop checking and keep the sub list.

ERROR-DATA is from condition-case, and FILE (if we know it) is
the file being loaded when the error is encountered.")

(defun lazily--try-form-again-p (form-data)
  (or (null (nth 1 form-data))
      (and (eq 'void-variable (car (nth 1 form-data)))
           (boundp (cadr (nth 1 form-data))))
      (and (eq 'void-function (car (nth 1 form-data)))
           (fboundp (cadr (nth 1 form-data))))))

(defun lazily--redo (&rest _)
  "Try to execute all forms in `lazily--bad-forms'.

Any forms that throw void-variable or void-function errors are
kept in `lazily--bad-forms' to be tried again later."
  (dotimes (i (length lazily--bad-forms))
    (let ((dep-list (nth i lazily--bad-forms)) err)
      (while (and (null err) dep-list)
        (let* ((form-data (pop dep-list))
               (form (car form-data)))
          (if (lazily--try-form-again-p form-data)
              (condition-case error-data
                  (eval form)
                ((void-function void-variable)
                 (setq err t)
                 (push (list form error-data nil) dep-list)))
            (setq err t)
            (push form-data dep-list))))
      (setf (nth i lazily--bad-forms) dep-list)))
  (setq lazily--bad-forms (delq nil lazily--bad-forms))
  (when (null lazily--bad-forms)
    (remove-hook 'after-load-functions 'lazily--redo)))

(defun lazily--log (error-data form)
  "Print log message about bad form if enabled."
  (unless lazily-is-quiet
    (message "lazily: Found void (unknown) %s `%s' in form
        %s"
             (if (eq (car error-data) 'void-variable)
                 "variable" "function")
             (car (cdr-safe error-data))
             (pp-to-string form))))

(defun lazily-report ()
  "Produce report of bad forms found."
  (interactive)
  (let ((buffer (get-buffer-create "*lazily-report*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "lazily found the following errors in forms:\n\n")
      (dolist (dep-list lazily--bad-forms)
        (let* ((bad-form-data (car dep-list))
               (form (nth 0 bad-form-data))
               (err (car (nth 1 bad-form-data)))
               (sym (cadr (nth 1 bad-form-data)))
               (file (nth 2 bad-form-data))
               (remain (1- (length dep-list))))
          (insert
           (format "[%s] %s %s found in form which blocks %d form(s)\n  %s\n"
                   (or file "unknown file")
                   (or (replace-regexp-in-string
                        "-" " " (symbol-name err))
                       "unknown error for")
                   (or sym "unknown symbol")
                   (or remain 0)
                   (pp-to-string form))))))
    (switch-to-buffer-other-window buffer)
    (local-set-key "q" 'delete-window)))

;;;###autoload
(defmacro lazily-do (&rest forms)
  "Eval FORMS catching void-variable or void-function errors.

Any forms that throw void-variable or void-function errors are
stored in `lazily--bad-forms' to be tried again later. This
version stops after finding the first error and collects all
remaining forms to depend on the first one executing in
`lazily--bad-forms'. For the greedy alternative, see
`lazily-do-all'.

Specifically, a `after-load-functions' hook is added to try and
execute these bad forms again after a new feature is loaded. The
error condition is stored and checked before executing to avoid
executing more than once."
  (let ((wrapped
         (mapcar
          (lambda (form)
            `(if stop
                 (push (list ',form) skipped-forms)
               (condition-case error-data
                   ,form
                 ((void-variable void-function)
                  (lazily--log error-data ',form)
                  (setq stop t)
                  (setq bad-form-data
                        (list ',form error-data load-file-name))))))
          forms)))
    `(let (stop bad-form-data skipped-forms)
       ,@wrapped
       (setq lazily--bad-forms
             (append lazily--bad-forms
                     (list (cons bad-form-data (nreverse skipped-forms)))))
       (add-hook 'after-load-functions #'lazily--redo))))

;;;###autoload
(defmacro lazily-do-all (&rest forms)
  "Greedy version of `lazily-do'.

This macro continues to execute subsequent forms after finding a
bad one."
  (let ((wrapped
         (mapcar
          (lambda (form)
            `(condition-case error-data
                 ,form
               ((void-variable void-function)
                (lazily--log error-data ',form)
                ;; two lists because there are no dependencies in this case
                (push (list (list ',form error-data load-file-name))
                      error-forms))))
          forms)))
    `(let (error-forms)
       ,@wrapped
       (when error-forms
         (setq lazily--bad-forms
               (append lazily--bad-forms (nreverse error-forms)))
         (add-hook 'after-load-functions #'lazily--redo)))))

(provide 'lazily)
;;; lazily.el ends here
