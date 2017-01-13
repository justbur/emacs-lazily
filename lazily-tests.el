;;; lazily-tests.el --- Tests for lazily.el

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

;;; Code:

(require 'lazily)
(require 'ert)

(ert-deftest lazily-tests-check-form-processing ()
  "Check that forms are added to `lazily--bad-forms' correctly."
  (let ((obarray obarray)
        (good-list '(1))
        lazily--bad-forms)
    (cl-flet ((good-func (lambda () (message "good"))))
      (unintern 'bad-list obarray)
      (unintern 'bad-func obarray)
      (lazily-do
       (add-to-list 'good-list 2)
       (add-to-list 'bad-list 2)
       (good-func)
       (bad-func))
      (should
       (equal lazily--bad-forms
              '(((void-variable bad-list) . (add-to-list 'bad-list 2))
                ((void-function bad-func) . (bad-func)))))
      (defun bad-func () nil)
      (lazily--redo)
      (should
       (equal lazily--bad-forms
              '(((void-variable bad-list) . (add-to-list 'bad-list 2)))))
      (setq bad-list '(1))
      (lazily--redo)
      (should (null lazily--bad-forms)))))
