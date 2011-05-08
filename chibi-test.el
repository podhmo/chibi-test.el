;;; chibi-test.el --- chibi unittest

;; Copyright (C) 2011  podhmo

;; Author: podhmo <ababjam61@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Example:
;; (require 'chibi-test)
;; (with-chibi-test*
;;  (section "simplest example"
;;           (test "1+1=2" 2 (+ 1 1))
;;           (section "ng"
;;                    (test "ng 1+1=2" 10 (+ 1 1)))

;;           (defmacro let1 (b e &rest body)
;;             `(let ((,b ,e)) ,@body))

;;           (section "macro"
;;                    (test: "macro test(let1)" 
;;                           '(let ((b 10)) (* b b))
;;                           (macro (let1 b 10 (* b b)))))))
      
;; 

;;; Code:
(require 'cl)

;; internal variables
(defvar chibi-test:output-buffer-name "* chibi test output *")
(defvar chibi-test:prefix-space-vector
  (apply 'vector
         (cons ""
               (loop for i from 0 to 30 by 3
                     collect (make-string i ? )))))
(defvar chibi-test:current-level 0)
(defvar chibi-test:success-count 0)
(defvar chibi-test:fail-count 0)

(defun chibi-test:output (&rest args)
  (with-current-buffer (get-buffer-create chibi-test:output-buffer-name)
    (let ((prefix-space (aref chibi-test:prefix-space-vector 
                              chibi-test:current-level
                              )))
      (dolist (e args)
        (insert (format "%s%s" prefix-space e) "\n"))))
  (display-buffer chibi-test:output-buffer-name))

(defun chibi-test:output* (&rest args)
  (with-current-buffer (get-buffer-create chibi-test:output-buffer-name)
    (insert "\n"))
  (apply 'chibi-test:output args))

(defun chibi-test:internal-variables-initialize! ()
    (setq chibi-test:current-level 0)
    (setq chibi-test:success-count 0)
    (setq chibi-test:fail-count 0))

(defun chibi-test:clear () (interactive)
  (let ((buf (get-buffer chibi-test:output-buffer-name)))
    (and buf
         (with-current-buffer buf
           (erase-buffer)))
    (chibi-test:internal-variables-initialize!)))

(defface chibi-test:success-message-face
  '((t
     (:foreground "spring green"
                  ;;:background "dark grey"
                  :italic t
                  :bold t)))
  "")
(defvar chibi-test:success-message-face 'chibi-test:success-message-face)

(defface chibi-test:fail-message-face
  '((t
     (:foreground "orange red"
                  ;;:background "dark grey"
                  :italic t
                  :bold t)))
  "")
(defvar chibi-test:fail-message-face 'chibi-test:fail-message-face)
(defface chibi-test:fail-return-value-face
  '((t
     (:foreground "dark magenta"
                  ;;:background "dark grey"
                  :italic t
                  :bold t)))
  "")
(defvar chibi-test:fail-return-value-face 'chibi-test:fail-return-value-face)


(defvar chibi-test:max-length 20)
(defun* chibi-test:truncate (x &optional (max-length chibi-test:max-length))
  (let ((content (format "%s"  x)))
    (cond ((<= (length content) max-length) content)
          (t (concat (substring content 0 max-length) "...")))))

(defun chibi-test:test-condition (expect result)
  (equal expect result))

(defun chibi-test:test-not-condition (expect result)
  (not (equal expect result)))

(defmacro chibi-test:test (message expect expr)
  (chibi-test:test-internal
   message expect expr
   :prefix "test"
   :condition-function 'chibi-test:test-condition))

(defmacro chibi-test:test-not (message expect expr)
  (chibi-test:test-internal
   message expect expr
   :prefix "test-not"
   :condition-function 'chibi-test:test-not-condition))

(defmacro chibi-test:test-true (message expr)
  (chibi-test:test-internal
   message nil expr
   :prefix "test-true"
   :condition-function 'chibi-test:test-not-condition))

(defmacro chibi-test:test-false (message expr)
  (chibi-test:test-internal
   message nil  expr
   :prefix "test-false"
   :condition-function 'chibi-test:test-condition))

(defun* chibi-test:test-internal
    (message expect expr
             &key (prefix "test") (condition-function 'chibi-test:test-condition))
  (let ((result (gensym)))
    `(progn
       (chibi-test:output (format "%s: %s" ,prefix ,message))
       (let ((,result ,expr))
         (cond ((,condition-function ,expect ,result)
                (chibi-test:output 
                 (propertize (format "ok! %s" (chibi-test:truncate ,result))
                             'face 'chibi-test:success-message-face))
                 (incf chibi-test:success-count))
               (t
                (chibi-test:output 
                 (propertize (format "fail! expect: %s" ,expect)
                             'face 'chibi-test:fail-message-face))
                (chibi-test:output
                 (propertize (format "      return: %s" ,result)
                             'face 'chibi-test:fail-return-value-face))
                (incf chibi-test:fail-count)))))))


(defun chibi-test:section (section)
  (chibi-test:output* (format "--------%s------------" section)))

(defmacro chibi-test:expect-macro (body)
  `(macroexpand-all (quote ,body)))

;;; DSL
(defvar chibi-test:hide-after-n-sec 3)
(defvar chibi-test:hide-view-result-p t)
(defvar chibi-test:hide-view-result-timer nil)

(defun chibi-test:hide-view-result ()
  (let ((buf (get-buffer chibi-test:output-buffer-name)))
    (unless (equal buf (current-buffer))
      (let ((output-window 
             (find buf (window-list)
                   :test (lambda (b w) (equal b (window-buffer w))))))
        (and output-window
             (delete-window output-window))
        (message "chibi-test: -- hide view result --")))))

(defun chibi-test:hide-view-result-after-n-sec (&optional sec)
  (let ((sec (or sec chibi-test:hide-after-n-sec)))
    (when chibi-test:hide-view-result-p
      
      (when chibi-test:hide-view-result-timer
        (cancel-timer chibi-test:hide-view-result-timer)
        (setq hibi-test:hide-view-result-timer nil))

      (setq chibi-test:hide-view-result-timer
            (run-with-timer 
             sec  nil 'chibi-test:hide-view-result)))))

(defun chibi-test:mapcar-safe (fn maybe-list)
  "mapcar enable to iterate maybe-list (include dot-list)"
  (let ((r (list)) (xs maybe-list))
    (condition-case e
        (progn
          (while (not (null xs))
            (push (funcall fn (car xs)) r)
            (setq xs (cdr xs)))
          (nreverse r))
      (wrong-type-argument 
       (let ((r* (nreverse r)))
         (setcdr (last r*) (funcall fn xs))
         r*)))))

(defmacro with-chibi-test (&rest exprs)
  (labels ((%replace-and-status
            (expr)
            (destructuring-bind (func . args) expr
              (case func
                ((section section:)
                 (destructuring-bind (section-content . child-exprs) args
                   (values `(chibi-test:section ,section-content)
                           t 1
                           (%rec-replace-tree child-exprs))))
                ((clear clear:) (values `(chibi-test:clear ,@(%rec-replace-tree args)) t 0 nil))
                ((test test:) (values `(chibi-test:test ,@(%rec-replace-tree args)) t 1 nil))
                ((test-not test-not:) (values `(chibi-test:test-not ,@(%rec-replace-tree args)) t 1 nil))
                ((test-true test-true:) (values `(chibi-test:test-true ,@(%rec-replace-tree args)) t 1 nil))
                ((test-false test-false:) (values `(chibi-test:test-false ,@(%rec-replace-tree args)) t 1 nil))
                ((macro macro:) (values `(chibi-test:expect-macro ,@args) t 0 nil))
                (otherwise (values `(,func ,@(%rec-replace-tree args)) nil 0 nil)))))

           (%change-depth-with-delta
            (body delta &optional chidren)
            (cond ((<= delta 0) body)
                  (t
                   `(progn 
                      (incf chibi-test:current-level ,delta)
                      ,body ,@chidren
                      (decf chibi-test:current-level ,delta)))))

           (%rec-replace-tree
            (tree)
            (chibi-test:mapcar-safe
             #'(lambda (x)
                 (cond ((listp x)
                        (multiple-value-bind (x* status delta children) (%replace-and-status x)
                          (cond (status (%change-depth-with-delta x* delta children))
                                (t x*))))
                       ((or (eq x 'clear) (eq x 'clear:)) `(chibi-test:clear))
                       (t x)))
             tree)))
    `(progn
       ,@(%rec-replace-tree exprs)
       (chibi-test:short-description)
       (chibi-test:hide-view-result-after-n-sec))))

(defun chibi-test:short-description ()
  (let ((short-description (format "
========= short discription ===========
  Total: %d  OK: %d  NG: %d
=======================================
"
                                   (+ chibi-test:success-count chibi-test:fail-count)
                                   chibi-test:success-count
                                   chibi-test:fail-count)))
    (if (<= chibi-test:fail-count 0)
        (chibi-test:output short-description)
        (chibi-test:output
         (propertize short-description 'face 'chibi-test:fail-message-face)))))


(defmacro with-chibi-test* (&rest exprs)
  (let ((err (gensym)))
    `(condition-case ,err 
         (with-chibi-test
        clear
        ,@exprs)
       (error (chibi-test:internal-variables-initialize!)
              (message ,err)))))

;;; test
(defvar chibi-test:test-is-running-p nil)
;;(setq chibi-test:test-is-running-p t)
(when chibi-test:test-is-running-p
  ;; Don't use chibi-test:<foo> directly as bellow
  (chibi-test:clear)
  (chibi-test:section "using chibi-test:macro, directly")
  (chibi-test:test "1+1" 2 (+ 1 1))
  (chibi-test:test "with-chibi-test(simple)"
                   '(progn
                      (progn
                        (setq chibi-test:current-level (+ chibi-test:current-level 1))
                        (chibi-test:section "section")
                        (setq chibi-test:current-level (- chibi-test:current-level 1))))
                   (chibi-test:expect-macro
                    (with-chibi-test
                     (section "section"))))

  ;; using `with-chibi-test*' rather than `with-chibi-test'
  (with-chibi-test
   clear ;; when using `with-chibi-test' clear output buffer manually
   (section "with-chibi-test, with `with-chibi-test'"
            (test "1+1=2" 2 (+ 1 1))
            (test-not "1+1=10" 10 (+ 1 1))))

  (with-chibi-test*
   (section "utitlity"
            (test 'chibi-test:truncate1 "xx" (chibi-test:truncate "xx"))
            (test 'chibi-test:truncate2 "xx000..."
                  (chibi-test:truncate "xx00000000000" 5))
            (test-not 'chibi-test:truncate2 "xx00000000000"
                      (chibi-test:truncate "xx00000000000" 5)))

   (section "macro"
            (test "with-chibi-test+section(simple)"
                  '(progn
                     (progn
                       (setq chibi-test:current-level (+ chibi-test:current-level 1))
                       (chibi-test:section "section")
                       (setq chibi-test:current-level (- chibi-test:current-level 1)))
                     (chibi-test:short-description)
                     (chibi-test:hide-view-result-after-n-sec))
                  (macro
                   (with-chibi-test
                    (section "section"))))))
  )
(provide 'chibi-test)
;;; chibi-test.el ends here
