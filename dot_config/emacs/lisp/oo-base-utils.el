;;; oo-base-utils.el --- TODO: add commentary -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Free Software Foundation, Inc.
;;
;; Author: Luis Henriquez-Perez <luis@luishp.xyz>
;; Homepage: https://github.com/Luis-Henriquez-Perez/dotfiles/
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; TODO: add commentary
;;
;;; Code:
;;;; requirements
(require 'cl-lib)
;;;; helpers
(defun oo-wrap-forms (wrappers forms)
  "Return FORMS wrapped by WRAPPERS.
FORMS is a list of forms to be wrapped.  WRAPPERS are a list of forms
representing the wrappers to apply.  If WRAPPERS is empty, `progn' is added to
ensure the result is syntactically valid."
  (declare (pure t) (side-effect-free t))
  (unless wrappers (push '(progn) wrappers))
  (setq wrappers (reverse wrappers))
  (setq forms (append (pop wrappers) forms))
  (dolist (wrapper wrappers)
    (setq forms (append wrapper (list forms))))
  forms)

(defun oo-into-string (&rest args)
  "Return ARGS as a string."
  (declare (pure t) (side-effect-free t))
  (with-output-to-string (mapc #'princ args)))

(defun oo-into-symbol (&rest args)
  "Return an interned symbol from ARGS."
  (declare (pure t) (side-effect-free t))
  (intern (apply #'oo-into-string args)))

(defun oo-into-keyword (&rest args)
  "Return ARGS as a keyword."
  (declare (pure t) (side-effect-free t))
  (apply #'oo-into-symbol ":" args))

;; This function is extremely useful for testing things.
;; (defun oo-shuffle (list)
;;   "Return list with its elements shuffled."
;;   ;; I got this from online.  Me, I would not use `cl-loop'.
;;   ;; https://stackoverflow.com/questions/49490551/how-to-shuffle-list-in-lisp
;;   ;; TODO: revise loop so it can do this from...downto stuff.
;;   (cl-loop for i from (length list) downto 2
;;            do (cl-rotatef (elt list (random i))
;;                           (elt list (1- i))))
;;   list)
(defun oo-shuffle (list)
  "Return a shuffled copy of LIST using the Fisher-Yates shuffle algorithm."
  (let ((copy (copy-sequence list))
        (n (length list)))
    (while (> n 1)
      (setq n (1- n))
      (let ((swap (random n))
            (temp (elt copy n)))
        (setf (elt copy n) (elt copy swap))
        (setf (elt copy swap) temp)))
    copy))
;;;; oo-condition-case-fn
;; One thing is the fact that because it is a function it can be composed and
;; chained.  Another is I can swap in and out the =condition-case= body and handlers
;; without having to write out the whole =condition-case= form.  Even though the
;; =condition-case= form is relatively simple I have admittedly had trouble
;; remembering its components off the top of my head.

;; I'd like to have the signature be something like ~(function &rest args &key ...)~;
;; that way it would be truly analogous to =funcall=. However, then the signature
;; would ambiguous if =function= has arguments that are the same as keys specified by
;; =&key=.
(defun oo-condition-case-fn (fn action &optional handlers)
  "Return a function that calls ACTION when errors matching HANDLERS are raised.
ACTION is a function with three arguments the error object, FN and the list of
arguments FN will be called with."
  ;; To be honest I'm not sure if I need to make a gensym for the variable
  ;; `err'.  I do it just in case.
  (cl-callf or handlers 'error)
  (cl-callf or action #'ignore)
  (cl-with-gensyms (err)
    `(lambda (&rest args)
       (condition-case ,err
           (apply #',fn args)
         (,handlers (funcall #',action ,err #',fn args))))))
(defalias 'oo-cond-case-fn 'oo-condition-case-fn)
;;;; oo-in-string-or-comment-p
;; This function is used by captain and abbrev.  Instead of redefining it twice,
;; I prefer to place it here.
(defun oo-in-string-or-comment-p ()
  "Return 'string if point is in a string, 'comment if in a comment, nil otherwise."
  (let ((ppss (syntax-ppss)))
    (cond ((nth 3 ppss) 'string)
          ((nth 4 ppss) 'comment)
          (t nil))))
;;;; if-not!
;; More often than not when I am using `if', the default else clause is simpler than
;; the then clause.  And in that case I end up having to wrap the then clause in
;; a `progn'. I want to invert the else clause and the if clause so I do not
;; need to include the extra `progn' in that case.  I also considered just
;; writing a macro that expands to an `if' with the then and else reversed, but
;; I think it might be confusing.
(defmacro if-not! (cond then &rest else)
  (declare (indent 2))
  `(if (not ,cond) ,then ,@else))

(defalias 'nif! 'if-not!)
;;; provide
(provide 'oo-base-utils)
;;; oo-base-utils.el ends here
