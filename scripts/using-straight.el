;; This file has code for installing packages with [[][straight.el]].
;; https://github.com/lccambiaghi/vanilla-emacs

;; Store the package recipes.
(defvar oo-recipe-list nil)

;;; straight.el

;;;; recipe defining macro
;; O.K., so this macro is simple but I wanted to be able to populate
;; =oo-recipes= without having to nest anything.  Also I wanted the
;; ability to.

;;;; recipes

(recipe! )

;;;; register all packages
(dolist (recipe oo-recipes)
  (straight-register-recipe recipe))

;;;; clone packages
(dolist (recipe )
  ())

;;;; set the commit
(dolist (recipe foo)
  ;; checkout the proper commit.
  ())

;;;; now actually load the packages 
(dolist (recipe recipes)
  (straight-use-package recipe))
