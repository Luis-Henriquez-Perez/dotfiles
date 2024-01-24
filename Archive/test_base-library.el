(require 'draft_oo-base-library)
(require 'ert)

(ert-deftest oo-cons-cell-p ()
  (should (oo-cons-cell-p (cons 1 2)))
  (should-not (oo-cons-cell-p (list 1 2)))
  (should-not (oo-cons-cell-p '(1 2 . 3)))
  (should-not (oo-cons-cell-p 1)))

(ert-deftest oo-proper-list-p ()
  (should (oo-proper-list-p '(1 2)))
  (should-not (oo-proper-list-p '(1 . 2)))
  (should-not (oo-proper-list-p '(1 2 . 3)))
  (should-not (oo-proper-list-p 10.5)))

(ert-deftest oo-block-interpret-tree ()
  (should (equal '(nil ((catch 'break! (for! (n 10) (catch 'continue (+ 1 1))))))
                 (oo-block-interpret-tree nil '((for! (n 10) (+ 1 1))))))

  (should (equal '((:let ((foo nil))) ((collecting! foo 1)))
                 (oo-block-interpret-tree nil '((collecting! foo 1)))))

  (should (equal (nil (cl-flet ((foo nil (+ 1 1))) (+ 2 2)))
                 (oo-block-interpret-tree nil '((stub! foo () (+ 1 1)) (+ 2 2)))))

  (should (equal (oo-block-interpret-tree nil '((stub! foo ()))))))

(ert-deftest oo-block ()
  (block! nil
    (+ 1 1)
    (appending! foo 2))

  (block! nil
    (+ 1 1)
    (maxing! foo 2)))
