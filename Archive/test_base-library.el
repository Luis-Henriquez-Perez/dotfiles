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

(should (oo-snoc '(1 2)))
(should (oo-snoc '(1 2)))
(should (oo-snoc '(1 2)))
(should (oo-snoc '(1 2)))

(should (for! (repeat 10) 1))
(should (for! 10 1))
(should (for! [1 2 3 4] 1))

(should (oo-snoc '(1 2)))
(should (oo-snoc '(1 2)))
(should (oo-snoc '(1 2)))
(should (oo-snoc '(1 2)))
(should (oo-snoc '(1 2)))
(should (oo-snoc '(1 2)))

(ert-deftest oo-block-interpret-tree ()
  (should (equal '(nil ((catch 'break! (for! (n 10) (catch 'continue (+ 1 1))))))
                 (oo-block-interpret-tree nil '((for! (n 10) (+ 1 1))))))

  (should (equal '((:let ((foo nil))) ((collecting! foo 1)))
                 (oo-block-interpret-tree nil '((collecting! foo 1)))))

  (should (equal (nil (cl-flet ((foo nil (+ 1 1))) (+ 2 2)))
                 (oo-block-interpret-tree nil '((stub! foo () (+ 1 1)) (+ 2 2)))))

  (should (equal (oo-block-interpret-tree nil '((without! a b c)))
                 '((:no-let (a b c)) (nil))))

  (should (equal (oo-block-interpret-tree nil '((let! foo 1)))
                 `((:let ((foo nil))) (nil))))

  (should (equal ((:let ((gc-cons-threshold gc-cons-threshold))) (nil)))
          )
  (should (equal (oo-block-interpret-tree nil '((let! foo 1)))))

  (should (equal (oo-block-interpret-tree nil '((let! foo 1))))))

(ert-deftest oo-block ()
  (block! nil
    (+ 1 1)
    (appending! foo 2))

  (block! nil
    (+ 1 1)
    (maxing! foo 2)))
