(require 'ert)

;; (add-to-list 'load-path "~/.local/share/chezmoi/Archive")
(load-file "draft_oo-base-library.el")

(ert-deftest oo-cons-cell-p ()
  (should     (oo-cons-cell-p (cons 1 2)))
  (should-not (oo-cons-cell-p (list 1 2)))
  (should-not (oo-cons-cell-p '(1 2 . 3)))
  (should-not (oo-cons-cell-p 1)))

(ert-deftest oo-proper-list-p ()
  (should     (oo-proper-list-p '(1 2)))
  (should-not (oo-proper-list-p '(1 . 2)))
  (should-not (oo-proper-list-p '(1 2 . 3)))
  (should-not (oo-proper-list-p 10.5)))

(ert-deftest oo-snoc ()
  (should (equal '(1 2 3) (oo-snoc '(1 2) 3))))

(ert-deftest oo-wrap-forms ()
  (should (equal '(when 1 (save-excursion foo)) (oo-wrap-forms '((when 1) (save-excursion)) '(foo)))))

;; (ert-deftest oo-non-keyword-symbol-p ()
;;   (should (oo-non-keyword-symbol-p 'foo))
;;   (should-not (oo-non-keyword-symbol-p :foo))
;;   (should-not (oo-non-keyword-symbol-p "foo"))
;;   (should-not (oo-non-keyword-symbol-p 1)))

;; (ert-deftest oo-args-to-string ()
;;   (should (equal "foo" (oo-args-to-string 'foo)))
;;   (should (equal ":foo" (oo-args-to-string :foo)))
;;   (should (equal "foo" (oo-args-to-string "foo")))
;;   (should (equal "1" (oo-args-to-string 1))))

;; (ert-deftest oo-args-to-symbol ()
;;   (should (equal 'foo (oo-args-to-symbol 'foo)))
;;   (should (equal :foo (oo-args-to-symbol :foo)))
;;   (should (equal 'foo (oo-args-to-symbol "foo")))
;;   (should (equal '1 (oo-args-to-symbol 1))))

(ert-deftest alet! ()
  (should (= 2 (alet! 1 (+ it it)))))

(ert-deftest aif! ()
  (should (= 1 (aif! 1 it 0))))

(ert-deftest awhen! ()
  (should (= 2 (awhen! 1 (+ it it)))))

;; (ert-deftest adjoining! ()
;;   (adjoining!))

(ert-deftest for! ()
  ;; Works for the syntax =(repeat N)= where N is a positive integer.
  (should (= 11 (let ((n 1)) (for! (repeat 10) (cl-incf n)) n)))

  ;; Also works if you just pass in the raw number.
  ;; Not the most precise because I cannot specify that its the same symbol for
  ;; all of =,(pred symbolp)= but good enough for now.
  (should (= 11 (let ((n 1)) (for! 10 (cl-incf n)) n)))

  ;; Should allow me to loop through sequences.
  (should (equal '(111 108 108 101 104)
                 (let (chars) (for! (char "hello") (push char chars)) chars)))

  (should (equal '(4 3 2 1)
                 (let (nums) (for! (n [1 2 3 4]) (push n nums)) nums)))

  (should (equal '(4 3 2 1)
                 (let (nums) (for! (n '(1 2 3 4)) (push n nums)) nums)))

  ;; Should allow me to destructure arguments.
  ;; (should '(for! (a b) '((1 2) (4 5))))
  )

;; (ert-deftest oo-block-interpret-tree ()
;;   (should (equal '(nil ((catch 'break! (for! (n 10) (catch 'continue (+ 1 1))))))
;;                  (oo-block-interpret-tree nil '((for! (n 10) (+ 1 1))))))

;;   (should (equal '((:let ((foo nil))) ((collecting! foo 1)))
;;                  (oo-block-interpret-tree nil '((collecting! foo 1)))))

;;   (should (equal (nil (cl-flet ((foo nil (+ 1 1))) (+ 2 2)))
;;                  (oo-block-interpret-tree nil '((stub! foo () (+ 1 1)) (+ 2 2)))))

;;   (should (equal (oo-block-interpret-tree nil '((without! a b c)))
;;                  '((:no-let (a b c)) (nil))))

;;   (should (equal (oo-block-interpret-tree nil '((let! foo 1)))
;;                  `((:let ((foo nil))) (nil))))

;;   ;; (should (equal ((:let ((gc-cons-threshold gc-cons-threshold))) (nil)))
;;   ;;         )
;;   ;; (should (equal (oo-block-interpret-tree nil '((let! foo 1)))))

;;   ;; (should (equal (oo-block-interpret-tree nil '((let! foo 1)))))
;;   )

;; (ert-deftest block! ()
;;   (block! nil (+ 1 1) (maxing! foo 2))
;;   (should (pcase (block! nil (+ 1 1) (appending! foo 2))
;;             (`)
;;             (_))))
