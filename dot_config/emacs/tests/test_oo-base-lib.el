;; Does not need to be required because already autoloaded.

(require 'oo-base-lib)

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

(ert-deftest oo-improper-list-p ()
  (should-not (oo-improper-list-p '(1 2)))
  (should (oo-improper-list-p '(1 . 2)))
  (should (oo-improper-list-p '(1 2 . 3)))
  (should-not (oo-improper-list-p 10.5)))

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

(ert-deftest adjoining! ()
  (let (adjoined)
    (adjoining! adjoined 1)
    (adjoining! adjoined 1)
    (should (equal '(1) adjoined))
    ;; (should)
    ))

;; (ert-deftest collecting! ()
;;   (let (collected)
;;     (collecting! adjoined 1)
;;     (collecting! adjoined 1)
;;     (should (equal '(1) collected))
;;     ;; (should)
;;     ))

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
  (should (equal '(3 9) (let (list) (for! (a b) '((1 2) (4 5)))))))

(ert-deftest take-while! ()
  (should (let ((foo '(a b 1 2 3))) (list (take-while! (symbolp foo) foo) foo))
          '((a b) (1 2 3)))
  ;; (should ())
  )

(ert-deftest oo-block-interpret-tree ()
  (should (equal '(nil ((catch 'break! (for! (n 10) (catch 'continue (+ 1 1))))))
                 (oo-block-interpret-tree nil '((for! (n 10) (+ 1 1))))))

  (should (equal '((:let ((foo nil))) ((collecting! foo 1)))
                 (oo-block-interpret-tree nil '((collecting! foo 1)))))

  ;; Stubbing functions.
  (should (equal '(nil ((cl-flet ((foo nil (+ 1 1))) (+ 2 2))))
                 (oo-block-interpret-tree nil '((stub! foo () (+ 1 1)) (+ 2 2)))))

  ;; Getting data from `without!'.
  (should (equal (oo-block-interpret-tree nil '((without! a b c)))
                 '((:no-let (a b c)) (nil))))

  ;; Getting data from `let!'.
  (should (equal `((:let ((foo nil))) ((setq foo 1)))
                 (oo-block-interpret-tree nil '((let! foo 1)))))
  
  (should (equal '((:let ((gc-cons-threshold gc-cons-threshold))) ((setq gc-cons-threshold 100)))
                 (oo-block-interpret-tree nil '((let! gc-cons-threshold 100)))))

  ;; I want to see how it works with multiple data.
  )

;; (ert-deftest block! ()
;;   (block! nil (+ 1 1) (maxing! foo 2))
;;   (should (pcase (block! nil (+ 1 1) (appending! foo 2))
;;             (`)
;;             (_))))

(ert-deftest oo-pcase-pattern ()
  (should (equal '`((,a (,b) ,c . ,d)) (oo-pcase-pattern '((a (b) c . d)))))
  (should (equal '`[,a ,b ,c] (oo-pcase-pattern [a b c])))
  (should (equal '`(,a [,b ,c (,f [,g]) [,d]] ,e)
                 (oo-pcase-pattern '(a [b c (f [g]) [d]] e)))))

(provide 'test_oo-base-lib)
