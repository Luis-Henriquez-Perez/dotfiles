;; (require 'oo-base-library)
(require 'ert)

(should (equal '(1 2) (oo-tree-transform-nodes #'oo-block-interpret-tree '(1 2))))

(should (let ((x (oo-tree-transform-nodes #'oo-block-interpret-tree '((for! (n 10) (+ 1 1))))))
          (equal x '((catch 'break! (for! (n 10) (catch 'continue (+ 1 1))))))))

(should (let ((x (oo-block-interpret-tree '(for! (n 10) (+ 1 1)))))
          (equal x '(catch 'break! (for! (n 10) (catch 'continue (+ 1 1)))))))

(should (let (x) (equal x '(let ,foo ,@body))))

(should (for! (n 10) (+ 1 1)))

(should (oo-destructure '(a b) '(list 1 2)))
