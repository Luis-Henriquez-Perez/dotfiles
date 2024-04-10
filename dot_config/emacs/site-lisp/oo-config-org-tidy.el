(defun oo-org-tidy--element-to-ov (element)
  "Turn a single property ELEMENT into a plist for merge."
  (let* ((should-tidy (org-tidy-should-tidy element))
         (beg (org-element-property :begin element))
         (end (org-element-property :end element))
         (is-top-property (= 1 beg))
         (push-ovly nil)
         (display nil)
         (choices (list is-top-property org-tidy-top-property-style org-tidy-properties-style)))
    
    (pcase choices
      (`(t invisible ,_)
       (setq display 'empty push-ovly t))

      (`(t keep ,_) )

      ;; Unless I have this condition I am not able to make property drawers
      ;; invisible because `is-top-property' is nil, but I want to make all the
      ;; property drawers disappear.
      (`(nil invisible ,_)
       (setq display 'empty push-ovly t))

      (`(nil ,_ inline)
       (setq display 'inline-symbol push-ovly t))

      (`(nil ,_ fringe)
       (setq display 'fringe push-ovly t)))

    (when (and should-tidy push-ovly)
      (list :beg beg
            :end end
            :is-top-property is-top-property
            :display display))))

(advice-add #'org-tidy--element-to-ov :override #'oo-org-tidy--element-to-ov)
