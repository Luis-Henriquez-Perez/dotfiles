;; Not =cl= which is depreciated.  Loading =cl= instead of =cl-lib= will create an annoying warning message
;; during startup that says package cl is depreciated.
;; https://emacs.stackexchange.com/questions/66758/package-cl-is-deprecated-is-there-any-easy-fix-for-it
(require 'cl-lib)

(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order `(elpaca
	                      :repo "https://github.com/progfolio/elpaca.git"
	                      :ref "9478158"
	                      :files (:defaults (:exclude "extensions"))
	                      :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	    (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		         ((zerop (call-process "git" nil buffer t "clone"
				                       (plist-get order :repo) repo)))
		         ((zerop (call-process "git" nil buffer t "checkout"
				                       (plist-get order :ref))))
		         (emacs (concat invocation-directory invocation-name))
		         ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
				                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
		         ((require 'elpaca))
		         ((elpaca-generate-autoloads "elpaca" repo)))
	        (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	      (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

(defun oo-prefer-depth-1 (_) '(:depth 1))
(add-hook 'elpaca-order-functions #'oo-prefer-depth-1)

;; Install all of the packages I want and with their specified recipes.
;; (elpaca (elpaca :repo "https://github.com/progfolio/elpaca.git"
;;                 :branch "master"
;;                 :ref "9478158"
;;                 :files (:defaults (:exclude "extensions"))
;;                 :build (:not elpaca--activate-package)))

;; (elpaca (dogears :fetcher github :repo "alphapapa/dogears.el" :files (:defaults (:exclude "helm-dogears.el"))))

;; (elpaca (captain :repo "git://git.sv.gnu.org/emacs/elpa" :local-repo "captain" :branch "externals/captain" :ref "364ee98"))

;; (elpaca (lambda-themes :repo "Lambda-Emacs/lambda-themes" :branch "main" :fetcher github :ref "7342250"))

;; (elpaca (consult :repo "minad/consult" :fetcher github :branch "main" :ref "fae9b50"))

;; (elpaca (textsize :repo "WJCFerguson/textsize" :fetcher github :branch "master" :ref "df91392"))

;; (elpaca (on :host github :repo "ajgrf/on.el" :branch "master" :ref "3cf623e"))

;; (elpaca (ligature :fetcher github :repo "mickeynp/ligature.el" :ref "3d14604"))

;; (elpaca (log4e :repo "aki2o/log4e" :fetcher github :ref "7df0c1f"))

;; (elpaca (org :local-repo "org" :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :ref "f731d45"))

;; (elpaca (refine :repo "Wilfred/refine" :fetcher github :ref "d72fa50"))

;; (elpaca (ace-window :repo "abo-abo/ace-window" :fetcher github :ref "c7cb315"))

;; (elpaca (aggressive-fill-paragraph :fetcher github :repo "davidshepherd7/aggressive-fill-paragraph-mode" :ref "4a620e6"))

;; (elpaca (aggressive-indent :repo "Malabarba/aggressive-indent-mode" :fetcher github :ref "b0ec004"))

;; (elpaca (all-the-icons :repo "domtronn/all-the-icons.el" :fetcher github :ref "be99987"))

;; (elpaca (all-the-icons-completion :repo "iyefrat/all-the-icons-completion" :fetcher github :ref "286e2c0"))

;; (elpaca (anaphora :repo "rolandwalker/anaphora" :fetcher github :ref "3b2da3f"))

;; (elpaca (async :repo "jwiegley/emacs-async" :fetcher github :ref "9a8cd0c"))

;; (elpaca (auth-source-pass :fetcher github :repo "DamienCassou/auth-source-pass" :ref "aa7f171"))

;; (elpaca (auto-capitalize :fetcher github :repo "emacsmirror/auto-capitalize" :ref "0ee14c7"))

;; (elpaca (avy :repo "abo-abo/avy" :fetcher github :ref "e92cb37"))

;; (elpaca (buffer-expose :host github :repo "clemera/buffer-expose" :fetcher github :ref "c4a1c74"))

;; (elpaca (centered-cursor-mode :fetcher github :repo "andre-r/centered-cursor-mode.el" :ref "ebaeb80"))

;; (elpaca (centered-window :fetcher github :repo "anler/centered-window-mode" :old-names (centered-window-mode) :ref "80965f6"))

;; (elpaca (corfu :host github :branch "main" :repo "minad/corfu" :fetcher github :ref "a59c41d"))

;; (elpaca (counsel :repo "abo-abo/swiper" :fetcher github :ref "8f2abd3"))

;; (elpaca (dash :fetcher github :repo "magnars/dash.el" :ref "7a9c937"))

;; (elpaca (dash-functional :fetcher github :repo "magnars/dash.el" :ref "7a9c937"))

;; (elpaca (dashboard :fetcher github :repo "emacs-dashboard/emacs-dashboard" :ref "36c8da4"))

;; (elpaca (decide :fetcher github :repo "lifelike/decide-mode" :ref "668fa55"))

;; (elpaca (default-text-scale :fetcher github :repo "purcell/default-text-scale" :ref "bfc0987"))

;; (elpaca (dirvish :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))

;; (elpaca (dirvish-collapse :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))

;; (elpaca (dirvish-icons :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))

;; (elpaca (dirvish-media :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))

;; (elpaca (dirvish-subtree :fetcher github :repo "alexluigit/dirvish" :ref "ec41006"))

;; (elpaca (doct :repo "progfolio/doct" :fetcher github :ref "15974ad"))

;; (elpaca (edit-indirect :fetcher github :repo "Fanael/edit-indirect" :ref "bdc8f54"))

;; (elpaca (ednc :repo "sinic/ednc" :fetcher github :ref "d1a3c37"))

;; (elpaca (elfeed :repo "skeeto/elfeed" :fetcher github :ref "de4b64b"))

;; (elpaca (elfeed-org :repo "remyhonig/elfeed-org" :fetcher github :ref "77b6bbf"))

;; (elpaca (elfeed-score :fetcher github :repo "sp1ff/elfeed-score" :ref "5fff415"))

;; (elpaca (elisp-demos :fetcher github :repo "xuchunyang/elisp-demos" :ref "ed9578d"))

;; (elpaca (elisp-refs :repo "Wilfred/elisp-refs" :branch "master" :fetcher github :ref "bf3cca8"))

;; (elpaca (ellocate :fetcher github :repo "walseb/ellocate" :ref "8140508"))

;; (elpaca (embark :repo "oantolin/embark" :fetcher github :ref "5d0459d"))

;; (elpaca (emms :fetcher github :url "https://git.savannah.gnu.org/git/emms.git" :repo "emacsmirror/emms" :ref "5c3226b"))

;; (elpaca (eros :fetcher github :repo "xiongtx/eros" :ref "dd89102"))

;; (elpaca (eshell-up :fetcher github :repo "peterwvj/eshell-up" :ref "9c100ba"))

;; (elpaca (eshell-z :fetcher github :repo "xuchunyang/eshell-z" :ref "337cb24"))

;; (elpaca (evil :repo "emacs-evil/evil" :fetcher github :ref "cc9d688"))

;; (elpaca (evil-cleverparens :fetcher github :repo "luxbock/evil-cleverparens" :ref "8c45879"))

;; (elpaca (evil-easymotion :repo "PythonNut/evil-easymotion" :fetcher github :ref "f96c2ed"))

;; (elpaca (evil-goggles :repo "edkolev/evil-goggles" :fetcher github :ref "08a2205"))

;; (elpaca (evil-magit :fetcher github :repo "emacs-evil/evil-magit" :ref "f4a8c8d"))

;; (elpaca (evil-surround :repo "emacs-evil/evil-surround" :fetcher github :old-names (surround) :ref "346d4d8"))

;; (elpaca (expand-region :repo "magnars/expand-region.el" :fetcher github :ref "ea6b4cb"))

;; (elpaca (exwm :branch "master" :host github :repo "ch11ng/exwm" :fetcher github :ref "b62d5e7"))

;; (elpaca (exwm-edit :repo "agzam/exwm-edit" :fetcher github :ref "2fd9426"))

;; (elpaca (exwm-firefox-core :fetcher github :repo "walseb/exwm-firefox-core" :ref "e2fe2a8"))

;; (elpaca (exwm-firefox-evil :fetcher github :repo "walseb/exwm-firefox-evil" :ref "14643ee"))

;; (elpaca (exwm-float :fetcher gitlab :repo "mtekman/exwm-float.el" :ref "eb1b60b"))

;; (elpaca (f :fetcher github :repo "rejeep/f.el" :ref "1814209"))

;; (elpaca (fennel-mode :fetcher sourcehut :repo "technomancy/fennel-mode" :ref "da958db"))

;; (elpaca (figlet :fetcher github :repo "jpkotta/figlet" :ref "19a3878"))

;; (elpaca (frame-cmds :fetcher github :repo "emacsmirror/frame-cmds" :ref "b803354"))

;; (elpaca (frame-fns :fetcher github :repo "emacsmirror/frame-fns" :ref "b675ee5"))

;; (elpaca (gcmh :repo "koral/gcmh" :fetcher gitlab :ref "0089f9c"))

;; (elpaca (git-auto-commit-mode :fetcher github :repo "ryuslash/git-auto-commit-mode" :ref "a6b6e0f"))

;; (elpaca (git-commit :fetcher github :repo "magit/magit" :old-names (git-commit-mode) :ref "86eec7b"))

;; (elpaca (git-gutter+ :fetcher github :repo "nonsequitur/git-gutter-plus" :ref "b772699"))

;; (elpaca (goto-chg :repo "emacs-evil/goto-chg" :fetcher github :ref "2af6121"))

;; (elpaca (grugru :repo "ROCKTAKEY/grugru" :fetcher github :ref "92e588e"))

;; (elpaca (helm :repo "emacs-helm/helm" :fetcher github :ref "8de5444"))

;; (elpaca (helm-core :repo "emacs-helm/helm" :fetcher github :ref "8de5444"))

;; (elpaca (helm-system-packages :repo "emacs-helm/helm-system-packages" :fetcher github :ref "e93f4ae"))

;; (elpaca (helpful :repo "Wilfred/helpful" :branch "master" :fetcher github :ref "6f8991a"))

;; (elpaca (hide-mode-line :repo "hlissner/emacs-hide-mode-line" :fetcher github :ref "8888882"))

;; (elpaca (highlight-quoted :fetcher github :repo "Fanael/highlight-quoted" :ref "2410347"))

;; (elpaca (ht :fetcher github :repo "Wilfred/ht.el" :ref "2850301"))

;; (elpaca (hydra :repo "abo-abo/hydra" :fetcher github :ref "2d55378"))

;; (elpaca (ialign :fetcher github :repo "mkcms/interactive-align" :ref "bc4d30d"))

;; (elpaca (idle-require :fetcher github :repo "nschum/idle-require.el" :ref "33592bb"))

;; (elpaca (iedit :repo "victorhge/iedit" :fetcher github :ref "27c6186"))

;; (elpaca (ivy :repo "abo-abo/swiper" :fetcher github :ref "8f2abd3"))

;; (elpaca (key-chord :fetcher github :repo "emacsorphanage/key-chord" :ref "7f7fd7c"))

;; (elpaca (lispy :repo "abo-abo/lispy" :fetcher github :ref "1ad128b"))

;; (elpaca (lispyville :fetcher github :repo "noctuid/lispyville" :ref "0f13f26"))

;; (elpaca (loopy :fetcher github :repo "okamsn/loopy" :ref "31dc58f"))

;; (elpaca (lv :repo "abo-abo/hydra" :fetcher github :ref "2d55378"))

;; (elpaca (macrostep :fetcher github :repo "joddie/macrostep" :ref "424e373"))

;; (elpaca (magit :fetcher github :repo "magit/magit" :ref "86eec7b"))

;; (elpaca (magit-section :fetcher github :repo "magit/magit" :ref "86eec7b"))

;; (elpaca (map :host github :repo "emacs-straight/map" :fetcher github :ref "dc4f657"))

;; (elpaca (marginalia :repo "minad/marginalia" :fetcher github :ref "b65d66e"))

;; (elpaca (markdown-mode :fetcher github :repo "jrblevin/markdown-mode" :ref "c338cdf"))

;; (elpaca (mini-modeline :repo "kiennq/emacs-mini-modeline" :fetcher github :ref "7dcd0ab"))

;; (elpaca (mmt :repo "mrkkrp/mmt" :fetcher github :ref "d772956"))

;; (elpaca (modus-themes :fetcher github :repo "protesilaos/modus-themes" :ref "38236a9"))

;; (elpaca (notmuch :url "https://git.notmuchmail.org/git/notmuch" :fetcher git :ref "a5f7efd"))

;; (elpaca (orderless :repo "oantolin/orderless" :fetcher github :ref "cbc0109"))

;; (elpaca (org-auto-tangle :repo "yilkalargaw/org-auto-tangle" :fetcher github :ref "2494a6f"))

;; (elpaca (org-ml :repo "ndwarshuis/org-ml" :fetcher github :ref "385e3be"))

;; (elpaca (org-ql :fetcher github :repo "alphapapa/org-ql" :ref "d7ada53"))

;; (elpaca (org-remark :host github :repo "emacs-straight/org-remark" :ref "7e72e86"))

;; (elpaca (org-super-agenda :fetcher github :repo "alphapapa/org-super-agenda" :ref "f5e80e4"))

;; (elpaca (org-superstar :fetcher github :repo "integral-dw/org-superstar-mode" :ref "7f83636"))

;; (elpaca (ov :fetcher github :repo "emacsorphanage/ov" :ref "c5b9aa4"))

;; (elpaca (paredit :fetcher nil :url "https://mumble.net/~campbell/git/paredit.git" :repo "https://mumble.net/~campbell/git/paredit.git" :ref "d0b1a2f"))

;; (elpaca (pass :fetcher github :repo "NicolasPetton/pass" :ref "a095d24"))

;; (elpaca (password-generator :fetcher github :repo "vandrlexay/emacs-password-genarator" :ref "c1da979"))

;; (elpaca (password-store :fetcher github :repo "zx2c4/password-store" :ref "f152064"))

;; (elpaca (password-store-otp :repo "volrath/password-store-otp.el" :fetcher github :ref "04998c8"))

;; (elpaca (pinentry :host github :repo "emacs-straight/pinentry" :fetcher github :ref "cd942f7"))

;; (elpaca (plural :fetcher github :repo "emacsmirror/plural" :ref "b91ce15"))

;; (elpaca (popup :fetcher github :repo "auto-complete/popup-el" :ref "bd5a0df"))

;; (elpaca (popwin :fetcher github :repo "emacsorphanage/popwin" :ref "215d6cb"))

;; (elpaca (rainbow-delimiters :fetcher github :repo "Fanael/rainbow-delimiters" :ref "f43d48a"))

;; (elpaca (redacted :fetcher github :repo "bkaestner/redacted.el" :ref "156311e"))

;; (elpaca (restart-emacs :fetcher github :repo "iqbalansari/restart-emacs" :ref "1607da2"))

;; (elpaca (s :fetcher github :repo "magnars/s.el" :ref "43ba8b5"))

;; (elpaca (search-web :repo "tomoya/search-web.el" :fetcher github :ref "c4ae86a"))

;; (elpaca (shut-up :fetcher github :repo "cask/shut-up" :ref "081d6b0"))

;; (elpaca (smartparens :fetcher github :repo "Fuco1/smartparens" :ref "63695c6"))

;; (elpaca (spell-number :fetcher github :repo "emacsmirror/spell-number" :ref "3ce612d"))

;; (elpaca (super-save :fetcher github :repo "bbatsov/super-save" :ref "886b551"))

;; (elpaca (swiper :repo "abo-abo/swiper" :fetcher github :ref "8f2abd3"))

;; (elpaca (swiper-helm :repo "abo-abo/swiper-helm" :fetcher github :ref "93fb6db"))

;; (elpaca (tempel :repo "minad/tempel" :fetcher github :ref "b4bb703"))

;; (elpaca (transient :fetcher github :repo "magit/transient" :ref "90e640f"))

;; (elpaca (transpose-frame :fetcher github :repo "emacsorphanage/transpose-frame" :ref "12e523d"))

;; (elpaca (treepy :repo "Luis-Henriquez-Perez/treepy.el" :fetcher github :ref "191d84c"))

;; (elpaca (ts :fetcher github :repo "alphapapa/ts.el" :ref "b7ca357"))

;; (elpaca (undo-tree :host github :repo "emacs-straight/undo-tree" :fetcher github :ref "e326c61"))

;; (elpaca (vc-auto-commit :fetcher github :repo "thisirs/vc-auto-commit" :ref "56f4780"))

;; (elpaca (vertico :host github :branch "main" :repo "minad/vertico" :fetcher github :ref "956c81b"
;;                  :files (:defaults "extensions/*")))

;; (elpaca (which-key :repo "justbur/emacs-which-key" :fetcher github :ref "428aedf"))

;; (elpaca (with-editor :fetcher github :repo "magit/with-editor" :ref "139ef39"))

;; (elpaca (with-emacs :fetcher github :repo "twlz0ne/with-emacs.el" :ref "9f99bec"))

;; (elpaca (workgroups2 :repo "pashinin/workgroups2" :fetcher github :ref "c9403c6"))

;; (elpaca (xelb :host github :repo "emacs-straight/xelb" :fetcher github :ref "f5880e6"))

;; (elpaca (xr :host github :repo "emacs-straight/xr" :fetcher github :ref "277c549"))

;; (elpaca (zone-matrix :fetcher github :repo "emacsmirror/zone-matrix" :ref "e1fc8c7"))

;; (elpaca (zone-rainbow :repo "kawabata/zone-rainbow" :fetcher github :ref "2ba4f1a"))

;; (elpaca (zone-sl :repo "kawabata/zone-sl" :fetcher github :ref "7ec22e3"))

;; (elpaca (zoom-frm :fetcher github :repo "emacsmirror/zoom-frm" :ref "59e2fce" ))

;; (elpaca (zoom-window :fetcher github :repo "emacsorphanage/zoom-window" :ref "474ca47"))

;; (elpaca (el-init :fetcher github :repo "HKey/el-init" :ref "" :branch "master"))

;; (elpaca (cape))

;; (elpaca (chezmoi))

;; filladapt

;; burly

(if-let ((queues (reverse elpaca--queues))
	     ((mapc #'elpaca--maybe-reset-queue queues))
	     (incomplete (cl-find 'incomplete queues :key #'elpaca-q<-status)))
    (progn (elpaca-process-queues)
	       (add-hook 'elpaca-after-init-hook #'restart-emacs))
  (run-hooks 'elpaca--post-queues-hook))

(provide 'oo-packages)
