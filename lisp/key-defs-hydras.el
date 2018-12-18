
;; ============= HYDRAS ===============================
;; ^Registers^  
 ;; _m_ mark pt
 ;; _u_ jump to
 ;; _v_ view
(defhydra hydra-prog-search ()
  "
^Search^    ^Edit Pos^   ^Replace^   ^Quit^ 
_y_ sym@pt  _k_ prev     _r_ sym@pt  _q_ quit
_p_ up      _j_ next     ^Goto^
_n_ down    _s_ search   _l_ line
"
  ("m" point-to-register nil :exit t)
  ("u" jump-to-register nil :exit t)
  ("v" view-register nil :exit t)

  ("k" goto-last-change nil)
  ("j" goto-last-change-reverse nil)
  ("l" avy-goto-line nil :exit t)

  ("b" hydra-buffers/body ">BUFFERS<" :exit t)

  ("y" isearch-forward-symbol-at-point nil)
  ("p" isearch-repeat-backward nil)
  ("n" isearch-repeat-forward nil)
  ("s" isearch-forward nil :exit t)

  ("r" query-replace-symbol-at-point nil :exit t)
  ("q" isearch-exit nil :exit t)) 
(defhydra hydra-buffers ()
  "
 ^Goto^    ^^^       BUFFERS ^^^
^Window^   ^ Goto  ^ ^ Save  ^ ^ Misc  ^  
^------^   ^-------^ ^-------^ ^-------^
  _1_      _k_ prev  _s_ this  _d_ kill
  _2_      _j_ next  _a_ all   _b_ list 
  _3_      ^ ^       ^ ^       _c_ kill (no exit)
  _4_
"
  ("j" next-buffer nil)
  ("k" previous-buffer nil)

  ("s" save-buffer nil)
  ("a" (lambda () (interactive) (save-some-buffers t)) nil :exit t)
  ("b" helm-mini nil :exit t)
  ("d" kill-this-buffer nil :exit t)
  ("c" (lambda () (interactive) (kill-this-buffer) (next-buffer)) nil)

  ("1" winum-select-window-1 nil :exit t)
  ("2" winum-select-window-2 nil :exit t)
  ("3" winum-select-window-3 nil :exit t)
  ("4" winum-select-window-4 nil :exit t)

  ("q" nil "quit" :exit t :color pink))
(defhydra hydra-cider-eval ()
  "
EVAL
_b_ buffer
_r_ previous sexp and replace 
_p_ eval sexp result in comments
_q_ quit
"
  ("b" cider-eval-buffer nil :exit t)
  ("r" cider-eval-last-sexp-and-replace nil :exit t)
  ("p" eval-sexp-print-in-comment nil :exit t)
  ("q" nil nil :exit t))
(defhydra hydra-lisp-movement ()
  "move"
  ("a" beginning-of-defun "begin of defun" :exit t))
(defhydra hydra-elisp-comma ()
  "
^SEXP^
_o_ space around 
_j_ next
_k_ next
_d_ debug
_[_ to open paren
"
  ("o" insert-space-around-sexp nil)
  ("j" forward-parent-sexp nil)
  ("k" backward-parent-sexp nil)
  ("[" first-open-paren nil :exit t)
  ("d" edebug-defun nil :exit t)
  ("q" nil "quit" :exit t)

  )
;; get here by: ', r' from cloj buffer

(defhydra hydra-cloj-comma-r ()
  "
^REPL^         ^SEXP^  
_r_ goto repl  _l_ insert last       
_n_ set ns     _p_ eval, print
_k_ kill
"
  ("r" cider-switch-to-repl-buffer nil :exit t)
  ("n" repl-reload-ns nil :exit t)
  ("k" cider-quit nil :exit t)

  ("l" cider-insert-last-sexp-in-repl nil :exit t)
  ("p" eval-sexp-print-in-comment nil :exit t)

  ("q" nil "quit" :exit t))
(defhydra hydra-cider-test-comma ()
  "
TESTS
^Run^                    ^Navigate Report^    ^Report^
----------------------------------------------------------
_p_ run test at point    _k_ prev result      _f_ re-run failed
_n_ test all namespace   _j_ next result      _s_ show report
_a_ all project tests    _d_ goto definition  _h_ hide auto show report
^^                       _e_ show error
"
  ("p" cider-test-run-test nil :exit t)
  ("n" cider-test-run-ns-tests nil :exit t)
  ("a" cider-test-run-project-tests nil :exit t)
  ("h" (lambda () (interactive)
         (setq cider-auto-test-mode nil))
   nil :exit t)

  ("k" cider-test-previous-result nil)
  ("j" cider-test-next-result nil)
  ("d" cider-test-jump :exit t)
  ("e" cider-test-stacktrace nil)
  
  ("f" cider-test-rerun-failed-tests nil)
  ("s" cider-test-show-report nil)
  
  ("q" nil "quit" :exit t))
(defhydra hydra-cloj-comma-t ()
  "
^FILES^         ^TESTS^
_t_ goto tests  _p_ run project
_i_ goto impl   _n_ run ns

_r_ goto test reporr
"
  ("t" open-test-file nil :exit t)
  ("i" open-implementation-file nil :exit t)
  ("p" cider-test-run-project-tests nil :exit t)
  ("n" cider-test-run-ns-tests nil :exit t)
  ("r" cider-test-show-report nil :exit t)
  )

;; get here when hit ', r' from REPL 
(defhydra hydra-clojure-comma ()
  "
^CIDER^          ^SEXP^            
_'_ CLJ  JackIn  _o_ space around  
_\"_ CLJS JackIn  _j_ next
_d_ debug defun  _k_ next          
_l_ load buffer  _[_ to open paren 
"
  ("'" cider-jack-in nil :exit t)
  ("\"" cider-jack-in-cljs nil :exit t)
  ("d" cider-debug-defun-at-point nil :exit t)
  ("l" load-buffer-set-ns nil :exit t)
  
  ("o" insert-space-around-sexp nil)
  ("j" forward-parent-sexp nil)
  ("k" backward-parent-sexp nil)
  ("[" first-open-paren nil :exit t)

  ("i" cider-insert-last-sexp-in-repl nil :exit t)
  ("r" hydra-cloj-comma-r/body "+REPL+" :exit t)
  ("t" hydra-cloj-comma-t/body "+TESTS+" :exit t)

  ("q" nil "quit" :exit t))
(defhydra hydra-repl-comma ()
  "
_r_ goto cloj buffer
"
  ("r" cider-switch-to-last-clojure-buffer nil :exit t)
  ("q" nil "quit" :exit t))

;; ============= General: Key Defs  ==============
(setq none-any-all
      '("[" (evil-lispy/enter-state-left :wk "enter lispy mode left")
        "]" (evil-lispy/enter-state-right :wk "enter lispy mode right")
        "(" (lispy-parens-from-normal :wk "enter lispy, insert parens")
        "q" (cider-popup-buffer-quit-function :wk "quit")
        "C-p" (helm-show-kill-ring :wk "show kill ring")
        "ESC" (keyboard-escape-quit :wk "quit")))
(setq none-shared-lisp
      '(";" (lispy-comment :wk "lispy comment")
        ;; "," (hydra-lisp-comma/body :wk ">>>Movement<<<")
        "C-u" (universal-argument :wk "universal argument")
        "M-." (lispy-goto-symbol :wk "goto symbol")
        "M-," (pop-tag-mark :wk "pop from symbol")
        "C-n" (evil-scroll-page-down :wk "down")
        "C-k" (lispy-kill-sentence :wk "kill sentence")
        "C-p" (evil-scroll-page-up :wk "up")
        ;; "C-]" (end-of-parent-sexp :wk "end of sexp")
        ;; "C-[" (first-open-paren :wk "beginning of defun")
        "q" (self-insert-command :wk "self insert")))
(setq none-any-lisp none-shared-lisp)
(setq none-normal-lisp
      (append
       '("i" (evil-lispy-state :wk "insert -> lispy state")
         ;; "I" (I-lispy :wk "insert line -> lispy state")
         "o" (o-lispy :wk "open below -> lispy state")
         "O" (O-lispy :wk "open above -> lispy state")
         "a" (a-lispy :wk "append -> lispy state")
         "A" (A-lispy :wk "append line -> lispy state")
         )
       none-shared-lisp))
(setq spc-kz
      '("" nil
        "f" (:ignore t :wk "Files")
        "b" (hydra-buffers/body :wk ">BUFFERS<")
        "g" (:ignore t :wk "Magit")
        "o" (:ignore t :wk "Fold")
        "p" (:ignore t :wk "Projects")
        "s" (hydra-prog-search/body :wk "Search")
        "w" (:ignore t :wk "Window")
        "q" (:ignore t :wk "Quit")

        "1" (winum-select-window-1 :wk "move window 1")
        "2" (winum-select-window-2 :wk "move window 2")
        "3" (winum-select-window-3 :wk "move window 3")
        "4" (winum-select-window-4 :wk "move window 4")
        "5" (winum-select-window-5 :wk "move window 5")
        "/" (helm-projectile-ag :wich-key "ag")
        "SPC" (helm-M-x :wk "run command")
        "." (lispy-goto-symbol :wk "find definition")
        "," (xref-pop-marker-stack :wk "pop back")
        "e" (eval-sexp-or-buffer :wk "elisp eval buffer")

        "ff" (helm-find-files :wk "Find Files")
        ;; "fed" (ffs "/home/fenton/.emacs.d/init.el" :wk "open init.el")

        "gs" (magit-status :wk "magit status")
        "gb" (:ignore t :wk "Magit Blame")
        "gbb" (magit-blame :wk "magit blame")
        "gbq" (magit-blame-quit :wk "magit blame quit")

        "os" (hs-show-all :wk "show all")
        "oh" (hs-hide-all :wk "hide all")
        "oo" (hs-toggle-hiding :wk "toggle folding")

        "pp" (helm-projectile-switch-project :wk "switch to project")
        "pf" (helm-projectile-find-file :wk "find file")
        "ps" (helm-projectile-ag :wk "find git project file")

        "qq" (save-buffers-kill-terminal :wk "Emacs Quit")

        "wd" (delete-window-balance :wk "delete window")
        "w0" (delete-window-balance :wk "delete window")
        "wm" (delete-other-windows :wk "maximize window")
        "w1" (delete-other-windows :wk "maximize window")
        "wv" (split-window-vertical-balance :wk "split vertically")
        "w2" (split-window-vertical-balance :wk "split vertically")
        "w-" (split-window-below-balance :wk "split horizontally")
        "w3" (split-window-below-balance :wk "split horizontally")
        "w=" (balance-windows :wk "balance windows")
        "wt" (transpose-windows :wk "transpose windows")))
(setq comma-cloj-common
      '("." (cider-find-var :wk "find var")
        "," (cider-pop-back :wk "popback from var")
        "c" (hydra-cljr-help-menu/body :wk "CLOJURE REFACTOR")
        "e" (hydra-cider-eval/body :wk "CIDER EVAL")
        "r" (:ignore t :wk "REPL**")
        "t" (hydra-cider-test/body :wk "TEST")
        "rq" (cider-quit :wk "REPL quit")
        "rs" (cider-jack-in-cljs :wk "Make cljscript REPL")))
(setq comma-cloj
      (append
       '("jr" (cider-switch-to-repl-buffer :wk "goto REPL"))
       comma-cloj-common))
(setq comma-cloj-repl
      (append
       '("jr" (:wk "goto REPL")
         "jr" (cider-switch-to-last-clojure-buffer :wk "goto REPL"))
       comma-cloj-common))
;; (setq cider-test-report-keys
;;       '("k" cider-test-previous-result)
;;       '("j" cider-test-next-result nil)
;;       '("d" cider-test-jump :exit t))

(fset 'gdk 'general-define-key)

;; ==> None.....None/All
(apply 'gdk :keymaps '(emacs-lisp-mode-map)
       (append '("" nil)
               '("e" (e-lisp :wk "elisp eval"))
               none-any-lisp))
(apply 'gdk :keymaps '(clojure-mode-map)
       (append '("" nil)
               '("e" (e-clojure :wk "cider eval"))
               none-any-lisp))

;; ==> None.....Normal
(apply 'gdk :states ; :keymaps: None/All
       '(normal visual emacs)
       none-any-all)
(apply 'gdk :keymaps '(clojure-mode-map)
       :states '(normal visual emacs)
       (append '("" nil)
               '("," (hydra-clojure-comma/body :wk "comma"))
               none-normal-lisp)) 
(apply 'gdk :keymaps '(cider-test-report-mode-map)
       :states '(normal visual emacs)
       (append '("," (hydra-cider-test-comma/body :wk "tests"))))
(apply 'gdk :keymaps '(emacs-lisp-mode-map)
       :states '(normal visual emacs)
       (append '("" nil)
               '("," (hydra-elisp-comma/body :wk "comma"))
               none-normal-lisp))
(apply 'gdk :keymaps '(cider-repl-mode-map)
       :states '(normal visual emacs)
       (append '("" nil)
               '("C-k" nil) ;; cannot get "C-k" unbound :(
               '("," hydra-repl-comma/body :wk "comma"
                 "k" (cider-repl-backward-input :wk "Previous Command")
                 "j" (cider-repl-forward-input :wk "Next Command")
                 "i" (evil-lispy-state :wk "insert -> lispy state"))
               none-any-lisp))

;; ==> SPC......Normal
(apply 'gdk :prefix "SPC" ; :keymaps: None/All
       :states '(normal visual emacs motion)
       spc-kz)
(apply 'gdk :prefix "SPC" ; obscure keymaps
       :keymaps
       '(magit-status-mode-map
         magit-revision-mode-map
         ielm-map
         help-mode-map
         magit-log-mode-map)
       :states '(normal visual emacs)
       spc-kz)

;; ==> OTHER??
(general-def org-mode-map "C-'" 'org-edit-special)
(general-def org-src-mode-map "C-'" 'org-edit-src-abort)

;; =======================================================
(eval-after-load "lispy"
  `(progn
     (my-remove-lispy-key (kbd "C-,"))
     (my-remove-lispy-key (kbd "C-j"))
     (my-remove-lispy-key (kbd "d"))
     (my-remove-lispy-key (kbd "e"))
     (lispy-define-key lispy-mode-map (kbd "d") 'lispy-kill-at-point)
     (lispy-define-key lispy-mode-map (kbd "x") 'collapse-expand)
     (lispy-define-key lispy-mode-map (kbd "y") 'special-lispy-new-copy)
     (lispy-define-key lispy-mode-map (kbd "p") 'special-lispy-paste)
     (lispy-define-key lispy-mode-map (kbd "f") 'special-lispy-flow)
     (lispy-define-key lispy-mode-map (kbd "i") 'special-lispy-tab)
     (lispy-define-key lispy-mode-map (kbd ":") 'evil-ex)
     (lispy-define-key lispy-mode-map (kbd "\"") 'evil-ex)
     ))

;; ==> None.....Input
;; (apply 'gdk :keymaps '(cider-repl-mode-map)
;;        :states '(input)
;;        (append '("" nil)
;;                '("C-k" nil) ;; cannot get "C-k" unbound :(
;;                '("," hydra-repl-comma/body :wk "comma"
;;                  "C-k" (cider-repl-backward-input :wk "Previous Command")
;;                  "C-j" (cider-repl-forward-input :wk "Next Command"))
;;                none-any-lisp))

;; ==> Comma....Normal
;; (apply 'gdk :prefix "," :keymaps '(clojure-mode-map)
;;        :states '(normal visual emacs)
;;        (append '("" nil) comma-cloj))
