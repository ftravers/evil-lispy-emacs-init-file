;; ============= HYDRAS ===============================
(defhydra hydra-elisp-comma ()
  "
^SEXP MOVEMENT^    ^^               ^^
_[_ to top level   _d_ debug defun  _o_ insert space around   
_j_ next           _e_ sexp/buffer 
_k_ prev           _._ find defn 
_]_ to open paren  _,_ pop back
"
  ("[" (lambda () (interactive) (goto-top-level-sexp) (evil-lispy-state)) nil :exit t)
  ("j" forward-parent-sexp nil)
  ("k" backward-parent-sexp nil)
  ("]" first-open-paren nil :exit t)
  
  ("d" edebug-eval-top-level-form nil :exit t)
  ("e" eval-sexp-or-buffer nil :exit t)
  ("." lispy-goto-symbol "find definition" :exit t)
  ("," xref-pop-marker-stack "pop back" :exit t)

  ("o" insert-space-around-sexp nil)

  ("q" nil "quit" :exit t))
(defhydra hydra-cloj-comma ()
  "
^SEXP MOVEMENT^    ^EVAL^            ^CIDER^            ^SEXP FORMAT^      
_[_ to top level   _l_ load buffer   _'_ CLJ  JackIn    _o_ space around
_j_ next           _e_ top lvl sexp  _\"_ CLJS JackIn
_k_ next           _d_ debug defn    _._ find defn
_]_ to open paren  ^^                _,_ pop back
"
  ("[" (lambda () (interactive) (goto-top-level-sexp) (evil-lispy-state))  nil)
  ("j" forward-parent-sexp nil)
  ("k" backward-parent-sexp nil)
  ("]" first-open-paren nil :exit t)

  ("l" load-buffer-set-ns nil :exit t)
  ("e" cider-eval-defun-at-point nil :exit t)
  ("d" cider-debug-defun-at-point nil :exit t)
  
  ("'" cider-jack-in nil :exit t)
  ("\"" cider-jack-in-cljs nil :exit t)
  ("." cider-find-var "find definition" :exit t)
  ("," xref-pop-marker-stack "pop back" :exit t)

  ("o" insert-space-around-sexp nil)

  ("r" hydra-cloj-comma-r/body "+REPL+" :exit t)
  ("t" hydra-cloj-comma-t/body "+TESTS+" :exit t)
  ("g" hydra-cloj-comma-g/body "+GO+" :exit t)
  
  ("q" nil "quit" :exit t))
(defhydra hydra-cider-test-report-comma ()
  "
^TEST^               ^TEST MOVEMENT^
_r_ re-run test      _k_ prev
_d_ goto definition  _j_ next 
"
  ("r" cider-test-run-test nil)
  ("d" cider-test-jump nil :exit t)

  ("k" cider-test-previous-result nil)
  ("j" cider-test-next-result nil)

  ("q" nil "quit" :exit t))
(defhydra hydra-lisp-spc-s ()
  "
^Symbol^    ^Misc^   
_y_ search  _s_ search  
_k_ up         
_j_ down      
_r_ replace
"
  ("y" isearch-forward-symbol-at-point nil)
  ("k" isearch-repeat-backward nil)
  ("j" isearch-repeat-forward nil)
  ("r" query-replace-symbol-at-point nil :exit t)

  ("s" isearch-forward nil :exit t)

  ("q" isearch-exit "Quit" :exit t))
(defhydra hydra-all-spc-b ()
  "
+------ BUFFERS -----+  +---- WINDOWS ----+
^ Goto  ^ ^ Save  ^ ^ Misc  ^ ^Resize^    ^Misc^      ^FILES^          ^FONT SIZE^
^-------^ ^------^  ^------^  ^--------^  ^---------^ ^--------------^ ^----------^
_k_ prev  _s_ this  _d_ kill  _l_ widen   _p_ swap    _f_ open         _U_ increase
_j_ next  _a_ all   _b_ list  _h_ narrow  _r_ rotate  _w_ write        _u_ decrease
^^        ^^        ^^        _i_ taller  _o_ other   _C-k_ quit emacs
"
  ("k" previous-buffer nil)
  ("j" next-buffer nil)
  ("o" other-window nil)

  ("s" save-buffer nil)
  ("a" (lambda () (interactive) (save-some-buffers t)) nil :exit t)

  ("d" (lambda () (interactive) (kill-this-buffer) (next-buffer)) nil)
  ("b" helm-mini nil :exit t)

  ("l" enlarge-window-horizontally nil)
  ("h" shrink-window-horizontally nil)
  ("i" enlarge-window nil)

  ("p" swap-windows nil)
  ("r" fenton/rotate-window-split nil)

  ("f" helm-find-files nil :exit t)
  ("w" write-file nil :exit t)
  ("C-k" save-buffers-kill-terminal nil :exit t)

  ("U" text-scale-increase nil)
  ("u" text-scale-decrease nil)

  ("q" nil "quit" :exit t :color pink))
(defhydra hydra-elisp-g ()
  "
^Edit File^   ^Edit Pos^  ^Goto^         ^Buffer^
_f_ my fns    _k_ prev    _l_ line       _p_ prev
_h_ my hydras _j_ next    _g_ first line _n_ next
_i_ init      ^^          ^^             _x_ kill
"
  ("f" (lambda () (interactive) (find-file (concat user-emacs-directory "/lisp/my-functions.el"))) nil :exit t)
  ("h" (lambda () (interactive) (find-file (concat user-emacs-directory "/lisp/key-defs-hydras.el"))) nil :exit t)
  ("i" (lambda () (interactive) (find-file (concat user-emacs-directory "/init.el"))) "init" :exit t)

  ("k" goto-last-change nil)
  ("j" goto-last-change-reverse nil)

  ("p" previous-buffer nil)
  ("n" next-buffer nil)
  ("x" (lambda () (interactive) (kill-this-buffer) (next-buffer)) nil)

  ("g" evil-goto-first-line nil :exit t)
  ("l" avy-goto-line nil :exit t)

  ("q" nil "quit" :exit t))
(defhydra hydra-cloj-g ()
  "
^Clojure^       ^Files^    ^Edit Pos^ ^Goto^
_r_ REPL        _f_ fns    _k_ prev   _l_ line
_t_ test        _h_ hydras _j_ next   _g_ first line
_p_ test report _i_ init
_c_ cloj
"
  ("r" cider-switch-to-repl-buffer nil :exit t)
  ("t" toggle-goto-test-impl nil :exit t)
  ("p" cider-test-show-report nil :exit t)
  ("c" toggle-goto-test-impl nil :exit t)

  ("f" (lambda () (interactive) (find-file (concat user-emacs-directory "/lisp/my-functions.el"))) :exit t)
  ("h" (lambda () (interactive) (find-file (concat user-emacs-directory "/lisp/key-defs-hydras.el"))) :exit t)
  ("i" (lambda () (interactive) (find-file (concat user-emacs-directory "/init.el"))) "init" :exit t)

  ("k" goto-last-change nil)
  ("j" goto-last-change-reverse nil)

  ("g" evil-goto-first-line nil :exit t)
  ("l" avy-goto-line nil :exit t)

  ("q" nil "quit" :exit t))
(defhydra hydra-repl-g ()
  "
_g_ prev cloj file 
_r_ prev cloj file 
"
  ("g" cider-switch-to-last-clojure-buffer nil :exit t)
  ("r" cider-switch-to-last-clojure-buffer nil :exit t))
(defhydra hydra-org-comma ()
  "
_j_ next                 _w_ move tree up        _c_ cut
_k_ prev                 _s_ move tree down      _p_ paste
_h_ prev heading         _C-h_ promote subtree   _o_ cycle fOld all
_l_ next visib heading   _C-l_ demote subtree    _x_ cycle fold this 
"
  ("j" org-forward-element nil)
  ("k" org-backward-element nil)
  ("l" org-next-visible-heading nil)
  ("h" org-previous-visible-heading nil)

  ("w" org-move-subtree-up nil)
  ("s" org-move-subtree-down nil)
  ("C-h" org-promote-subtree nil)
  ("C-l" org-demote-subtree nil)

  ("c" org-cut-subtree nil)
  ("p" org-paste-subtree nil)
  ("o" org-shifttab nil)
  ("x" org-cycle nil)

  ("q" nil "quit" :exit t))
(defhydra hydra-repl-comma () ""
  ("k" cider-quit "kill REPL" :exit t)) 
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
(defhydra hydra-cloj-comma-g ()
  "
GOTO
_r_ repl _c_ cloj _t_ test _p_ test report 
"
;_s_ spec 
  ("r" cider-switch-to-repl-buffer nil :exit t)
  ("c" cider-switch-to-last-clojure-buffer nil :exit t)
  ("t" toggle-goto-test-impl nil :exit t)
  ("p" cider-test-show-report nil :exit t)

  ("q" nil "quit" :exit t))
(defhydra hydra-cloj-comma-t ()
  "
^GOTO^           ^TESTS^
_t_ test/impl    _p_ run project
_r_ test report  _n_ run ns

"
  ("t" toggle-goto-test-impl nil :exit t)
  ("r" cider-test-show-report nil :exit t)

  ("p" cider-test-run-project-tests nil :exit t)
  ("n" cider-test-run-ns-tests nil :exit t)

  ("q" nil "quit" :exit t))
(defhydra hydra-any-spc-e ()
  "
EDIT
"
  ("i" (lambda () (interactive) (find-file (concat user-emacs-directory "/init.el"))) "init" :exit t)
  ("f" (lambda () (interactive) (find-file (concat user-emacs-directory "/lisp/my-functions.el"))) "functions" :exit t)
  ("h" (lambda () (interactive) (find-file (concat user-emacs-directory "/lisp/key-defs-hydras.el"))) "hydras" :exit t))
(defhydra hydra-any-spc-f ()
  "
come back here and maybe make it point to spc-b
FILES/BUFFERS
"
  ("f"  "init" :exit t)
  ("f"  "functions" :exit t)
  ("h"  "hydras" :exit t))
;; ============= General: Key Defs  ==============
(setq none-any-all ; prefix-key: none, state: any, keymap: all
      '("(" (lispy-parens-from-normal :wk "enter lispy, insert parens")
        "q" (cider-popup-buffer-quit-function :wk "quit")
        "C-p" (helm-show-kill-ring :wk "show kill ring")
        "ESC" (keyboard-escape-quit :wk "quit")))
(setq none-shared-lisp
      '("                       ;" (lispy-comment :wk "lispy comment")
        "C-u" (universal-argument :wk "universal argument")
        "M-." (lispy-goto-symbol :wk "goto symbol")
        "M-," (pop-tag-mark :wk "pop from symbol")
        "C-n" (evil-scroll-page-down :wk "down")
        "C-k" (lispy-kill-sentence :wk "kill sentence")
        "C-y" (evil-paste-before :wk "paste")
        "C-p" (evil-scroll-page-up :wk "up")
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
         "[" (evil-lispy/enter-state-left :wk "enter lispy mode left")
         "]" (evil-lispy/enter-state-right :wk "enter lispy mode right")
         )
       none-shared-lisp))
(setq spc-kz
      '("" nil
        "b" (hydra-all-spc-b/body :wk ">BUFFERS<")
        "c" (:ignore t :wk "Command Log")
        "e" (hydra-any-spc-e/body :wk ">EDIT<")
        "f" (hydra-all-spc-b/body :wk ">BUFFERS<")
        "g" (:ignore t :wk "Magit")
        "o" (:ignore t :wk "Fold")
        "p" (:ignore t :wk "Projects")
        "q" (:ignore t :wk "Quit")
        "s" (hydra-lisp-spc-s/body :wk "Search/Replace")
        "w" (:ignore t :wk "Window")

        "1" (winum-select-window-1 :wk "move window 1")
        "2" (winum-select-window-2 :wk "move window 2")
        "3" (winum-select-window-3 :wk "move window 3")
        "4" (winum-select-window-4 :wk "move window 4")
        "5" (winum-select-window-5 :wk "move window 5")
        "/" (helm-projectile-ag :wich-key "ag")
        "SPC" (helm-M-x :wk "run command")
        ;; "." (lispy-goto-symbol :wk "find definition")
        ;; "," (xref-pop-marker-stack :wk "pop back")
        ;; "e" (eval-sexp-or-buffer :wk "elisp eval buffer")


        ;; "ff" (helm-find-files :wk "Find Files")
        ;; "fed" (ffs "/home/fenton/.emacs.d/init.el" :wk "open init.el")

        "cc" (command-log-mode :wk "toggle command log mode")
        "cl" (clm/open-command-log-buffer :wk "show command log")

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
        "ws" (swap-windows :wk "swap windows")
        "wr" (fenton/rotate-window-split :wk "rotate window split")))
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

;; edebug-eval-defun <-- with prefix instrument
;; edebug-step-mode,edebug-forward-sexp, edebug-step-in
;; edebug-step-out, edebug-previous-result, edebug-trace-mode
;; edebug-backtrace 

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
               '("g" (hydra-cloj-g/body :wk ""))
               '("," (hydra-cloj-comma/body :wk "comma"))
               none-normal-lisp)) 
(apply 'gdk :keymaps '(emacs-lisp-mode-map)
       :states '(normal visual emacs)
       (append '("" nil)
               '("g" (hydra-elisp-g/body :wk ""))
               '("," (hydra-elisp-comma/body :wk "comma"))
               none-normal-lisp))
(apply 'gdk :keymaps '(org-mode-map)
       :states '(normal visual emacs)
       (append '("" nil)
               '("," (hydra-org-comma/body :wk "comma"))))
(apply 'gdk :keymaps '(cider-test-report-mode-map)
 :states '(normal visual emacs)
 (append
  '("," (hydra-cider-test-report-comma/body :wk "tests"))
  ;; "k" (cider-test-previous-result :wk "prev result")
  ;; "j" (cider-test-next-result :wk "next result")
  ;; "RET" (cider-test-jump :wk "jump to def")
  none-normal-lisp
  '("q" nil
    "q" (cider-popup-buffer-quit-function :wk "quit"))))
(apply 'gdk :keymaps '(cider-repl-mode-map)
       :states '(normal visual emacs)
       (append '("" nil)
               '("C-k" nil) ;; cannot get "C-k" unbound :(
               '("," hydra-repl-comma/body :wk "comma"
                 "g" (hydra-repl-g/body :wk "go")
                 "k" (cider-repl-backward-input :wk "Previous Command")
                 "j" (cider-repl-forward-input :wk "Next Command")
                 "i" (evil-lispy-state :wk "insert -> lispy state"))
               none-any-lisp))
(apply 'gdk :keymaps '(edebug-mode-map)
       :states '(normal visual emacs)
       (append '("" nil
                 "i" edebug-step-in :wk ""
                 "o" edebug-step-out :wk ""

                 )))
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
;; (defhydra hydra-cider-eval ()
;;   "
;; EVAL
;; _b_ buffer
;; _r_ previous sexp and replace 
;; _p_ eval sexp result in comments
;; _q_ quit
;; "
;;   ("b" cider-eval-buffer nil :exit t)
;;   ("r" cider-eval-last-sexp-and-replace nil :exit t)
;;   ("p" eval-sexp-print-in-comment nil :exit t)
;;   ("q" nil nil :exit t))
;; 

;; (defhydra hydra-cider-test-comma ()
;;   "
;; TESTS
;; ^Run^                    ^Navigate Report^    ^Report^
;; ----------------------------------------------------------
;; _p_ run test at point    _k_ prev result      _f_ re-run failed
;; _n_ test all namespace   _j_ next result      _s_ show report
;; _a_ all project tests    _d_ goto definition  _h_ hide auto show report
;; ^^                       _e_ show error
;; "
;;   ("p" cider-test-run-test nil :exit t)
;;   ("n" cider-test-run-ns-tests nil :exit t)
;;   ("a" cider-test-run-project-tests nil :exit t)
;;   ("h" (lambda () (interactive)
;;          (setq cider-auto-test-mode nil))
;;    nil :exit t)

;;   ("k" cider-test-previous-result nil)
;;   ("j" cider-test-next-result nil)
;;   ("d" cider-test-jump :exit t)
;;   ("e" cider-test-stacktrace nil)
  
;;   ("f" cider-test-rerun-failed-tests nil)
;;   ("s" cider-test-show-report nil)
  
;;   ("q" nil "quit" :exit t))
