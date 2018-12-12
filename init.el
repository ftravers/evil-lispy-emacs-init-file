(set-face-attribute 'default nil :height 140 :family "DejaVu Sans Mono") 

;; interesting page:
;; https://github.com/emacs-evil/evil-collection/issues/116
;; ============= package archives ========
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; ============== use package =============
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents) 
      (package-install 'use-package)))
(require 'use-package)
(setq use-package-always-ensure t)    ;; download packages if not already downloaded

;; ============== favorite packages ==================
(use-package clojure-mode               ; clojure more
  :mode ("\\.clj\\'"))
(use-package evil)                      ; vi like key bindings
(use-package projectile)                ; navigate git projects
(use-package diminish)                  ; reduce mode-line clutter
(use-package delight)                   ; change how modes appear in mode-line
(use-package which-key                  ; show which keys you can press next
  :diminish which-key-mode)             
(use-package lispy)                     ; structural lisp editing
(use-package evil-lispy)                ; vi bindings for lispy
(use-package helm-projectile            ; auto-complete commands
  :diminish helm-mode)                  
(use-package helm-ag)			; silver searcher for projects
(use-package winum)                     ; switch between buffers using numbers
(use-package magit) 			; git integration

(use-package evil-magit)                ; vi bindings for magit
(use-package helm-ag)                   ; silver searcher
(use-package highlight-parentheses)     ; rainbow parens
(use-package company)                   ; completion 
(use-package buffer-move)               ; move buffers 
(use-package el-get)                    ; package management
(use-package cider)                     ; clojure debugger
(use-package general                    ; key binding framework
  :config (general-evil-setup t)) 
(use-package goto-chg)                  ; goto last edit
(use-package repeat)                    ; make repeatable commands
(use-package hydra)                     ; hydra menus
(use-package clomacs)                   ; call clojure from elisp and vice-versa
(require 'clomacs)
(use-package ivy)
(require 'thingatpt)
;; (use-package cl)

;; ============= Simple Config ====================
(evil-mode 1)
(load-theme 'wombat t)                  ; color theme
(which-key-mode)
(winum-mode)
(global-company-mode)
(helm-mode 1)
(setq-default truncate-lines t) 	; dont wrap long lines
(setq-default indent-tabs-mode nil)     ; always replace tabs with spaces
(setq inhibit-startup-message t
      initial-scratch-message nil ; Don't insert instructions in the *scratch* buffer
      x-select-enable-clipboard t ; set cut/copy to go to system clipboard
      scroll-step 1)              ; Scroll one line (not half a page) when moving past the bottom of the window
(mouse-wheel-mode t)                    ; mouse scrolling
(line-number-mode 1)                    ; Show line-number and column-number in the mode line
(column-number-mode 1)                  ; Show line-number and column-number in the mode line
(fset 'yes-or-no-p 'y-or-n-p)           ; When emacs asks for "yes" or "no", let "y" or "n" sufficide
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(highlight-parentheses-mode 1)
(save-place-mode)
(show-paren-mode 1)
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))
(projectile-global-mode)
(helm-projectile-on)
(setq help-window-select t
      projectile-completion-system 'helm
      projectile-switch-project-action 'helm-projectile
      cider-font-lock-dynamically '(macro core function var)
      cider-pprint-fn "clojure.pprint/pprint"
      cider-repl-pop-to-buffer-on-connect nil
      cider-eldoc-display-context-dependent-info t
      cider-overlays-use-font-lock t
      cider-default-cljs-repl 'figwheel
      eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit
      cider-eldoc-display-context-dependent-info t
      cider-overlays-use-font-lock t
      cider-prompt-for-symbol nil
      cider-auto-select-test-report-buffer nil
      cider-test-show-report-on-success nil
      cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"
      show-paren-delay 0) 
;; (cider-auto-test-mode 1)
(set-face-background 'show-paren-match "#640000")
(delight '((helm-mode) (emacs-lisp-mode)))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(diminish 'auto-revert-mode)
(diminish 'projectile-mode)
(diminish 'helm-mode)
(diminish 'lispy-mode)
(setq hl-paren-colors
      '("red" "green1" "orange1" "cyan1" "yellow1" 
        "slateblue1" "magenta1" "purple")
      hl-paren-background-colors
      '("gray14" "gray14" "gray14" "gray14" "gray14" "gray14" "gray14" "gray14"))
(setq desktop-dirname             (expand-file-name  "~/.emacs.d/")
      desktop-auto-save-timeout   30
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-files-not-to-save   "^$" )
(desktop-save-mode 1)

;; ============= Custom Functions ===================
;; (defun in-l()
;;   (interactive)
;;   (if (evil-lispy-state-p)
;;       (message "in lispy state")
;;     (message "NOT in lispy state")))

(defun in-lispy ()
  (interactive)
  (if (evil-lispy-state-p)
      (hydra-buffer-menu/body)
    (self-insert-command)))

(defun collapse-expand ()
  (interactive)
  (hs-toggle-hiding)
  (beginning-of-line))

(defun my-remove-lispy-key (key)
  (define-key lispy-mode-map-base key nil)
  (define-key lispy-mode-map-lispy key nil)
  (define-key lispy-mode-map-oleh key nil)
  (define-key lispy-mode-map-paredit key nil)
  (define-key lispy-mode-map-special key nil))

(defun split-window-vertical-balance ()
  (interactive)
  (split-window-right)
  (balance-windows))

(defun delete-window-balance ()
  (interactive)
  (delete-window)
  (balance-windows))

(defun split-window-below-balance ()
  (interactive)
  (split-window-below)
  (balance-windows))

(defun transpose-windows ()
  "Transpose two windows.  If more or less than two windows are visible, error."
  (interactive)
  (unless (= 2 (count-windows))
    (error "There are not 2 windows."))
  (let* ((windows (window-list))
         (w1 (car windows))
         (w2 (nth 1 windows))
         (w1b (window-buffer w1))
         (w2b (window-buffer w2)))
    (set-window-buffer w1 w2b)
    (set-window-buffer w2 w1b)))

;; (defun lispy-append ()
;;   (interactive)
;;   (forward-char)
;;   (evil-lispy-state))

(defun lispy-parens-from-normal ()
  (interactive)
  (evil-lispy-state)
  (lispy-parens))

(defun load-ns-goto-repl ()
  "Since cider-load-buffer is already an interactive function
must call with: call-interactively in order to not exit after
function call."
  (interactive)
  (call-interactively #'cider-load-buffer)
  (call-interactively #'cider-repl-set-ns)
  (call-interactively #'cider-switch-to-repl-buffer))

(defun o-lispy ()
  (interactive)
  (evil-open-below 1)
  (call-interactively #'evil-lispy-state))

(defun O-lispy ()
  (interactive)
  (evil-open-above 1)
  (call-interactively #'evil-lispy-state))

(defun a-lispy ()
  (interactive)
  (evil-append 1)
  (call-interactively #'evil-lispy-state))

(defun A-lispy ()
  (interactive)
  (evil-append-line 1)
  (call-interactively #'evil-lispy-state))

(defun I-lispy ()
  (interactive)
  (evil-insert-line 1)
  (call-interactively #'evil-lispy-state))

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))

(defun eval-sexp-print-in-comment ()
  (interactive)
  (cider-eval-print-last-sexp)
  (beginning-of-line)
  (insert ";; =>  "))

;; (clomacs-defun adder (+ 1 1))
;; (message (adder))
(defun lispy-right-p ()
  "Return t if after lispy-right character."
  (looking-back "[])}]"
                (line-beginning-position)))

(defun lispy-left-p ()
  "Return t if on lispy-left character."
  (looking-at "[([{]"))

(defun in-special-p ()
  (and (evil-lispy-state-p)
       (or (lispy-right-p) (lispy-left-p))))

(defun e-clojure ()
  "call cider-eval-last-sexp when in special position"
  (interactive)
  (if (in-special-p)
      (cider-eval-last-sexp)
    (self-insert-command 1)))

(defun e-lisp ()
  "call eval-last-sexp if in special position"
  (interactive)
  (if (in-special-p)
      (call-interactively #'eval-last-sexp)
    (self-insert-command 1)))

(defun query-replace-symbol-at-point (inp)
  "jumps to top of file to start the replacing...eeek, dont like
that."
  (interactive
   (let ((symb-at-pt (thing-at-point 'symbol)))
     (list (read-string (format "Replace %s with: " symb-at-pt) nil 'my-history))))
  (let ((symb-at-pt (thing-at-point 'symbol)))
    (query-replace symb-at-pt inp t (line-beginning-position)  (point-max))))

(defun forward-search-symbol-at-point ()
  "search forward the symbol at point"
  (interactive)
  (let ((symb-at-pt (thing-at-point 'symbol)))
    (search-forward symb-at-pt)))

(defun end-of-parent-sexp ()
  (interactive)
  (evil-lispy/enter-state-left)
  (special-lispy-beginning-of-defun)
  (o-lispy))
(defun first-open-paren ()
  (interactive)
  (search-forward "("))
(defun repl-reload-ns ()
  "Thin wrapper around `cider-test-run-tests'."
  (interactive)
  (when (cider-connected-p)
    (let ((cider-auto-select-test-report-buffer nil)
          (cider-test-show-report-on-success nil))
      (cider-repl-set-ns
       (cider-current-ns))
      (cider-ns-reload))))
;; ============= Hooks ==============================

;; ============= HOOKS ==============================
(add-hook 'prog-mode-hook
          (lambda ()
            (highlight-parentheses-mode 1)
            (hs-minor-mode)
            (hs-hide-all)))
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (highlight-parentheses-mode 1)
            (company-mode)))
(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode 1)))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)
(add-hook 'cider-mode-hook
          (lambda ()
            (highlight-parentheses-mode 1)
            (company-mode)
            (cider-company-enable-fuzzy-completion)
            (eldoc-mode)))
 
;; ============= HYDRAS ===============================
(defhydra hydra-prog-search ()
  "
^Registers^ |  ^Jump^  |  ^Search^    |  ^Replace^   |  ^Quit^ 
----------|--------|------------|------------|-------
_m_ mark pt   _k_ prev   _s_ symb @pt   _r_ symb @pt   _q_ quit
_u_ jump to   _j_ next   _j_ up 
_v_ view      _l_ line   _k_ down
"
  ("k" goto-last-change nil)
  ("j" goto-last-change-reverse nil)

  ("b" hydra-buffers/body ">BUFFERS<" :exit t)

  ("m" point-to-register nil :exit t)
  ("u" jump-to-register nil :exit t)
  ("v" view-register nil :exit t)

  ("l" avy-goto-line nil :exit t)
  ("s" isearch-forward-symbol-at-point nil)
  ("j" isearch-repeat-forward nil)
  ("k" isearch-repeat-backward nil)
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
(defhydra hydra-cider-test ()
  "
TESTS
^Run^                    ^Navigate Report^    ^Report^
----------------------------------------------------------
_p_ run test at point    _k_ prev result      _f_ re-run failed
_n_ test all namespace   _j_ next result      _s_ show report
_a_ all project tests    _d_ goto definition  _h_ hide auto show report
_q_ quit                 _e_ show error
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
  
  ("q" nil nil :exit t))
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
  "_a_ begin of defun"
  ("a" beginning-of-defun nil :exit t))
;; ============= General: Key Defs  ==============
(setq comma-cloj-common
      '("'" (cider-jack-in :wk "Cider Jack In")
        "\"" (cider-jack-in-cljs :wk "Cider Jack In CLJS")
        "." (cider-find-var :wk "find var")
        "," (cider-pop-back :wk "popback from var")
        "c" (hydra-cljr-help-menu/body :wk "CLOJURE REFACTOR")
        "e" (hydra-cider-eval/body :wk "CIDER EVAL")
        "r" (:ignore t :wk "REPL**")
        "t" (hydra-cider-test/body :wk "TEST")

        "rb" (cider-jack-in-clj&cljs :wk "Cider Jack In CLJ & CLJS")
        "rc" (cider-jack-in :wk "Make clj REPL")
        "rd" (cider-debug-defun-at-point :wk "instrument fun at point for debugging")
        "ri" (cider-insert-last-sexp-in-repl :wk "insert sexp into repl")
        "rl" (cider-load-buffer :wk "Load Buffer")
        "rn" (repl-reload-ns :wk "reload repl & run tests")
        "rp" (eval-sexp-print-in-comment :wk "print result of prev sexp")
        "rq" (cider-quit :wk "REPL quit")
        "rs" (cider-jack-in-cljs :wk "Make cljscript REPL")))
(setq none-lisp
      (append
       '("i" (evil-lispy-state :wk "insert -> lispy state")
         ;; "I" (I-lispy :wk "insert line -> lispy state")
         "o" (o-lispy :wk "open below -> lispy state")
         "O" (O-lispy :wk "open above -> lispy state")
         "a" (a-lispy :wk "append -> lispy state")
         "A" (A-lispy :wk "append line -> lispy state")
         ";" (lispy-comment :wk "lispy comment")
         ;; "," (hydra-lisp-movement/body :wk ">>>Movement<<<")
         "C-u" (universal-argument :wk "universal argument")
         "M-." (lispy-goto-symbol :wk "goto symbol")
         "M-," (pop-tag-mark :wk "pop from symbol")
         "C-n" (evil-scroll-page-down :wk "down")
         "C-k" (lispy-kill-sentence :wk "kill sentence")
         "C-p" (evil-scroll-page-up :wk "up")
         ;; "C-]" (end-of-parent-sexp :wk "end of sexp")
         ;; "C-[" (beginning-of-defun :wk "beginning of defun")
         "q" (self-insert-command :wk "self insert"))))
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
        "e" (eval-buffer :wk "elisp eval buffer")

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
(setq comma-cloj
      (append
       '("jr" (cider-switch-to-repl-buffer :wk "goto REPL"))
       comma-cloj-common))
(setq comma-cloj-repl
      (append
       '("jr" (:wk "goto REPL")
         "jr" (cider-switch-to-last-clojure-buffer :wk "goto REPL"))
       comma-cloj-common))
(setq cider-test-report-keys
      '(
        "k" cider-test-previous-result)
      ;; ("j" cider-test-next-result nil)
  ;; ("d" cider-test-jump :exit t)
      )

(fset 'gdk 'general-define-key)
;; ==> Prefix: None.....State: None/All
(apply 'gdk :keymaps '(emacs-lisp-mode-map)
       (append '("" nil)
               '("e" (e-lisp :wk "elisp eval"))))
(apply 'gdk :keymaps '(clojure-mode-map)
       (append '("" nil)
               '("e" (e-clojure :wk "cider eval"))))
;; ==> Prefix: None.....State: None/All
(gdk :states ; :keymaps: None/All
 '(normal visual emacs)
 "[" '(evil-lispy/enter-state-left :wk "enter lispy mode left")
 "]" '(evil-lispy/enter-state-right :wk "enter lispy mode right")
 "(" '(lispy-parens-from-normal :wk "enter lispy, insert parens")
 "q" '(cider-popup-buffer-quit-function :wk "quit")
 "C-p" '(helm-show-kill-ring :wk "show kill ring")
 "ESC" '(keyboard-escape-quit :wk "quit"))
;; ==> Prefix: None.....State: Normal
(apply 'gdk :keymaps '(emacs-lisp-mode-map clojure-mode-map)
       :states '(normal visual emacs)
       (append '("" nil) none-lisp))
;; ==> Prefix: None.....State: Normal & Input
(apply 'gdk :keymaps '(cider-repl-mode-map)
       :states '(normal visual emacs input)
       (append '("" nil)
               '("C-k" nil) ;; cannot get "C-k" unbound :(
               '("C-k" (cider-repl-backward-input :wk "Previous Command")
                 "C-j" (cider-repl-forward-input :wk "Next Command"))
               none-lisp))  
(general-def org-mode-map "C-'" 'org-edit-special)
(general-def org-src-mode-map "C-'" 'org-edit-src-abort)
;; ==> Prefix: SPC......State: Normal
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
;; ==> Prefix: COMMA....State: Normal
(apply 'gdk :prefix "," :keymaps '(clojure-mode-map cider-test-report-mode-map)
       :states '(normal visual emacs)
       (append '("" nil) comma-cloj))


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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cider-eldoc sr-speedbar cider winum wk use-package magit lispy highlight-parentheses helm-projectile helm-ag evil el-get diminish delight company clojure-mode buffer-move))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
