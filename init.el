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
      cider-eldoc-display-context-dependent-info t
      cider-overlays-use-font-lock t
      cider-default-cljs-repl 'figwheel
      cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"
      show-paren-delay 0) 
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
(defun e-clojure ()
  "call cider-eval-last-sexp when in special position"
  (interactive)
  (if (evil-lispy-state-p)
      (cider-eval-last-sexp)
    (self-insert-command 1)))

(defun lispy-right-p ()
  "Return t if after lispy-right character."
  (looking-back "[])}]"
                (line-beginning-position)))

(defun lispy-left-p ()
  "Return t if on lispy-left character."
  (looking-at "[([{]"))

(defun e-lisp ()
  "call eval-last-sexp if in special position"
  (interactive)
  (if (and (evil-lispy-state-p)
           (or (lispy-right-p) (lispy-left-p)))
      (call-interactively #'eval-last-sexp)
    (self-insert-command 1)))

;; ============= Hooks ==============================

;; ============= HOOKS ==============================
(add-hook 'prog-mode-hook
          (lambda ()
            (highlight-parentheses-mode 1)
            (hs-minor-mode)
            (hs-hide-all)))
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (highlight-parentheses-mode 1)))
(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode 1)))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

;; ============= HYDRAS ===============================
(defhydra hydra-jump ()
  "
^Registers^    ^Edit Position^     ^Jump^
-------------------------------------------
_m_ mark point _k_: previous       _l_ to line
_u_ jump to    _j_: next
_v_ view      
"
  ("k" goto-last-change nil)
  ("j" goto-last-change-reverse nil)

  ("b" hydra-buffers/body ">BUFFERS<" :exit t)

  ("m" point-to-register nil :exit t)
  ("u" jump-to-register nil :exit t)
  ("v" view-register nil :exit t)

  ("l" avy-goto-line nil :exit t)

  ("q" nil "quit" :exit t :color pink))
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
_p_ run test at point
_n_ test all namespace
_a_ all project tests
"
  ("p" cider-test-run-test nil :exit t)
  ("n" cider-test-run-ns-tests nil :exit t)
  ("a" cider-test-run-project-tests nil :exit t))
(defhydra hydra-cider-eval ()
  "
EVAL
_b_ buffer
_r_ previous sexp and replace 
_p_ eval sexp result in comments
"
  ("b" cider-eval-buffer nil :exit t)
  ("r" cider-eval-last-sexp-and-replace nil :exit t)
  ("p" eval-sexp-print-in-comment nil :exit t))
(defhydra hydra-cider-test-report ()
  "
TESTS
_k_ prev result
_j_ next result
_d_ goto definition
"
  ("k" cider-test-previous-result nil)
  ("j" cider-test-next-result nil)
  ("d" cider-test-jump :exit t)
  )
;; ============= GENERAL ==============================

(setq normal-keys
      '("f" (:ignore t :wk "Files")
        "b" (hydra-buffers/body :wk ">BUFFERS<")
        "g" (:ignore t :wk "Magit")
        "i" (:ignore t :wk "Fill")
        "j" (hydra-jump/body :wk ">JUMP<")
        "o" (:ignore t :wk "Fold")
        "p" (:ignore t :wk "Projects")
        "s" (:ignore t :wk "Search")
        "w" (:ignore t :wk "Window")
        "q" (:ignore t :wk "Quit")

        "1" (winum-select-window-1 :wk "move window 1")
        "2" (winum-select-window-2 :wk "move window 2")
        "3" (winum-select-window-3 :wk "move window 3")
        "4" (winum-select-window-4 :wk "move window 4")
        "5" (winum-select-window-5 :wk "move window 5")
        "/" (helm-projectile-ag :wich-key "ag")
        "SPC" (helm-M-x :wk "run command")
        "." (xref-find-definitions :wk "find definition")
        "," (xref-pop-marker-stack :wk "pop back")
        "e" (eval-buffer :wk "elisp eval buffer")

        ;; "bb" (helm-mini :wk "buffer list")
        ;; "bd" (kill-this-buffer :wk "kill buffer")
        ;; "bt" (transpose-windows :wk "transpose windows")
        ;; "bs" (:ignore t :wk "Buffer Save")
        ;; "bss" (save-buffer :wk "save this buffer")
        ;; "bsa" (save-some-buffers :wk "save all buffers")

        "ff" (helm-find-files :wk "Find Files")
        "fed" (ffs "/home/fenton/.emacs.d/init.el" :wk "open init.el")

        "gs" (magit-status :wk "magit status")
        "gb" (:ignore t :wk "Magit Blame")
        "gbb" (magit-blame :wk "magit blame")
        "gbq" (magit-blame-quit :wk "magit blame quit")

        "ip" (fill-paragraph :wk "fill paragraph")

        "os" (hs-show-all :wk "show all")
        "oh" (hs-hide-all :wk "hide all")
        "oo" (hs-toggle-hiding :wk "toggle folding")

        "pp" (helm-projectile-switch-project :wk "switch to project")
        "pf" (helm-projectile-find-file :wk "find file")
        "ps" (helm-projectile-ag :wk "find git project file")

        "qq" (save-buffers-kill-terminal :wk "Emacs Quit")

        "rm" (point-to-register :wk "set current point to register")
        "rj" (jump-to-register :wk "jump to register")
        "rv" (view-register :wk "view registers")
        "rn" (goto-last-change :wk "goto last change in buffer")
        "rp" (goto-last-change-reverse :wk "goto last change in buffer - reverse")
        
        "sr" (query-replace :wk "query replace")

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
(setq cider-common-keys
      '("'" (cider-jack-in :wk "Cider Jack In")
        "\"" (cider-jack-in-cljs :wk "Cider Jack In CLJS")
        "." (cider-find-var :wk "find var")
        "," (cider-pop-back :wk "popback from var")
        "c" (hydra-cljr-help-menu/body :wk "CLOJURE REFACTOR")
        "e" (hydra-cider-eval/body :wk "CIDER EVAL")
        "r" (:ignore t :wk "REPL")
        "t" (hydra-cider-test/body :wk "TEST")

        "rb" (cider-jack-in-clj&cljs :wk "Cider Jack In CLJ & CLJS")
        "rc" (cider-jack-in :wk "Make clj REPL")
        "rd" (cider-debug-defun-at-point :wk "instrument fun at point for debugging")
        "ri" (cider-insert-last-sexp-in-repl :wk "insert sexp into repl")
        "rl" (cider-load-buffer :wk "Load Buffer")
        "rn" ((lambda () (interactive) (cider-repl-set-ns (cider-current-ns)) (cider-ns-reload))
              :wk "load & set REPL ns")
        "rq" (cider-quit :wk "REPL quit")
        "rs" (cider-jack-in-cljs :wk "Make cljscript REPL")))
(setq cider-files-keys
      (append
       '("jr" (cider-switch-to-repl-buffer :wk "goto REPL"))
       cider-common-keys))
(setq cider-repl-keys
      (append
       '("jr" (:wk "goto REPL")
         "jr" (cider-switch-to-last-clojure-buffer :wk "goto REPL"))
       cider-common-keys))
(setq cider-test-report-keys
      '(
        "k" cider-test-previous-result)
      ;; ("j" cider-test-next-result nil)
  ;; ("d" cider-test-jump :exit t)
      )
(setq insert-keys
      '("i" (evil-lispy-state :wk "insert -> lispy state")
        ;; "I" (I-lispy :wk "insert line -> lispy state")
        "o" (o-lispy :wk "open below -> lispy state")
        "O" (O-lispy :wk "open above -> lispy state")
        "a" (a-lispy :wk "append -> lispy state")
        "A" (A-lispy :wk "append line -> lispy state")
        ";" (lispy-comment :wk "lispy comment")
        "C-u" (universal-argument :wk "universal argument")
        "M-." (lispy-goto-symbol :wk "goto symbol")
        "M-," (pop-tag-mark :wk "pop from symbol")))
(setq prog-keys
      (append insert-keys
              '("C-j" (evil-scroll-page-down :wk "page down")
                "C-k" (evil-scroll-page-up :wk "page up"))))

;; (setq elisp-keys)


;; we put "" nil at top of key defs that are going to get used by
;; general-define-key, but leave it out above where we mix and match
;; definitions.
(setq cider-files-keyz (append '("" nil) cider-files-keys))
(setq cider-repl-keyz (append '("" nil) cider-repl-keys))
(setq normal-keyz (append '("" nil) normal-keys))
(setq insert-keyz (append '("" nil) insert-keys))
(setq prog-keyz (append '("" nil) prog-keys))

(general-define-key :states
                    '(normal visual emacs)
                    "[" '(evil-lispy/enter-state-left :wk "enter lispy mode left")
                    "]" '(evil-lispy/enter-state-right :wk "enter lispy mode right")
                    "(" '(lispy-parens-from-normal :wk "enter lispy, insert parens")
                    "q" '(cider-popup-buffer-quit-function :wk "quit")
                    "C-p" '(helm-show-kill-ring :wk "show kill ring")
                    "ESC" '(keyboard-escape-quit :wk "quit"))
(apply 'general-define-key :prefix "SPC"
       :states '(normal visual emacs motion)
       normal-keyz)
(apply 'general-define-key :keymaps '(prog-mode-map)
       :states '(normal visual emacs)
       insert-keyz)  
(apply 'general-define-key :keymaps '(clojure-mode-map)
       :states '(normal visual emacs)
       prog-keys)
(apply 'general-define-key :prefix "," :keymaps '(clojure-mode-map)
       :states '(normal visual emacs)
       cider-files-keyz)
(apply 'general-define-key :keymaps '(cider-repl-mode-map)
       :states '(normal visual emacs)
       "C-j" '(cider-repl-forward-input :wk "Next Command")
       "C-k" '(cider-repl-backward-input :wk "Previous Command")
       prog-keyz)
(apply 'general-define-key :prefix "," :keymaps '(cider-repl-mode-map)
       :states '(normal visual emacs)
       cider-repl-keyz)
(apply 'general-define-key :prefix "SPC"
       :keymaps
       '(magit-status-mode-map
         magit-revision-mode-map
         ielm-map
         help-mode-map
         magit-log-mode-map)
       :states '(normal visual emacs)
       normal-keyz)  
(general-def org-mode-map "C-'" 'org-edit-special)
(general-def org-src-mode-map "C-'" 'org-edit-src-abort)
;; (general-def emacs-lisp-mode-map "C-'" 'org-edit-src-abort)
(general-def magit-hunk-section-map ; not really working...?
  "-" 'magit-diff-less-context         
  "+" 'magit-diff-more-context)
;; (general-def cider-test-report-map "b" 'hydra-buffers/body)
;; (apply 'general-define-key :keymaps '(cider-test-report-mode-map)
;;                     :states '(normal visual emacs)
;;                     "" nil
;;                     "b" 'hydra-buffers/body)
;; (apply 'general-define-key :keymaps '(emacs-lisp-mode-map)
;;                     :states '(normal visual emacs)
;;                     "" nil
;;                     "b" 'hydra-buffers/body)

;; ============= LISPY ==============================
;; (general-def lispy-mode-map
;;   :definer 'lispy
;;   "y" #'lispy-new-copy                  ; lose lispy-occur -> /
;;   ;; swap p and P
;;   "p" #'lispy-paste
;;   "P" #'lispy-eval-other-window
;;   "d" #'noct:lispy-delete               ; lose lispy-different -> o
;;   ;; like in visual state
;;   "o" #'lispy-different                 ; lose lispy-other-mode (don't use)
;;   "/" #'lispy-occur                     ; lose lispy-splice -> x
;;   "x" #'lispy-splice                    ; lose lispy-x -> c
;;   ;; "change" works as a mnemonic for some of these actions (e.g. change to
;;   ;; cond, change to if, change to defun, and change to lambda; there's not
;;   ;; really a common theme for the other actions)
;;   "c" #'lispy-x                         ; lose lispy-clone -> q
;;   "f" #'lispy-ace-paren                 ; lose lispy-flow -> n
;;   ;; maybe
;;   "F" #'lispy-ace-char                  ; lose lispy-follow -> ???
;;   ;; or this
;;   "t" #'lispy-ace-char                  ; lose lispy-teleport -> ???
;;   ;; kind of like repeating a search
;;   "n" #'lispy-flow
;;   ;; I don't have a good mnemonic for this other than that q sounds vaguely
;;   ;; similar to c
;;   "q" #'lispy-clone
;;   ;; swap m and v
;;   "v" #'lispy-mark-list
;;   "m" #'lispy-view)

;;   ;; extra not-vimmy personal configuration
;;   ;; swap H and A; makes more sense given default h and a
;;   "H" #'lispy-beginning-of-defun
;;   "A" #'lispy-ace-symbol-replace)
(general-def clojure-mode-map "e" 'e-clojure)
(general-def emacs-lisp-mode-map "e" 'e-lisp)

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
     ;; (lispy-define-key lispy-mode-map (kbd "e") 'cider-eval-last-sexp)
     ;; (lispy-define-key lispy-mode-map (kbd "e") 'special-lispy-eval)
     (lispy-define-key lispy-mode-map (kbd "\"") 'evil-ex)
     ;; (lispy-define-key lispy-mode-map (kbd ",") 'in-lispy)
     ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (sr-speedbar cider winum wk use-package magit lispy highlight-parentheses helm-projectile helm-ag evil el-get diminish delight company clojure-mode buffer-move))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
