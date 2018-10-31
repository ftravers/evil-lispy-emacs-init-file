(set-face-attribute 'default nil :height 120 :family "DejaVu Sans Mono")

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
(use-package clojure-mode :mode ("\\.clj\\'"))
(use-package evil)
(use-package projectile)                ; navigate git projects
(use-package diminish)                  ; reduce mode-line clutter
(use-package delight)                   ; change how modes appear in mode-line
(use-package which-key :diminish which-key-mode)             ; show which keys you can press next
(use-package lispy)
(use-package evil-lispy)
(use-package helm-projectile :diminish helm-mode) ; auto-complete commands
(use-package helm-ag)				  ; silver searcher for projects
(use-package winum)                     ; switch between buffers using numbers
(use-package magit) 			; git integration
(use-package evil-magit)
(use-package helm-ag)
(use-package highlight-parentheses)
(use-package company)
(use-package buffer-move)
(use-package el-get)
(use-package cider)
(use-package general :config (general-evil-setup t))

; (apply use-package '(cider el-get))

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

(show-paren-mode 1)
(setq show-paren-delay 0) 
(set-face-background 'show-paren-match "#640000")
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(highlight-parentheses-mode 1)
(save-place-mode)
(setq help-window-select t)
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))

;; projectile/helm config
(projectile-global-mode)
(setq projectile-completion-system 'helm
      projectile-switch-project-action 'helm-projectile)
(helm-projectile-on)
(setq cider-font-lock-dynamically '(macro core function var))
(setq cider-eldoc-display-context-dependent-info t)
(setq cider-overlays-use-font-lock t)
(setq cider-default-cljs-repl 'figwheel)
(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

;; Modify how modes show up in the mode-line
(delight '((helm-mode)
	   (emacs-lisp-mode)))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(diminish 'auto-revert-mode)
(diminish 'projectile-mode)
(diminish 'helm-mode)
(diminish 'lispy-mode)

;; highlight parens colors
(setq hl-paren-colors
      '("red" "green1" "orange1" "cyan1" "yellow1" 
        "slateblue1" "magenta1" "purple")
      hl-paren-background-colors
      '("gray14" "gray14" "gray14" "gray14" "gray14" "gray14" "gray14" "gray14"))

;; desktop config
(setq desktop-dirname             (expand-file-name  "~/.emacs.d/")
      desktop-auto-save-timeout   30
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-files-not-to-save   "^$" )
(desktop-save-mode 1)

;; ============= Custom Functions ===================
(defun in-lispy ()
  (interactive)
  (if (evil-lispy-state-p)
      (hydra-buffer-menu/body)
    (self-insert-command)))

(message "yess")

;; (defun in-l()
;;   (interactive)
;;   (if (evil-lispy-state-p)
;;       (message "in lispy state")
;;     (message "NOT in lispy state")))


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

(defun my-lispy-delete ()
  (interactive)
  (lispy-kill-at-point)
  (lispy-down 1)) 

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

(defun lispy-append ()
  (interactive)
  (forward-char)
  (evil-lispy-state))

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

;; (switch-to-buffer (find-file-noselect "~/.emacs.d/init.el"))

;; ============= Hooks ==============================
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

;; ============= GENERAL ==============================
(general-define-key :states '(normal visual emacs)
                    "[" '(evil-lispy/enter-state-left :which-key "enter lispy mode left")
                    "]" '(evil-lispy/enter-state-right :which-key "enter lispy mode right")
                    "(" '(lispy-parens-from-normal :which-key "enter lispy, insert parens")
                    "ESC" '(keyboard-escape-quit :which-key "quit"))

(general-define-key :states '(normal visual emacs motion) :prefix "SPC"
                    :non-normal-prefix "C-SPC"
                    "" nil
                    "b" '(:ignore t :which-key "Buffers")
                    "f" '(:ignore t :which-key "Files")
                    "g" '(:ignore t :which-key "Magit")
                    "j" '(:ignore t :which-key "Jump")
                    "p" '(:ignore t :which-key "Projects")
                    "s" '(:ignore t :which-key "Search")
                    "w" '(:ignore t :which-key "Window")
                    "q" '(:ignore t :which-key "Quit")

                    "1" '(winum-select-window-1 :which-key "move window 1")
                    "2" '(winum-select-window-2 :which-key "move window 2")
                    "3" '(winum-select-window-3 :which-key "move window 3")
                    "4" '(winum-select-window-4 :which-key "move window 4")
                    "5" '(winum-select-window-5 :which-key "move window 5")

                    "/" '(helm-projectile-ag :wich-key "ag")
                    "SPC" '(helm-M-x :which-key "run command")

                    "bb" '(helm-mini :which-key "buffer list")
                    "bd" '(kill-this-buffer :which-key "kill buffer")
                    "bt" '(transpose-windows :which-key "transpose windows")
                    "bs" '(:ignore t :which-key "Buffer Save")
                    "bss" '(save-buffer :which-key "save this buffer")
                    "bsa" '(save-some-buffers :which-key "save all buffers")


                    "ff" '(helm-find-files :which-key "Find Files")
                    "fed" '(ffs "/home/fenton/.emacs.d/init.el" :which-key "open init.el")

                    "gs" '(magit-status :which-key "magit status")
                    "gb" '(:ignore t :which-key "Magit Blame")
                    "gbb" '(magit-blame :which-key "magit blame")
                    "gbq" '(magit-blame-quit :which-key "magit blame quit")

                    "jl" '(avy-goto-line :which-key "jump to line")

                    "pp" '(helm-projectile-switch-project :which-key "switch to project")
                    "pf" '(helm-projectile-find-file :which-key "find file")
                    "ps" '(helm-projectile-ag :which-key "find git project file")

                    "qq" '(save-buffers-kill-terminal :which-key "Emacs Quit")

                    "sr" '(query-replace :which-key "query replace")

                    "wd" '(delete-window-balance :which-key "delete window")
                    "w0" '(delete-window-balance :which-key "delete window")
                    "wm" '(delete-other-windows :which-key "maximize window")
                    "w1" '(delete-other-windows :which-key "maximize window")
                    "wv" '(split-window-vertical-balance :which-key "split vertically")
                    "w2" '(split-window-vertical-balance :which-key "split vertically")
                    "w-" '(split-window-below-balance :which-key "split horizontally")
                    "w3" '(split-window-below-balance :which-key "split horizontally")
                    "w=" '(balance-windows :which-key "balance windows")
                    "wt" '(transpose-windows :which-key "transpose windows")) 

(general-define-key :keymaps '(prog-mode-map clojure-mode-map cider-repl-mode-map)
                    :states '(normal visual emacs)
                    "i" '(evil-lispy-state :which-key "insert -> lispy state")
                    "I" '(I-lispy :which-key "insert line -> lispy state")
                    "o" '(o-lispy :which-key "open below -> lispy state")
                    "O" '(O-lispy :which-key "open above -> lispy state")
                    "a" '(a-lispy :which-key "append -> lispy state")
                    "A" '(A-lispy :which-key "append line -> lispy state")
                    ";" '(lispy-comment :which-key "lispy comment")
                    "C-u" '(universal-argument :which-key "universal argument")
                    "M-." '(lispy-goto-symbol :which-key "goto symbol")
                    "M-," '(pop-tag-mark :which-key "pop from symbol")
                    "C-j" '(evil-scroll-page-down :which-key "page down")
                    "C-k" '(evil-scroll-page-up :which-key "page up"))

(general-define-key :keymaps 'prog-mode-map :prefix "SPC"
                    :states '(normal visual emacs)
                    "o" '(:ignore t :which-key "Fold")
                    "i" '(:ignore t :which-key "Fill")
                    "j" '(:ignore t :which-key "Jump")
                    "e" '(eval-buffer :which-key "Eval Buffer")
                    "." '(xref-find-definitions :which-key "find definition")
                    "," '(xref-pop-marker-stack :which-key "pop back")
                    "jr" '(cider-switch-to-repl-buffer :which-key "goto REPL")
                    "os" '(hs-show-all :which-key "show all")
                    "oh" '(hs-hide-all :which-key "hide all")
                    "oo" '(special-collapse-expand :which-key "toggle folding")
                    "ip" '(fill-paragraph :which-key "fill paragraph")) 

(general-define-key :keymaps '(clojure-mode-map cider-repl-mode-map) :prefix "SPC"
                    :states '(normal visual emacs)
                    "'" '(cider-jack-in :which-key "Cider Jack In")
                    "\"" '(cider-jack-in-cljs :which-key "Cider Jack In CLJS")
                    "." '(cider-find-var :which-key "find var")
                    "," '(cider-pop-back :which-key "popback from var")

                    "c" '(hydra-cljr-help-menu/body :which-key "CLOJURE REFACTOR")
                    "e" '(:ignore t :which-key "EVAL")
                    "r" '(:ignore t :which-key "REPL")
                    "t" '(:ignore t :which-key "TESTS")

                    "eb" '(cider-eval-buffer :which-key "eval buffer")

                    "rb" '(cider-jack-in-clj&cljs :which-key "Cider Jack In CLJ & CLJS")
                    "rc" '(cider-jack-in :which-key "Make clj REPL")
                    "rd" '(cider-debug-defun-at-point :which-key "instrument fun at point for debugging")
                    "ri" '(cider-insert-last-sexp-in-repl :which-key "insert sexp into repl")
                    "rl" '(cider-load-buffer-and-switch-to-repl-buffer :which-key "Load Buffer")
                    "rn" '(load-ns-goto-repl :which-key "load buffer, set REPL namespace, goto REPL")
                    "rq" '(cider-quit :which-key "REPL quit")
                    "rs" '(cider-jack-in-cljs :which-key "Make cljscript REPL")) 

(general-define-key :keymaps '(cider-repl-mode-map) :prefix "SPC"
                    :states '(normal visual emacs)
                    "j" '(:ignore t :which-key "JUMP")
                    "jr" '(cider-switch-to-last-clojure-buffer :which-key "pop back"))

(general-define-key :keymaps '(cider-stacktrace-mode-map)
                    :states '(normal visual emacs)
                    "q" '(cider-popup-buffer-quit-function :which-key "quit"))  

(general-define-key :keymaps '(magit-status-mode-map help-mode-map) :prefix "SPC"
                    :states '(normal visual emacs)
                    "1" '(winum-select-window-1 :which-key "move window 1")
                    "2" '(winum-select-window-2 :which-key "move window 2")
                    "3" '(winum-select-window-3 :which-key "move window 3")
                    "4" '(winum-select-window-4 :which-key "move window 4")
                    "5" '(winum-select-window-5 :which-key "move window 5"))

(general-define-key :keymaps 'cider-repl-mode-map 
                    :states '(normal visual emacs)
                    "C-j" '(cider-repl-forward-input :which-key "Next Command")
                    "C-k" '(cider-repl-backward-input :which-key "Previous Command")) 
;; ============= LISPY ==============================
(eval-after-load "lispy"
  `(progn
     (my-remove-lispy-key (kbd "C-,"))
     (my-remove-lispy-key (kbd "C-j"))
     (my-remove-lispy-key (kbd "d"))
     (lispy-define-key lispy-mode-map (kbd "d") 'my-lispy-delete)
     (lispy-define-key lispy-mode-map (kbd "x") 'collapse-expand)
     (lispy-define-key lispy-mode-map (kbd "p") 'special-lispy-paste)
     (lispy-define-key lispy-mode-map (kbd "f") 'special-lispy-flow)
     (lispy-define-key lispy-mode-map (kbd "i") 'special-lispy-tab)
     (lispy-define-key lispy-mode-map (kbd ":") 'evil-ex)
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
    (cider winum which-key use-package magit lispy highlight-parentheses helm-projectile helm-ag evil el-get diminish delight company clojure-mode buffer-move))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
