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
(use-package auto-complete)
(use-package evil-surround :ensure t :config (global-evil-surround-mode 1))
;; (use-package cl)
(use-package clj-refactor)
(use-package command-log-mode)

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
      backup-directory-alist '(("" . "~/.emacs.d/backup")) ; put all backups here
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
      ;; cider-default-cljs-repl 'figwheel-main
      eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit
      cider-eldoc-display-context-dependent-info t
      cider-overlays-use-font-lock t
      cider-prompt-for-symbol nil
      cider-auto-select-test-report-buffer nil
      cider-auto-test-mode 1
      cider-test-show-report-on-success nil
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
;; ========== function defs ==========
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "my-functions.el")

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
            (clj-refactor-mode 1)
            (yas-minor-mode 1)
            (company-mode)
            (cider-company-enable-fuzzy-completion)
            (eldoc-mode))) 

;; ========== key defs and hydras ==========
(load "key-defs-hydras.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (command-log-mode clj-refactor evil-surround auto-complete cider-eldoc sr-speedbar cider winum wk use-package magit lispy highlight-parentheses helm-projectile helm-ag evil el-get diminish delight company clojure-mode buffer-move)))
 '(safe-local-variable-values
   (quote
    ((source-dir . "aoc2018_14")
     (source-dir . "aoc2018_13")
     (source-dir . aoc2018_13)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
