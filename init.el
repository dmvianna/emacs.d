;;; Package --- Summary
;;; Commentary:
;;;
;;; Reload this config with M-: (load user-init-file)
;;;
;;; init file
;;; Code:

;;;
;;;
;;; File structure inspired by emacs-bedrock
;;; https://git.sr.ht/~ashton314/emacs-bedrock
;;;
;;;

;;;
;;; To reload the config, run
;;; M-: (load user-init-file)
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Startup speed, annoyance suppression
;;
;; We have started, bring gc threshold back down
(setq gc-cons-threshold (* 16 1024 1024) ;; 16 MB
      gc-cons-percentage 0.1)

;; garbage collection is hard, let a library do it
(use-package gcmh
  :delight (gcmh-mode)
  :commands (gcmh-mode)
  :functions (gcmh-idle-garbage-collect)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-verbose nil)
  :hook
  (emacs-startup-hook . gcmh-mode))

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))

;;; auto-byte-compile
(setq load-prefer-newer t)
(package-initialize)
(use-package auto-compile
  :config
  '((auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

;; file encoding
(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;; Disable backgrounding, of course. The first thing
;; a newbie in a terminal should do to remain sane.
(global-unset-key (kbd "C-z"))

;; Basic layout
;; t is True, nil is False. If you're copy-pasting 1 or -1,
;; these are also truthy and falsy values, respectively.
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; My favourite font. Do as you please.
(add-to-list 'default-frame-alist '(font . "Inconsolata-16"))

;; default mode for the *scratch* buffer
(setq initial-major-mode 'fundamental-mode)

;; No electric indent
(electric-indent-mode -1)

;; this information is useless for most
(setq display-time-default-load-average nil)

;; Automatically reread from disk if the underlying file changes
(setq auto-revert-interval 3)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; line wrap by default
(global-visual-line-mode t)

;; Save history of minibuffer
(savehist-mode)

;;; Save all tempfiles in $TMPDIR/emacs-$UID/
(defconst emacs-tmp-dir
   (format "%s/%s%s/" temporary-file-directory "emacs-" (user-uid)))
(unless (file-directory-p emacs-tmp-dir)
  (make-directory emacs-tmp-dir))
(setq backup-directory-alist
      `(("." . ,emacs-tmp-dir)))
(setq undo-tree-history-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setq enable-recursive-minibuffers t)                             ; Use the minibuffer whilst in the minibuffer
(setq completion-cycle-threshold 1)                               ; TAB cycles candidates
(setq completions-detailed t)                                     ; Show annotations
(setq tab-always-indent 'complete)                                ; When I hit TAB, try to complete, otherwise, indent

(fido-vertical-mode)                                              ; Show completion candidates in a vertical, interactive list
(setq completion-styles '(basic initials substring))              ; Different styles to match input to candidates
(define-key minibuffer-mode-map (kbd "TAB") 'minibuffer-complete) ; TAB acts more like how it does in the shell

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Mouse
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mouse-wheel-mode t)
(xterm-mouse-mode t)

;;; Mouse color must be the same in emacs-daemon
(setq mouse-color "#002b36") ;;; solarized-dark light gray
(set-mouse-color mouse-color) ;;; that's emacs

;;; Keep mouse color in GUI too
(require 'frame)
(defun set-default-hook (frame)
  "Set all default frame logic.  FRAME is the frame being created."
  (modify-frame-parameters
   frame (list (cons 'mouse-color mouse-color))))

;; Enable horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; mouse wheel in iterm2 terminal
(require 'mwheel)
(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)
(global-set-key [mouse-4] 'next-line)
(global-set-key [mouse-5] 'previous-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   My preferred default interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; focus on emacs frame when it is started
(add-hook 'server-switch-hook #'raise-frame)

;;; make a fullscreen function
(defun fullscreen-frame ()
  "Make Emacs use all the screen area."
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

;;; make a default layout function
(defun default-layout ()
  "Apply my preferred layout to an existing frame."
  (interactive)
  (fullscreen-frame)
  (split-window-horizontally)
  (treemacs))


;;; that's what emacs-daemon uses
(add-hook
 'after-make-frame-functions
 (lambda (frame)
   (set-default-hook frame)
   (select-frame frame)
   (when (display-graphic-p frame)
     (default-layout))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Switch windows easily
(global-set-key (kbd "M-o") 'other-window)

;; Mode line information
(setq line-number-mode t)                         ; Show current line in modeline
(setq column-number-mode t)                       ; Show column as well

(setq x-underline-at-descent-line nil)            ; Prettier underlines
(setq switch-to-buffer-obey-display-actions t)    ; Make switching buffers more consistent

(setq-default show-trailing-whitespace nil)       ; By default, don't underline trailing spaces
(setq-default indicate-buffer-boundaries 'left)   ; Show icons showing the size of the buffer in the margin

;;; Set tab width
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; Misc. UI tweaks
(blink-cursor-mode nil)           ; Steady cursor
;; (pixel-scroll-precision-mode)     ; Smooth scrolling
(global-hl-line-mode)             ; Highlight the current line

;; bind home and end keys to beginning and end of file
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

(use-package prog-mode
  :straight nil
  :config
  ;; open pairs with extra newline in between, and autoindent
  (electric-pair-local-mode t)
  :init
  (add-to-list 'auto-mode-alist '("\\.list$" . prog-mode))
  ;; Display line numbers
  :hook
  (prog-mode . display-line-numbers-mode))

;; Use shift-arrow to move between windows
(require 'windmove)
(windmove-default-keybindings 'shift)

;;; change capitalisation
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; ;; Display sane file names
;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'forward)

;; Disable emoji picker / rebind it in Gnome
;; I can't do it from within emacs but I need somewhere to put my documentation,
;; so here it is:
;;
;; List relevant info:
;;
;; $ gsettings list-recursively org.freedesktop.ibus.panel.emoji
;;
;; It will list the default:
;;
;; > org.freedesktop.ibus.panel.emoji hotkey ['<Super>period']
;;
;; Replace with something safe:
;;
;; $ gsettings set org.freedesktop.ibus.panel.emoji hotkey '["<Super>e"]'
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setq tab-bar-show 0)

;; Add the time to the tab-bar, if visible
;; (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
;; (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
;; (setq display-time-format "%a %F %T")
;; (setq display-time-interval 1)
;; (display-time-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t))

(use-package all-the-icons)
(use-package all-the-icons-dired
  :requires all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Org mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org is loaded by other packages, so it must be loaded first lest we load
;; conflicting versions
(use-package org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optional mixins
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uncomment these lines or copy from the mixin/ files as you see fit

;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
(load-file (concat user-emacs-directory "mixins/ui.el"))

;; Packages for software development
(load-file (concat user-emacs-directory "mixins/dev.el"))

;; Language support
(load-file (concat user-emacs-directory "mixins/languages.el"))

;; File viewers
(load-file (concat user-emacs-directory "mixins/viewers.el"))

;; Minibuffer & popups
(load-file (concat user-emacs-directory "mixins/minibuffers.el"))

;; Entertainment
(load-file (concat user-emacs-directory "mixins/entertainment.el"))

;; Modes that must be loaded early
(load-file (concat user-emacs-directory "mixins/early-modes.el"))

(provide 'init)
;;; init.el ends here
