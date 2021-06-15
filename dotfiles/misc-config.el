;;; Package --- Summary
;;; Commentary:
;;; Misc config
;;; Code:

(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

(column-number-mode t)
(global-unset-key (kbd "C-z"))
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(set-frame-font "Inconsolata-16")
(require 'uniquify) ;; Inbuilt - Display sane file names
(setq uniquify-buffer-name-style 'forward)
(require 'ido) ;; Inbuilt - Finding files made easier
(ido-mode t)
(global-auto-revert-mode 1) ;; Reload files that have been changed
(global-visual-line-mode t) ;; line wrap by default

;;; Tagging
;; (require 'etags-table)
;; (setq etags-table-search-up-depth 10)
;; (setq tags-revert-without-query 1) ;; Stop annoying tag reversal queries

(require 'windmove)
(windmove-default-keybindings 'shift)
;; (global-set-key (kbd "C-e") 'eshell)

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
;; (setq undo-tree-history-directory-alist
;;       `((".*" . ,emacs-tmp-dir)))
;; (setq auto-save-list-file-prefix
;;       emacs-tmp-dir)

;;; Set tab width
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;;; Mouse
(mouse-wheel-mode t)
(xterm-mouse-mode t)

;;; change capitalisation
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; auto-byte-compile
(setq load-prefer-newer t)
(package-initialize)
(use-package auto-compile
  :ensure t
  :straight t
  :config
  '((auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

;; exec path from shell
(use-package exec-path-from-shell
  :ensure t
  :straight t
  :config
  (exec-path-from-shell-initialize))

;; org-mode
(global-set-key "\C-ca" 'org-agenda)

;; open file in browser
(global-set-key "\C-c\C-zv" 'browse-url-of-file)

;; mouse wheel in iterm2 terminal
(require 'mwheel)
(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)
(global-set-key [mouse-4] 'next-line)
(global-set-key [mouse-5] 'previous-line)

;; move lines and regions
(global-set-key [(meta down)] 'elpy-nav-move-line-or-region-down)
(global-set-key [(meta up)] 'elpy-nav-move-line-or-region-up)

;; theme
(use-package solarized-theme
  :straight t
  :ensure t
  :config
  (load-theme 'solarized-dark t))

;; because in Big Sur this is set to /
(setq default-directory (getenv "HOME"))

;; directory tree view
(use-package neotree
  :straight t
  :ensure t
  :config
  '((global-set-key [f8] 'neotree-toggle)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))

;; incremental completions
(use-package consult
  :straight t
  :ensure t)
(use-package selectrum
  :straight t
  :ensure t
  :config
  '((selectrum-mode)
    (setq completion-styles '(substring))))

;; bind meta to super
(setq x-meta-keysym 'super
      x-super-keysym 'meta)

;; magit is too important to put anywhere else
(use-package magit
  :straight t
  :ensure t)

(use-package drag-stuff
  :straight t
  :ensure t
  :init (setq drag-stuff-modifier 'ctrl)
  :config (drag-stuff-global-mode t)
  (bind-keys :map drag-stuff-mode-map
             ("<M-S-up>" . drag-stuff-up)
             ("<M-S-down>" . drag-stuff-down)))

(provide 'misc-config)
;;; misc-config ends here
