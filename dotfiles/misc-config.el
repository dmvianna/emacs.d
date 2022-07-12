;;; Package --- Summary
;;; Commentary:
;;; Misc config
;;; Code:

(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

(column-number-mode t)
(global-unset-key (kbd "C-z"))

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(font . "Inconsolata-16"))

;; disable indenting after newline
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

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

;;; Mouse color must be the same in emacs-daemon
(setq mouse-color "#002b36") ;;; solarized-dark light gray
(set-mouse-color mouse-color) ;;; that's emacs
(require 'frame)
(defun set-mouse-hook (frame)
(modify-frame-parameters
  frame (list (cons 'mouse-color mouse-color))))
;;; that's what emacs-daemon uses
(add-hook 'after-make-frame-functions 'set-mouse-hook)

;;; change capitalisation
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; auto-byte-compile
(setq load-prefer-newer t)
(package-initialize)
(use-package auto-compile
  :straight t
  :config
  '((auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

;; exec path from shell
(use-package exec-path-from-shell
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
;; (global-set-key [(meta down)] 'elpy-nav-move-line-or-region-down)
;; (global-set-key [(meta up)] 'elpy-nav-move-line-or-region-up)

;; theme
(use-package solarized-theme
  :straight t
  :config
  (load-theme 'solarized-dark t))

;; because in Big Sur this is set to /
(setq default-directory (getenv "HOME"))

;; directory tree view
(use-package treemacs
  :straight t)

(use-package neotree
  :straight t
  :config
  '((global-set-key [f8] 'neotree-toggle)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))

;; persist history over Emacs restarts
(savehist-mode)

;; incremental completions
(use-package consult
  :straight t)

(use-package selectrum
  :straight t
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :straight t
  :custom
  (selectrum-prescient-enable-filtering nil)
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (orderless-skip-highlighting (lambda () selectrum-is-active))
  (selectrum-highlight-candidates-function #'orderless-highlight-matches))

;; bind meta to super
(setq x-meta-keysym 'super
      x-super-keysym 'meta)

;; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ibuffer-vc
  :straight t
  :hook (ibuffer-mode . ibuffer-vc-generate-filter-groups-by-vc-root)
  :bind (:map ibuffer-mode-map ("g" . ibuffer-list-buffers))
  :init
  ;; Include version control status info in the ibuffer list.
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)))
  :config (add-hook 'ibuffer-hook (lambda()
                                    (ibuffer-vc-set-filter-groups-by-vc-root)
                                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                                      (ibuffer-do-sort-by-alphabetic)))))

(use-package drag-stuff
  :straight t
  :init (setq drag-stuff-modifier 'ctrl)
  :config (drag-stuff-global-mode t)
  (bind-keys :map drag-stuff-mode-map
             ("<s-up>" . drag-stuff-up)
             ("<s-down>" . drag-stuff-down)))

(use-package smartparens
  :straight t
  :config (smartparens-global-mode t))

(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

(use-package all-the-icons
  :straight t)
(use-package all-the-icons-dired
  :straight t
  :requires all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package prog-mode
  :hook
  (prog-mode . display-line-numbers-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.list$" . prog-mode)))

;;; focus on emacs frame when it is started
(add-hook 'server-switch-hook #'raise-frame)

;;; make a fullscreen function
(defun fullscreen ()
  "Make Emacs use all the screen area."
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

;;; start emacs with my preferred layout
(split-window-horizontally)
(treemacs)
(fullscreen)

(provide 'misc-config)
;;; misc-config ends here
