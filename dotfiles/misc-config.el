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

(require 'frame)
(defun set-default-hook (frame)
  "Set all default frame logic.  FRAME is the frame being created."
  (modify-frame-parameters
   frame (list (cons 'mouse-color mouse-color))))

;;; that's what emacs-daemon uses
(add-hook
 'after-make-frame-functions
 (lambda (frame)
   (set-default-hook frame)
   (select-frame frame)
   (when (display-graphic-p frame)
     (default-layout))))

;;; change capitalisation
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; auto-byte-compile
(setq load-prefer-newer t)
(package-initialize)
(use-package auto-compile
  :config
  '((auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

;; exec path from shell
(use-package exec-path-from-shell
  :config
  (dolist (var '("PATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
                 "SSH_AGENT_PID" "SSH_AUTH_SOCK"
                 "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "XAUTHORITY"))
    (add-to-list 'exec-path-from-shell-variables var))
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
  :config
  (load-theme 'solarized-dark t))

;; because in Big Sur this is set to /
(setq default-directory (getenv "HOME"))

;; directory tree view
(use-package treemacs)

(use-package neotree
  :config
  '((global-set-key [f8] 'neotree-toggle)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))

;; persist history over Emacs restarts
(savehist-mode)

;; incremental completions
(use-package consult)

(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :custom
  (selectrum-prescient-enable-filtering nil)
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package orderless
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
  :init (setq drag-stuff-modifier 'ctrl)
  :config (drag-stuff-global-mode t)
  (bind-keys :map drag-stuff-mode-map
             ("<s-up>" . drag-stuff-up)
             ("<s-down>" . drag-stuff-down)))

(use-package smartparens
  :config (smartparens-global-mode t))

(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

(use-package all-the-icons)
(use-package all-the-icons-dired
  :requires all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package prog-mode
  :straight nil
  :hook
  (prog-mode . display-line-numbers-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.list$" . prog-mode)))

(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "tmp")
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "amazonka")
     (add-to-list 'grep-find-ignored-directories "amazonka-1.6.1")
     (add-to-list 'grep-find-ignored-directories "amazonka-2.0.0")
     (add-to-list 'grep-find-ignored-directories ".stack-work")))
(setq wgrep-enable-key (kbd "C-c C-c"))
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

(provide 'misc-config)
;;; misc-config.el ends here
