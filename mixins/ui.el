;;; ui.el --- Summary
;;; Commentary:
;;; UI goodies.
;;; Code:

;;
;; Re-spawn scratch buffer when killed
;;
(use-package immortal-scratch
  :init
  (setq initial-scratch-message "")
  (setq initial-major-mode 'text-mode)
  :hook
  (after-init . immortal-scratch-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package string-inflection)

(use-package visual-regexp)

(use-package visual-regexp-steroids
  :straight (visual-regexp-steroids
             :type git
             :host github
             :repo "benma/visual-regexp-steroids.el"
             :files ("visual-regexp-steroids.el" "regexp.py"))
  :requires visual-regexp
  :bind (:map global-map
              ("C-s" . isearch-forward)
              ("C-r" . isearch-backward)
              ("C-q" . query-replace)
              ("C-c r" . vr/replace)
              ("C-c q" . vr/query-replace)
              ("C-c r" . vr/isearch-backward)
              ("C-c s" . vr/isearch-forward)))


(use-package multiple-cursors
  :bind (:map global-map
              ("C-c m" . vr/mc-mark)
              ("C->" . mc/mark-next-like-this)
              ("C-<" . mc/mark-previous-like-this)
              ("C-c C-<" . mc/mark-all-like-this)))

;; directory tree view
(use-package treemacs
  :init
  (require 'treemacs-treelib))

(use-package neotree
  :config
  '((global-set-key [f8] 'neotree-toggle)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))


;; ibuffer
(use-package ibuffer
  :config (global-set-key [remap-list-buffers] 'ibuffer))

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

;; drag selections around
(use-package drag-stuff
  :init (setq drag-stuff-modifier 'ctrl)
  :config (drag-stuff-global-mode t)
  (bind-keys :map drag-stuff-mode-map
             ("M-S-<up>" . drag-stuff-up)
             ("M-S-<down>" . drag-stuff-down)))


(use-package smart-mode-line
  :init
  (progn
     (setq sml/theme nil
           sml/shorten-directory t
           sml/name-width 32
           sml/shorten-modes t)
           ;; sml/use-projectile-p 'before-prefixes
           ;; sml/projectile-replacement-format "%s/"

     (sml/setup)))


(provide 'ui)
;;; ui.el ends here.
