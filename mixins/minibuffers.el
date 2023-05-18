;;; minibuffers.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Minibuffers and popup goodies.
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Motion aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer and completion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :init
  (fido-mode -1)
  (vertico-mode))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :config
  (marginalia-mode))

;; Popup completion-at-point
(use-package corfu
  :elpaca (:host github :repo "minad/corfu")
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-scroll-margin 5)
  :config
  (global-corfu-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; Extensions
(use-package corfu-popupinfo
  :after corfu
  :elpaca (:repo "minad/corfu"
                 :files ("extensions/corfu-popupinfo.el"))
  :config (corfu-popupinfo-mode 1)
  :custom (corfu-popupinfo-delay '(0.5 . 0.2)))

(use-package corfu-history
  :after corfu
  :elpaca (:repo "minad/corfu"
                 :files ("extensions/corfu-history.el"))
  :config
  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Consult: Misc. enhanced commands
(load-file (concat user-emacs-directory "mixins/minibuffers/consult-config.el"))

;; Orderless: powerful completion style
(load-file (concat user-emacs-directory "mixins/minibuffers/orderless-config.el"))

(provide 'minibuffers)
;;; minibuffers.el ends here.
