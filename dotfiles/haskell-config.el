;;; Haskell -- Summary:
;;;
;;; This is where I configure most of my
;;; Haskell development environment
;;;
;;; Commentary:
;;; Code:

(defun haskell-lsp-ui-mode-hook ()
  (prog-mode . lsp-ui-mode)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )

(defun haskell-config-mode-hook ()
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)
  (define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
  (define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
  (custom-set-variables
   '(haskell-completion-backend 'lsp)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-enable-hindent-style 'fundamental)
   '(haskell-indent-spaces 2)
   '(haskell-process-args-ghci "ghci")
   '(haskell-process-type 'stack-ghci)
   '(haskell-stylish-on-save 't)
   '(haskell-tags-on-save t)
   '(lsp-modeline-code-actions-enable nil)
   )
  )


(provide 'haskell-config)

;;; haskell-daniel-config.el ends here

