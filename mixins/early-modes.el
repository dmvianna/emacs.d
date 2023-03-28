;;; early-modes.el --- Summary
;;; Commentary:
;;; Modes that need to be loaded early, before language modes and such,
;;; to work properly.
;;; Code:

(use-package envrc
 :config (envrc-global-mode))

(provide 'early-modes)
;;; early-modes.el ends here
