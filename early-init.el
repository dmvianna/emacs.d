;;; Package --- Summary
;;; Commentary:
;;; early init file
;;; Code:

;;; disable package.el
(setq package-enable-at-startup nil)

;;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; use use-package
(straight-use-package 'use-package)

;; Misc config
(add-to-list 'load-path
             (expand-file-name "dotfiles" user-emacs-directory)
             )

(provide 'early-init)
;;; early-init ends here

