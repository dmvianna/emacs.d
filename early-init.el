;;; Package --- Summary
;;; Commentary:
;;; early init file
;;; Code:

;; increase this early, decrease later on again
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;;; disable package.el and use straight by default
(setq package-enable-at-startup nil
      straight-use-package-by-default t
      straight-vc-git-default-protocol 'ssh)

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

;; add modules within this directory to the scope
(add-to-list 'load-path
             (expand-file-name "local-packages" user-emacs-directory))


(provide 'early-init)
;;; early-init.el ends here
