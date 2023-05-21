;;; Package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; early init file
;;; Code:

;; increase this early, decrease later on again
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

(unless (or (daemonp) noninteractive)
  ;; Site files tend to use `load-file', which emits "Loading X..." messages in
  ;; the echo area, which in turn triggers a redisplay. Redisplays can have a
  ;; substantial effect on startup times and in this case happens so early that
  ;; Emacs may flash white while starting up.
  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))

  ;; Undo our `load-file' advice above, to limit the scope of any edge cases it
  ;; may introduce down the road.
  (define-advice startup--load-user-init-file (:before (&rest _) init-doom)
    (advice-remove #'load-file #'load-file@silence)))

;;; disable package.el
(setq package-enable-at-startup nil)

;;; bootstrap elpaca
(defvar elpaca-installer-version 0.4)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; Populate elpaca queue with the packages that will process the config.
;; Install use-package support
(if (not (featurep 'use-package)) (elpaca use-package))
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Install system-packages
(elpaca system-packages
  (setq system-packages-use-sudo t
        system-packages-package-manager 'dnf
        async-shell-command-buffer 'new-buffer))

;; git shell ?! :-DDDD

;; nix-mode depends on a package that comes with magit, so we fetch magit
;; before moving forward. Below we tell elpaca to not continue until the
;; current download queue is empty.

(use-package magit
  :init
  ;; status is expensive in big repos, only refresh if
  ;; it is the current buffer
  ;; (setq magit-refresh-status-buffer nil)
  ;; it is always git, so no need to display it
  ;; https://emacs.stackexchange.com/a/10957/3895
  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((noback (replace-regexp-in-string
                     (format "^ %s" (vc-backend buffer-file-name))
                     " " vc-mode)))
        (setq vc-mode noback))))
  (setq vc-display-status nil) ;; don't display branch name in mode line
  (if (not (boundp 'project-switch-commands))
      (setq project-switch-commands nil))
  :bind (:map
         magit-mode-map
         ("C-x g" . magit-status)))

;; Block until current queue processed.
(elpaca-wait)

;; load the extension before continuing
(use-package use-package-ensure-system-package)
(elpaca-wait)

;; add modules within this directory to the scope
(add-to-list 'load-path
             (expand-file-name "local-packages" user-emacs-directory))

;; Premature redisplays can substantially affect startup times and produce
;; ugly flashes of unstyled Emacs.
(setq-default inhibit-redisplay t
              inhibit-message t)
;; We'll reset it later in init.el

(provide 'early-init)
;;; early-init.el ends here
