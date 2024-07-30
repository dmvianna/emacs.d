;;; tramp-config.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; More methods for tramping around the cloud.
;;; Code:

;; make sure you've set your default project with:
;; gcloud config set project <project-name>

(require 'tramp)

(setq
 tramp-remote-path
 '("/run/current-system/sw/bin"
   "/bin"
   "/usr/bin"
   "/sbin"
   "/usr/sbin"
   "/usr/local/bin"
   "/usr/local/sbin"
   "/opt/bin"
   "/opt/sbin"))

(add-to-list
 'tramp-methods
 '("gcssh"
   (tramp-login-program "gcloud")
   (tramp-login-args (("compute" "ssh" "%h")))
   (tramp-async-args (("-q")))
   (tramp-remote-shell "sh")
   (tramp-remote-shell-args ("-c"))
   (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null")
                   ("-o" "UserKnownHostsFile=/dev/null")
                   ("-o" "StrictHostKeyChecking=no")))
   (tramp-default-port 22)))

(add-to-list
 'tramp-methods
 '("gcsroot"
   (tramp-login-program "gcloud")
   (tramp-login-args (("compute" "ssh" "%h" "--" "sudo" "su")))
   (tramp-async-args (("-q")))
   (tramp-remote-shell "sh")
   (tramp-remote-shell-args ("-c"))
   (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null")
                   ("-o" "UserKnownHostsFile=/dev/null")
                   ("-o" "StrictHostKeyChecking=no")))
   (tramp-default-port 22)))


;; ... after which it's as easy as:
;;
;; C-x C-f /gcssh:compute-instance:/path/to/filename.smile

(provide 'tramp-config.el)
;;; tramp-config.el ends here
