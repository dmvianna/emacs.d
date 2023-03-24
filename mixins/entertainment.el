;;; entertainment.el --- Summary
;;; Commentary:
;;; Entertainment for the masses.
;;; Code:

;; emms -- media player

(use-package emms
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music/")
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-info-asynchronously t)
  (require 'emms-info-libtag) ;;; load functions that will talk to emms-print-metadata which in turn talks to libtag and gets metadata
  (setq emms-info-functions '(emms-info-libtag)) ;;; make sure libtag is the only thing delivering metadata
  (require 'emms-mode-line)
  (emms-mode-line 1)
  (require 'emms-playing-time)
  (emms-playing-time 1))
