;;; entertainment.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Entertainment for the masses.
;;; Code:

;; emms -- media player

(use-package emms
  :custom
  (emms-source-file-default-directory "~/music/")
  (emms-playlist-buffer-name "*Music*")
  (emms-info-asynchronously t)
  ;;; make sure libtag is the only thing delivering metadata
  (emms-info-functions '(emms-info-libtag))
  :config
  (require 'emms-setup)
  ;;; load functions that will talk to emms-print-metadata
  ;;; which in turn talks to libtag and gets metadata
  (require 'emms-info-libtag)
  (require 'emms-mode-line)
  (emms-all)
  (emms-default-players)
  (emms-mode-line 1)
  (require 'emms-playing-time)
  (emms-playing-time 1))

(provide 'entertainment)
;;; entertainment.el ends here.
