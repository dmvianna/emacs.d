;;; communication.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Email, internet messaging, etc.
;;; Code:

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "emacs-slack"
   :default t
   :token "xoxs-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
   :subscribed-channels '(test-rename rrrrr)
   :full-and-display-names t)

  (slack-register-team
   :name "test"
   :token "xoxs-yyyyyyyyyy-zzzzzzzzzzz-hhhhhhhhhhh-llllllllll"
   :subscribed-channels '(hoge fuga)))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(provide 'communication)
;;; communication.el ends here.
