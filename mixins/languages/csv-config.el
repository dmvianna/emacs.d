;;; csv-config.el --- Summary
;;; Commentary:
;;; Display CSV files nicely
;;; Code:

;; define a function to colourise columns
;; from https://www.reddit.com/r/emacs/comments/26c71k/comment/chq2r8m
(require 'cl)
(require 'color)

(defun csv-highlight (&optional separator)
  "Display csv columns in different colours.

SEPARATOR most likely a comma."
  (interactive (list (when current-prefix-arg (read-char "Separator: "))))
  (font-lock-mode 1)
  (let* ((separator (or separator ?\,))
         (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
         (colors (loop for i from 0 to 1.0 by (/ 2.0 n)
                       collect (apply #'color-rgb-to-hex
                                      (color-hsl-to-rgb i 0.3 0.5)))))
    (loop for i from 2 to n by 2
          for c in colors
          for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
          do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))

;; now define how to use csv-mode
(use-package csv-mode
  :hook
  (csv-mode . csv-highlight)
  (csv-mode . csv-align-mode)
  (csv-mode . (lambda () ( ;; this is has order and type, leave it alone
                          ;; and also relies on (setq truncate-partial-width-windows nil)
                          ;; disable line wrapping in csv-mode
                           (visual-line-mode -1)
                           (setq truncate-lines 1))))
  :mode "\\.csv\\'")

(provide 'csv-config)
;;; csv-config.el ends here
