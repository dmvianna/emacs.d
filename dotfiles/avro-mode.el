;;; avro-mode.el -- major mode for editing Avro IDL files. -*- coding: utf-8; lexical-binding: t; -*-

;;; Copyright © by Daniel Vianna
;;; Version: 0.0.1
;;; Created: 19 Aug 2021

;;; Keywords: languages

;;; This file is not part of GNU Emacs.

;;; License:

;;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Package --- summary
;;; Commentary:
;;;
;;; Avro interface description language mode (.avdl)
;;; Code:

;;; old
(setq avro-primitive-types
  '("null" "boolean" "int" "long" "float" "double" "bytes" "string"))

(setq avro-complex-types
  '("record" "enum" "array" "map" "union" "alias"))

(setq avro-reserved-words
      '("protocol" "namespace"))

(setq avro-delimiters
  '("<" ">" ";" "{" "}"))

(defun make-avro-keywords (types)
  (concat "\\<" (regexp-opt types) "\\>"))

(setq avro-comments
  "//.*")

(setq avro-highlights
  '(((make-avro-keywords avro-primitive-types) . font-lock-builtin-face)
    ((make-avro-keywords avro-complex-types) . font-lock-type-face)
    ((make-avro-keywords avro-reserved-words) . font-lock-keyword-face)
    ((regexp-opt 'avro-delimiters) . font-lock-constant-face)
    (avro_comments . font-lock-comment-face)
    ))

(define-derived-mode avro-mode fundamental-mode "Avro IDL"
  "major mode for editing Avro IDL files."
  (setq font-lock-defaults '(avro-highlights)))

;;; autoload
(add-to-list 'auto-mode-alist '("\\.avdl\\'" . avro-mode))

(provide 'avro-mode)

;;; avro-mode.el ends here
