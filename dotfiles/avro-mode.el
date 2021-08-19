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

(require 'generic-x)

(defun make-avro-keywords (types)
  (concat "\\<" (regexp-opt types) "\\>"))

(setq avro-primitive-types
      (make-avro-keywords
       '("null" "boolean" "int" "long" "float" "double" "bytes" "string")))

(setq avro-complex-types
      (make-avro-keywords
       '("record" "enum" "array" "map" "union" "alias")))

(setq avro-reserved-words
      '("protocol" "namespace"))

(setq avro-delimiters
  '("<" ">" ";" "{" "}"))

(setq avro-comment
  '("//"))

(define-generic-mode
    'avro-mode
  avro-comment
  avro-reserved-words
  ;; '(((make-avro-keywords avro-primitive-types) . font-lock-builtin-face)
  ;;   ((make-avro-keywords avro-complex-types) . font-lock-type-face)
  ;;   ((make-avro-keywords avro-reserved-words) . font-lock-keyword-face)
  ;;   ((regexp-opt 'avro-delimiters) . font-lock-operator)
  ;;   (("array") . font-lock-keyword-face)
  ;;   )
 '(("\\<\\(?:b\\(?:oolean\\|ytes\\)\\|double\\|float\\|int\\|long\\|null\\|string\\)\\>"
     . font-lock-builtin-face)
    ("\\<\\(?:a\\(?:lias\\|rray\\)\\|enum\\|map\\|record\\|union\\)\\>" . font-lock-type-face)
    ("\\<\\(?:namespace\\|protocol\\)\\>" . font-lock-keyword-face)
    ("{\\|}\\|;\\|<\\|>" . font-lock-constant-face))
  '("\\.avdl$")
  nil
  "Major mode for editing Avro IDL files."
  )

(provide 'avro-mode)

;;; avro-mode.el ends here
