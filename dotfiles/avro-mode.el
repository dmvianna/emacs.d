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
(defvar avro-primitive-types
  '("null" "boolean" "int" "long" "float" "double" "bytes" "string"))

(defvar avro-complex-types
  '("record" "enum" "array" "map" "union" "alias"))

(defvar avro-delimiters
  '("<" ">" ";" "{" "}"))

(defvar avro-custom-types
  '("<([^<>]+)>|record ([[:upper:]][[:alnum:]_]*)"))

(defvar avro-comments
  '("//.*" "\""))

(defvar avro-highlights
  '(((regexp-opt avro-primitive-types 'words) . font-lock-function-name-face)
    ((regexp-opt avro-complex-types 'words) . font-lock-builtin-face)
    ((regexp-opt avro-delimiters 'words) . font-lock-constant-face)
    ((avro-custom-types 1) . font-lock-type-face)
    ((avro-comments 'words) . font-lock-comment-face)))

(define-derived-mode avro-mode prog-mode "Avro IDL"
  "major mode for editing Avro IDL files."
  (setq font-lock-defaults '(avro-highlights)))

(provide 'avro-mode)

;;; avro-mode.el ends here
