;;; meson.el --- Build Meson projects in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/build.el
;; Version: 0.1
;; Keywords: compile, build-system, bazel

;;; Commentary:

;; This package implements a transient menu for the bazel build system.
;; Alongside three interactive commands.

;;; Requirements

(require 'build-api)

;;; Code

(defun ninja/build (&optional args)
  (interactive
   (list (transient-args transient-current-command)))
  (let* (
         (arguments (string-replace "-C=" "-C " (string-join args " ")))
         ;; Replace -C= with -C since ninja doesn't like the equal sign.
         (command (format "ninja %s" arguments)))
    (funcall build--compile command)))

(provide 'ninja)
