;;; build.el --- Hassle-free build systems in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/build.el
;; Version: 0.1
;; Package-Requires: ((emacs "?"))
;; Keywords: compile, build-system, bazel

;;; Commentary:

;; This package implements transient menus for multiple build systems.

;;; Requirements
(require 'build-api)
(require 'bzl)
(require 'make)
(require 'cmake)
(require 'cargo)

(provide 'build)
