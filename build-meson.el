;;; meson.el --- Build Meson projects in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/build.el
;; Version: 0.1
;; Keywords: compile, build-system, meson

;;; Commentary:

;; This package implements a transient menu for the meson build system.
;; Alongside three interactive commands.

;;; Requirements

(require 'build-api)
(require 'build-ninja)

;;; Code

(defun build-meson-project-p ()
  (build--project-file-exists-p "meson.build"))

(defun build-meson-setup (&optional directory)
  "`meson setup' a target"
  (interactive
   (list (transient-arg-value "-C=" (transient-args 'build-meson-transient))))
  (funcall build--compile (format "meson setup %s" directory)))

(transient-define-prefix build-meson-transient ()
  "Meson Build Commands"
  :value '("build")
  ["Meson Options\n"
   ["Generic"
    ("-C" "Build directory" "-C=" :prompt "Build directory: " :class transient-option :always-read t)
    ]
   ["Ninja Options"
    ("-j" "Threads" "-j " :prompt "# of threads: " :class transient-option)
    ("-n" "Dry run" "-n")
    ]
   ]
  [""
   ["Setup"
    ("s" "Setup" build-meson-setup)]
   ["Build"
    ("b" "Build (Ninja)" build-ninja-build)]
   ])

(add-to-list 'build--systems '(build-meson-project-p . build-meson-transient))

(provide 'build-meson)
