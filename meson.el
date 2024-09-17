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
(require 'ninja)

;;; Code

(defun meson-project-p ()
       (if (project-current)
             (file-exists-p (format "%s/meson.build" (project-root (project-current))))
             nil))

(defun meson/setup (&optional directory)
       "`meson setup' a target"
       (interactive
       (list (transient-arg-value "-C=" (transient-args 'meson/transient))))
       (princ (transient-args 'meson/transient))
       (compile (format "meson setup %s" directory)))

(with-eval-after-load 'transient
  ;; Bazel transient definition
  (transient-define-prefix meson/transient ()
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
            ("s" "Setup" meson/setup)]
        ["Build"
            ("b" "Build (Ninja)" ninja/build)]
    ]))

(add-to-list 'build--systems '(meson-project-p . meson/transient))

(provide 'meson)
