;;; cargo.el --- Build Cargo projects in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/build.el
;; Version: 0.1
;; Keywords: compile, build-system, cargo

;;; Commentary:

;;; Requirements

(require 'build-api)

;;; Code

(defun cargo-project-p ()
       (if (project-current)
             (file-exists-p (format "%s/Cargo.toml" (project-root (project-current))))
             nil))

(defun cargo/build (&optional args)
  "Run cargo build with the provided ARGS."
  (interactive
   (list (transient-args 'cargo/transient)))
   (compile (format "cargo build %s" (string-join args " "))))

(defun cargo/run (&optional args)
  "Run cargo run with the provided ARGS."
  (interactive
   (list (transient-args 'cargo/transient)))
   (compile (format "cargo run %s" (string-join args " "))))
(with-eval-after-load 'transient
  (transient-define-prefix cargo/transient ()
    "Cargo Build Commands"
    :value '("--debug")
    :incompatible '(
      ("--release" "--profile ")
      ("--lib" "--bins" "--examples"))
    ["Cargo Options\n"
        ["Generic"
            ("-r" "Release" "--release")
            ("-l" "Build library" "--lib")
            ("-b" "Build binaries" "--bins")
            ("-e" "Build examples" "--examples")
        ]
        ["Profile"
          ("-p" "Set profile" "--profile " :always-read t :class transient-option)
          ("-t" "Target triple" "--target " :always-read t :class transient-option)
        ]
    ]
    [""
        ["Build"
            ("b" "Build" cargo/build)
        ]
        ["Run"
            ("r" "Run" cargo/run)
        ]
    ]))

(add-to-list 'build--systems '(cargo-project-p . cargo/transient))

(provide 'cargo)
