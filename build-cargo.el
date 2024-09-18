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

(defun build-cargo-project-p ()
  (build--project-file-exists-p "Cargo.toml"))

(defun build-cargo-build (&optional args)
  "Run cargo build with the provided ARGS."
  (interactive
   (list (transient-args 'build-cargo-transient)))
  (funcall build--compile (format "cargo build %s" (string-join args " "))))

(defun build-cargo-run (&optional args)
  "Run cargo run with the provided ARGS."
  (interactive
   (list (transient-args 'build-cargo-transient)))
  (funcall build--compile (format "cargo run %s" (string-join args " "))))
(with-eval-after-load 'transient
  (transient-define-prefix build-cargo-transient ()
    "Cargo Build Commands"
    :value '("--debug")
    :incompatible '(("--release" "--profile ")
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
      ("b" "Build" build-cargo-build)
      ]
     ["Run"
      ("r" "Run" build-cargo-run)
      ]
     ]))

(add-to-list 'build--systems '(build-cargo-project-p . build-cargo-transient))

(provide 'build-cargo)
