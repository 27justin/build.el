;;; cmake.el --- Build CMake projects in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/build.el
;; Version: 0.1
;; Keywords: compile, build-system, cmake

;;; Commentary:

;;; Requirements:

(require 'build-api)

;;; Code

(defun cmake-project-p ()
  (if (project-current)
      (file-exists-p (format "%s/CMakeLists.txt" (project-root (project-current))))
    nil))

(defun cmake/build (&optional build-directory)
  "Run CMake build with the provided OPTIONS or default to '--build build'."
  (interactive
   (list (transient-arg-value "-B=" (transient-args 'cmake/transient))))

  (let ((default-directory (project-root (project-current))))
    ;; If options are provided, use them, otherwise default to `--build build`
    (let ((build-command (if build-directory
                             (format "cmake --build %s" build-directory)
                           "cmake --build build")))
      (compile build-command))))

(defun cmake/generate (&optional defines)
  "Run CMake generate with the provided OPTIONS or default to `-S . -B build`."
  (interactive
   (list (transient-args 'cmake/transient)))

  (let ((default-directory (project-root (project-current))))
    ;; If options are provided, use them, otherwise default to `-S . -B build`
    (let ((generate-command (if defines
                                (format "cmake %s" (cmake--strip-defines defines))
                              "cmake -S . -B build")))
      (compile generate-command))))

(defun cmake--strip-defines (args)
  "Remove `--defines` from ARGS list and keep `-D` options intact."
  (let ((result '()))
    (dolist (arg args)
      (push (string-replace "--defines=" "" arg) result))
    (string-join (reverse result) " ")))

(with-eval-after-load 'transient
  (transient-define-argument cmake--defines-argument ()
    "Defines"
    :argument "--defines="
    :shortarg "-D"
    :prompt "Set defines: "
    :always-read t
    :class 'transient-option)

  (transient-define-prefix cmake/transient ()
    "CMake Build Commands"
    :value '("-S=." "-B=build" "--defines=-DCMAKE_BUILD_TYPE=Release")
    ["CMake Options\n"
     ["Generic"
      ("-S" "Set source directory" "-S=" :prompt "Path to source: ")
      ("-B" "Set build directory" "-B=" :prompt "Path to build: ")
      ("-D" "Defines" cmake--defines-argument)
      ]
     ]
    ["Build"
     ("g" "Generate" cmake/generate)
     ("b" "Build" cmake/build)
     ]))

(add-to-list 'build--systems '(cmake-project-p . cmake/transient))
(provide 'cmake)
