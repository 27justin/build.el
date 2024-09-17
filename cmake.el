;;; cmake.el --- Build CMake projects in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/build.el
;; Package-Requires: ((seq))
;; Version: 0.1
;; Keywords: compile, build-system, cmake

;;; Commentary:

;;; Requirements:

(require 'build-api)
(require 'seq)

;;; Code

(defun cmake-project-p ()
  (build--project-file-exists-p "CMakeLists.txt"))

(defun cmake/build (&optional build-directory)
  "Run CMake build with the provided OPTIONS or default to '--build build'."
  (interactive
   (list (transient-arg-value "-B=" (transient-args 'cmake/transient))))
  (let* ((default-directory (project-root (project-current)))
         (build-command (if build-directory
                            (format "cmake --build %s" build-directory)
                          "cmake --build build")))
    ;; If options are provided, use them, otherwise default to `--build build`
    (funcall build--compile build-command)))

(defun cmake/generate (&optional defines)
  "Run CMake generate with the provided OPTIONS or default to `-S . -B build`."
  (interactive
   (list (transient-args 'cmake/transient)))

  (let ((default-directory (project-root (project-current))))
    ;; If options are provided, use them, otherwise default to `-S . -B build`
    (let ((generate-command (if defines
                                (format "cmake %s" (string-join defines " "))
                              "cmake -S . -B build")))
      (funcall build--compile generate-command))))

(transient-define-prefix cmake/transient ()
  "CMake Build Commands"
  :value '("-S=." "-B=build" "--defines=-DCMAKE_BUILD_TYPE=Release")
  ["CMake Options\n"
   ["Generic"
    ("-S" "Set source directory" "-S=" :prompt "Path to source: ")
    ("-B" "Set build directory" "-B=" :prompt "Path to build: ")
    ("-D" "Defines" " " :prompt "Set defines: " :class transient-option :always-read t)
    ]
   ]
  ["Build"
   ("g" "Generate" cmake/generate)
   ("b" "Build" cmake/build)
   ])

(add-to-list 'build--systems '(cmake-project-p . cmake/transient))
(provide 'cmake)
