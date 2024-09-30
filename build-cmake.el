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

(defvar build--cmake-generators '(
                                  "Unix Makefiles"
                                  "NMake Makefiles"
                                  "MinGW Makefiles"
                                  "Ninja"
                                  "Visual Studio 16 2019"
                                  "Visual Studio 17 2022"
                                  )
  "A list of default options for the generator option of the
CMake transient")

(defun build-cmake-project-p ()
  (build--project-file-exists-p "CMakeLists.txt"))

(defun build--cmake-strip-arguments (list args)
  "Strip elements in `args' from `list` using fuzzy matching.
If an element in `list` starts with any string in `args`, it will be stripped."
  (let ((results '()))
    (seq-do (lambda (item)
              (unless (seq-some (lambda (arg)
                                  (string-prefix-p arg item))
                                args)
                (push item results)))
            list)
    (reverse results)))

(defun build-cmake-build (&optional args)
  "Run CMake build with the provided OPTIONS or default to '--build build'."
  (interactive
   (list (transient-args 'build-cmake-transient)))
  (let* ((default-directory (project-root (project-current)))
         (build-command (format "cmake --build %s %s" (transient-arg-value "-B=" args) (string-join (build--cmake-strip-arguments args '("-B=" "-S=" "-G=" " -D")) " "))))
    (funcall build--compile build-command)))

(defun build-cmake-generate (&optional args)
  "Run CMake generate with the provided ARGS."
  (interactive
   (list (transient-args 'build-cmake-transient)))
  (let ((default-directory (project-root (project-current))))
    ;; Process arguments
    (let ((processed-args (mapcar (lambda (arg)
                                    (if (string-prefix-p "-G=" arg)
                                        ;; Wrap the value after '-G=' in quotes if it's a generator
                                        (concat "-G=\"" (substring arg 3) "\"")
                                      arg))
                                  (build--cmake-strip-arguments args '("--clean-first" "--target")))))
      ;; Join the processed args into a single string and run cmake
      (let ((generate-command (format "cmake %s" (string-join processed-args " "))))
        (funcall build--compile generate-command)))))

(transient-define-prefix build-cmake-transient ()
  "CMake Build Commands"
  :value '("-S=." "-B=build" "--defines=-DCMAKE_BUILD_TYPE=Release" "-G=Unix Makefiles")
  ["CMake Options\n"
   ["Generating"
    ("-S" "Set source directory" "-S=" :prompt "Path to source: ")
    ("-B" "Set build directory" "-B=" :prompt "Path to build: ")
    ("-D" "Defines" " " :prompt "Set defines: " :class transient-option :always-read t)
    ("-G" "Generator" "-G=" :prompt "Build generator: " :choices (lambda() build--cmake-generators) :always-read t)
    ]
   ["Building"
    ("-C" "Clean first" "--clean-first") ;; Only for building, stripped from generate.
    ]
   ]
  ["Build"
   ("g" "Generate" build-cmake-generate)
   ("b" "Build" build-cmake-build)
   ])

(add-to-list 'build--systems '(build-cmake-project-p . build-cmake-transient))
(provide 'build-cmake)
