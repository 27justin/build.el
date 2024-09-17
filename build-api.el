;;; build-api.el --- Hassle-free build systems in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/build.el
;; Package-Requires: ((transient) (seq))
;; Version: 0.1
;; Keywords: compile, build-system, bazel

;;; Commentary:

;; This package implements transient menus for multiple build systems.

;;; Requirements:

(require 'project)
(require 'transient)
(require 'seq)

;;; General variables:

(defgroup build nil
  "Customization options for the build system integration."
  :group 'tools
  :prefix "build--"
  :tag "Build System")

(defcustom build--completing-read 'completing-read
  "Use this variable to override the completion framework.
I've had issues with Vertico, where lots of targets were being displayed
weirdly."
  :type '(function)
  :group 'build)

(defcustom build--compile 'compile
  "Override the compilation command."
  :type '(function)
  :group 'build)

(defvar build--systems '()
  "List of build systems, you can add your own custom build system using
   `(add-to-list 'build--systems
                '(my-build-system-p . my-build-system/transient))'")

;;; Code:

(defun build--project-file-exists-p (file)
  (and (project-current)
       (file-exists-p (format "%s/%s" (project-root (project-current)) file))))

(defun build-system-p ()
  "Check if any project predicate in `build--systems` returns true."
  (seq-some (lambda (system) (funcall (car system))) build--systems))

;; Define a unified transient for either Bazel or Make
(transient-define-prefix build/menu ()
  "Project Build Commands"
  (interactive)
  ;; Check which project type to display
  (seq-some (lambda (system)
              (when (funcall (car system))
                ;; Run the transient when our build-system predicate matches.
                (funcall (cdr system))))
            build--systems))

(provide 'build-api)
;;; build-api.el ends here
