;;; build-api.el --- Hassle-free build systems in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/build.el
;; Version: 0.1
;; Keywords: compile, build-system, bazel

;;; Commentary:

;; This package implements transient menus for multiple build systems.

;;; Requirements

;;; General variables
(defvar build--completing-read  'completing-read
  "Use this variable to override the completion framework.
        I've had issues with vertico, where lots of targets were being displayed
        weirdly in vertico")

(defvar build--systems '()
  "List of build systems, you can add your own custom build system using 
 `(add-to-list 'build--systems '(my-build-system-p . my-build-system/transient))'")

;;; Code

(defun build-system-p ()
  "Check if any project predicate in `build--systems` returns true."
  (catch 'found
    (dolist (system build--systems)
      (when (funcall (car system))   ;; Call the predicate (the car of each pair)
        (throw 'found t)))           ;; Return t if a match is found
    nil))                             ;; Return nil if no match is found

(with-eval-after-load 'transient
  ;; Define a unified transient for either Bazel or Make
  (transient-define-prefix build/menu ()
    "Project Build Commands"
    (interactive)
    ;; Check which project type to display
    (catch 'found
      (dolist (system build--systems)
        (when (funcall (car system))   ;; Call the predicate (the car of each pair)
          (funcall (cdr system))
          (throw 'found t)))           ;; Return t if a match is found
      (message "No known build system found in this project")))                            ;; Error if no match is found
)

(provide 'build-api)
