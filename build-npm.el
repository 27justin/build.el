;;; npm.el --- Build NPM projects in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/build.el
;; Version: 0.1
;; Keywords: compile, build-system, npm

;;; Commentary:

;; This package implements a transient menu for npm.

;;; Requirements

(require 'build-api)
(require 'json)

;;; Code

(defun build-npm-project-p ()
  (build--project-file-exists-p "package.json"))

(defun build--npm-get-package ()
  (json-read-file (format "%s/package.json" (project-root (project-current)))))

(defun build--npm-get-targets (callback)
  (let* ((package-def (build--npm-get-package))
         (scripts-def (cdr (assoc 'scripts package-def)))
         (scripts '()))
    (seq-do (lambda(tuple)
              (push (car tuple) scripts)) scripts-def)
    (funcall callback scripts)))

(defun build-npm-run ()
  (interactive)
  (build--npm-get-targets (lambda(scripts)
                            (princ scripts)
                            (let ((choice (funcall build--completing-read "Script: " scripts)))

                              (funcall build--compile (format "npm run %s" choice))))))

(defun build-npm-install (&optional candidate)
  (interactive "sPackage: ")
  (funcall build--compile (format "npm install %s %s" (string-join (transient-args 'build-npm-transient) " ") candidate)))

(defun build-npm-uninstall ()
  (interactive)
  (let* ((package (build--npm-get-package))
         (deps (append (cdr (assoc 'dependencies package)) (cdr (assoc 'devDependencies package))))
         (candidate (funcall build--completing-read "Uninstall: " deps)))
    (funcall build--compile (format "npm uninstall %s" candidate))))

;; NPM transient definition
(transient-define-prefix build-npm-transient ()
  "NPM Build Commands"
  ["NPM Options\n"
   ["Install"
    ("-d" "Development dependency" "--save-dev")]]
  ["NPM Build\n"
   ["Run"
    ("r" "Run" build-npm-run)]
   ["Management"
    ("i" "Install" build-npm-install)
    ("u" "Uninstall" build-npm-uninstall)]])

(add-to-list 'build--systems '(build-npm-project-p . build-npm-transient))

(provide 'build-npm)
