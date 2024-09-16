;;; bzl.el --- Build Bazel projects in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/bzl.el
;; Version: 0.1
;; Package-Requires: ((emacs "?"))
;; Keywords: compile, build-system, bazel

;;; Commentary:

;; This package exposes three simple commands to build bazel projects through read-multiple-choice and compile.

;;; General variables
(defvar bzl--completing-read  'completing-read)
        ;; Use this variable to override the completion framework.
        ;; I've had issues with vertico, where lots of targets were being displayed
        ;; weirdly in vertico

;;; Code

(defun bzl--get-targets (callback query)
  "Call `callback' with all bazel targets that match `query'"
  (let* ((buffer (get-buffer-create "*bazel-query*" t))  ;; Create a persistent buffer
         (base-command (list "bazel" "query" query "--keep_going")))
    (with-current-buffer buffer
                         (erase-buffer))
    (make-process
     :name "Bazel Query"
     :buffer buffer
     :command base-command
     :filter (lambda (proc out)
               ;; Process each line of the output individually
               (with-current-buffer (process-buffer proc)
                 (dolist (line (split-string out "\n" t))  ;; Split output into lines
                   (when (string-match "^//" line)         ;; Filter out lines that do not start with "//"
                     (insert line "\n")))))                ;; Insert line if it matches

     :sentinel (lambda (proc msg)
               (funcall callback (with-current-buffer (process-buffer proc)
                                                      (split-string (buffer-string) "\n" t)))))))

(defun bzl/build ()
       "`bazel build' a target"
       (interactive)
       (bzl--get-targets (lambda(targets)
                         (let* ((choice (funcall bzl--completing-read "Target: " targets)))
                               (compile (format "bazel build %s" choice))))
                         "//..."))

(defun bzl/run ()
       "`bazel run' a target"
       (interactive)
       (bzl--get-targets (lambda(targets)
                         (let* ((choice (funcall bzl--completing-read "Target: " targets)))
                               (compile (format "bazel run %s" choice))))
                         "kind(\".*_binary|oci_tarball|container_image|.*_deploy\", //...)"))

(defun bzl/test ()
       "`bazel test' a target"
       (interactive)
       (bzl--get-targets (lambda(targets)
                         (let* ((choice (funcall bzl--completing-read "Target: " targets)))
                               (compile (format "bazel test %s" choice))))
                         "kind(\".*_test\", //...)"))

(provide 'bzl)
