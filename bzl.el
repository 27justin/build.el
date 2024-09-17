;;; bzl.el --- Build Bazel projects in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/build.el
;; Version: 0.1
;; Keywords: compile, build-system, bazel

;;; Commentary:

;; This package implements a transient menu for the bazel build system.
;; Alongside three interactive commands.

;;; Requirements

(require 'build-api)

;;; Code

(defun bzl-project-p ()
       (if (project-current)
             (file-exists-p (format "%s/BUILD" (project-root (project-current))))
             nil))

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

(defun bzl/build (&optional args)
       "`bazel build' a target"
       (interactive
       (list (transient-args 'bzl/transient)))
       (bzl--get-targets (lambda(targets)
                         (let* ((choice (funcall build--completing-read "Target: " targets)))
                               (compile (format "bazel build %s %s" choice (string-join args " ")))))
                         "//..."))

(defun bzl/run (&optional args)
       "`bazel run' a target"
       (interactive
       (list (transient-args 'bzl/transient)))
       (bzl--get-targets (lambda(targets)
                         (let* ((choice (funcall build--completing-read "Target: " targets)))
                               (compile (format "bazel run %s %s" choice (string-join args " ")))))
                         "kind(\".*_binary|oci_tarball|container_image|.*_deploy\", //...)"))

(defun bzl/test (&optional args)
       "`bazel test' a target"
       (interactive
       (list (transient-args 'bzl/transient)))
       (bzl--get-targets (lambda(targets)
                         (let* ((choice (funcall build--completing-read "Target: " targets)))
                               (compile (format "bazel test %s %s" choice (string-join args " ")))))
                         "kind(\".*_test\", //...)"))

(with-eval-after-load 'transient
  ;; Bazel transient definition
  (transient-define-prefix bzl/transient ()
    "Bazel Build Commands"
    :value '("-c fastbuild")
    ["Bazel Options\n"
        ["Generic"
            ("-s" "Sandbox debug" "--sandbox_debug")
            ("-c" "Profile" "-c " :choices ("fastbuild" "opt" "dbg") :always-read t :class transient-option)
            ("-o" "Output base" "--output_base=" :prompt "Folder: " :always-read t)
        ]
        ["Test related"
            ("-to" "Test output" "--test_output=" :choices ("summary" "all" "errors" "streamed") :always-read t)
            ("-tc" "Cache test results" "--cache_test_results=" :choices ("no" "yes") :always-read t)
        ]
    ]
    [""
        ["Build"
            ("b" "Build" bzl/build)]
        ["Test"
            ("t" "Test" bzl/test)]
        ["Run"
            ("r" "Run" bzl/run)]
    ]))

(add-to-list 'build--systems '(bzl-project-p . bzl/transient))

(provide 'bzl)
