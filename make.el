;;; make.el --- Build Makefile projects in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/bzl.el
;; Version: 0.1
;; Package-Requires: ((emacs "?"))
;; Keywords: compile, build-system, makefile

;;; Commentary:

;;; Requirements:

(require 'build-api)

;;; Code

(defun make-project-p ()
       (if (project-current)
             (file-exists-p (format "%s/Makefile" (project-root (project-current))))
             nil))

(defun make--get-targets (callback)
  "Call `callback' asynchronously with all Make targets that match `query'."
  (let ((buffer (generate-new-buffer "*make-targets*")))  ;; Create a temporary buffer to capture output
    (make-process
     :name "Make Targets Process"
     :buffer buffer
     :command '("sh" "-c" "make -qp | awk -F':' '/^[a-zA-Z0-9][^$#\\/\\t=]*:([^=]|$)/ {split($1,A,/ /);for(i in A)print A[i]}' | sort -u")
     :sentinel (lambda (proc event)
                 (when (string= event "finished\n")  ;; Check if the process has finished
                   (with-current-buffer (process-buffer proc)
                     (let ((output (buffer-string)))  ;; Get output from the buffer
                       (funcall callback (split-string output "\n" t)))  ;; Call the callback with results
                     (kill-buffer (process-buffer proc))))))))  ;; Clean up the buffer after processing


(defun make/run (&optional args)
       "`bazel test' a target"
       (interactive
        (list (transient-args 'make/transient))
       )
       (make--get-targets (lambda(targets)
                         (let* ((choice (funcall build--completing-read "Target: " targets)))
                               (compile (format "make %s %s" choice (string-join args " ")))))))

(with-eval-after-load 'transient
  ;; Make transient definition
  (transient-define-prefix make/transient ()
    "Make Commands"
    :value '("-j 1" "-f Makefile")
    ["Make Options\n"
        ["Generic"
            ("-f" "Makefile" "-f " :prompt "Path to Makefile: " :class transient-option)
            ("-j" "Threads" "-j " :prompt "# of threads: " :class transient-option)
            ("v" "Variables" " " :prompt "Override variables: " :class transient-option :always-read t)
        ]
    ]
    [""
        ["Run"
            ("r" "Run" make/run)
        ]
    ]))

(add-to-list 'build--systems '(make-project-p . make/transient))
(provide 'make)
