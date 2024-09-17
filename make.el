;;; make.el --- Build Makefile projects in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Andreas Lacoste

;; Author: Justin Andreas Lacoste <me@justin.cx>
;; URL: https://github.com/27justin/build.el
;; Version: 0.1
;; Keywords: compile, build-system, makefile
;;; Commentary:

;;; Requirements:

(require 'build-api)
(require 'make-mode)

;;; Code

(defun make-project-p ()
  (build--project-file-exists "Makefile"))

(defun make--get-targets (callback)
  "Call `callback' asynchronously with all Make targets that match `query'."
  (with-current-buffer (find-file (format "%s/Makefile" (project-root (project-current))))
    (setq-local makefile-need-target-pickup t)
    (makefile-pickup-targets)
    (funcall callback (flatten-list makefile-target-table))))

(defun make/run (&optional args)
  "`bazel test' a target"
  (interactive
   (list (transient-args 'make/transient))
   )
  (make--get-targets (lambda(targets)
                       (let* ((choice (funcall build--completing-read "Target: " targets)))
                         (funcall build--compile (format "make %s %s" choice (string-join args " ")))))))

(with-eval-after-load 'transient
  ;; Make transient definition
  (transient-define-prefix make/transient ()
    "Make Commands"
    :value '("-j 1" "-f Makefile")
    ["Make Options\n"
     ["Generic"
      ("-f" "Makefile" "-f " :prompt "Path to Makefile: " :class transient-option)
      ("-j" "Threads" "-j " :prompt "# of threads: " :class transient-option :always-read t)
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
