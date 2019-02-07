;;; test-helper.el --- Helpers for android-test.el

;;; Commentary:

;;; Code:

(defconst android-test-path (file-name-as-directory
                               (file-name-directory (or load-file-name buffer-file-name)))
  "The test directory.")

(defconst android-root-path (file-name-as-directory
                               (file-name-directory
                                (directory-file-name android-test-path)))
  "The android project root path.")
(add-to-list 'load-path android-root-path)

(defun android-emacs-init (action)
  "Initialize Emacs with Cask packages an invoke ACTION."
  (let* ((load-prefer-newer t)
         (source-directory (locate-dominating-file android-test-path "Cask"))
         (pkg-rel-dir (format ".cask/%s.%S/elpa" emacs-major-version emacs-minor-version)))

    (setq package-user-dir (expand-file-name pkg-rel-dir source-directory))
    (package-initialize)

    (message "Running tests on Emacs %s, built at %s"
             emacs-version (format-time-string "%F" emacs-build-time))
    (require 'android)
    (require 'elisp-lint)
    (funcall action)))

(defun android-lint-files ()
  "Main entry point for linter."
  (android-emacs-init
   (lambda ()
     (add-hook 'emacs-lisp-mode-hook
               (lambda ()
                 (setq indent-tabs-mode nil)
                 (setq fill-column 80)))
     (let ((debug-on-error t))
       (elisp-lint-files-batch)))))

(defun android-run-tests ()
  "Main entry point for linter."
  (android-emacs-init
   (lambda ()
     (let ((tests (directory-files "./test" t "test.el")))
       (while tests
         (load-file (car tests))
         (setq tests (cdr tests))))
     (let ((debug-on-error t))
       (ert-run-tests-batch-and-exit)))))

(defun android-package-lint ()
  "Checks that the metadata in Emacs Lisp files which to ensure they are intended to be packages."
  (android-emacs-init
   (lambda ()
     (eval-after-load 'flycheck
'(flycheck-package-setup)))))
;;; test-helper.el ends here
