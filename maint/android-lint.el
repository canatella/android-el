;;; android-lint.el --- Script to check the package for syntax errors  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Luis Gerhorst

;; Author: Luis Gerhorst <privat@luisgerhorst.de>
;; Keywords: lisp, maint

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Code to check for syntax errors.  See Makefile for usage.

;;; Code:

(eval-when-compile
  (defvar checkdoc-force-docstrings-flag)
  (defvar checkdoc-arguments-in-order-flag) ;default changed 26.1
  (defvar checkdoc-verb-check-experimental-flag)
  (defvar elisp-lint-ignored-validators))
(declare-function elisp-lint-files-batch "elisp-lint")
(declare-function flycheck-package-setup "flycheck-package")

(defconst android-test-path
  (file-name-as-directory
   (file-name-directory (or load-file-name buffer-file-name))))
(defconst android-root-path
  (file-name-as-directory
   (file-name-directory
    (directory-file-name android-test-path))))

(add-to-list 'load-path android-test-path)
(add-to-list 'load-path android-root-path)

(defun android-emacs-init (action)
  "Initialize Emacs with Cask packages an invoke ACTION."
  (let* ((load-prefer-newer t)
         (source-directory (locate-dominating-file android-test-path "Cask"))
         (pkg-rel-dir
          (format ".cask/%s.%S/elpa" emacs-major-version emacs-minor-version)))

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
     (setq elisp-lint-ignored-validators '("package-format"
                                           ;; "fill-column"
                                           "byte-compile"
                                           "indent"))
     (add-hook 'emacs-lisp-mode-hook
               (lambda ()
                 (setq indent-tabs-mode nil)
                 (setq fill-column 85)
                 (setq checkdoc-force-docstrings-flag nil
                       checkdoc-arguments-in-order-flag nil ;default changed 26.1
                       checkdoc-verb-check-experimental-flag nil)))
     (let ((debug-on-error t))
       (elisp-lint-files-batch)))))

(defun android-package-lint ()
  "Checks that the metadata in Emacs Lisp files which to ensure they are
intended to be packages."
  (android-emacs-init
   (lambda ()
     (eval-after-load 'flycheck
       '(flycheck-package-setup)))))

(provide 'android-lint)
;;; android-lint.el ends here
