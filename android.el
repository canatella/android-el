;;; android.el --- Android helpers. -*- lexical-binding: t; -*-

;; Author: Damien Merenne
;; URL: https://github.com/canatella/android-el
;; Created: 2019-02-07
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4"))
;; Package-Version: 0.9.0
;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Tools to help developping for Android.

;;; Code:
(require 'subr-x)
(require 'seq)
(require 'pbuf)

(defcustom android-sdk-home "~/android-sdk"
  "Path to the Android SDK installation."
  :group 'android
  :type 'directory)

(defcustom android-ndk-home "~/android-ndk"
  "Path to the Android NDK installation."
  :group 'android
  :type 'directory)

(defcustom android-ndk-platform "android-29"
  "The android NDK platform for this project."
  :group 'android
  :type 'string)

(defcustom android-ndk-abi "arm64-v8a"
  "The android NDK platform for this project."
  :group 'android
  :type '(radio
          (const :tag "armeabi")
          (const :tag "armeabi-v7a")
          (const :tag "arm64-v8a")
          (const :tag "x86")
          (const :tag "x86-64")))

(defcustom android-adb-path "adb"
  "The adb executable path."
  :group 'android
  :type 'string)

(defvar android-project-history nil "History list of activities.")

(defvar android-activity-history nil "History list of activities.")

(defvar android-default-tags nil "Tags to use in logcat.")

(defvar android-mandatory-tags '("dalvikvm"
                                 "DEBUG"
                                 "AndroidRuntime"
                                 "TestRunner")
  "The tags that are always displayed in log cat.")

(defun android-shell-command-to-string (command)
  "Run COMMAND, returning output without newline."
  (replace-regexp-in-string
   "\n$" "" (shell-command-to-string command)))

(defun android-ndk-var (name &optional abi platform)
  "Fetch Android ndk variable NAME for application ABI and PLATFORM."
  (android-shell-command-to-string
   (concat "NDK_LIBS_OUT=/dev/null NDK_OUT=/dev/null "
           "NDK_PROJECT_PATH=null make --no-print-dir "
           "-f "  android-ndk-home "/build/core/build-local.mk "
           "APP_BUILD_SCRIPT=/dev/null APP_PLATFORM="
           (or platform android-ndk-platform)
           " APP_ABI="
           (or abi android-ndk-abi)
           " DUMP_"
           name)))

(defun android-ndk-which (name &optional abi platform)
  "Fetch toolchain tool NAME path for application ABI and PLATFORM."
  (concat (android-ndk-var "TOOLCHAIN_PREFIX" abi platform) name))

(defun android-ndk-include-path (&optional abi platform)
  "Fetch NDK headers path for application ABI and PLATFORM."
  (file-name-as-directory (replace-regexp-in-string "-isystem " "" (android-ndk-var "SYSROOT_ARCH_INC_ARG" abi))))

(defun android-ndk-library-path (&optional abi platform)
  "Fetch library path of Android NDK for application ABI and PLATFORM."
  (file-name-as-directory (android-ndk-var "SYSROOT_API_LIB_DIR" abi)))

(defun android-project (&optional buffer)
  "Return the root of an Android project for BUFFER."
  (let ((path (or (buffer-file-name buffer) default-directory)))
    (when path
      (let ((gradle (locate-dominating-file path "build.gradle")))
        (when gradle
          (file-name-directory gradle))))))

(defun android-current-project (&optional project)
  "Return the current PROJECT."
  (if project (file-name-as-directory project)
    (android-project)))

(defun android-buffer-lookup-activity (buffer)
  "Return current activity name fo java BUFFER."
  (goto-char (point-min))
  (when (re-search-forward "class\s+\\(.\+\\)\s+extends\s+.*Activity" nil t)
    (let ((activity (substring-no-properties (match-string 1))))
      (goto-char (point-min))
      (when (re-search-forward "package\s+\\(.\+\\)\s*;" nil t)
        (let ((package (substring-no-properties (match-string 1))))
          (format "%s/.%s" package activity))))))

(defun android-buffer-activity (&optional buffer)
  "Return Android activity name of `current-buffer' or BUFFER.

Returns nil if the buffer is not a java buffer or does not define an activity."
  (with-current-buffer (or buffer (current-buffer))
    (when (eq major-mode 'java-mode)
      (save-excursion
        (android-buffer-lookup-activity buffer)))))

(defun android-project-gradle-command (task &optional project)
  "Return a gradle commandline to execute TASK for the Android PROJECT."
  (let ((gradlew (concat (android-current-project project) "gradlew")))
    (format "%s -p %s %s"
            (if (file-executable-p gradlew) gradlew "gradle")
            (android-current-project project)
            task)))

(defun android-project-compile-command (&optional project)
  "Return a gradle command line to comile Android PROJECT."
  (android-project-gradle-command "assembleDebug" project))

(defun android-project-install-command (&optional project)
  "Return an adb command line to install output of Android PROJECT."
  (android-project-gradle-command "installDebug" project))

(defun android-project-start-activity-command (activity)
  "Return an adb command line to start ACTIVITY."
  (format "adb shell am start -n %s" activity))

(defun android-read-project ()
  "Read an activity string from the minibuffer."
  (let ((default (or (android-project)
                     (car (last android-project-history)))))
    (read-string (format "Project (%s): " default)
                 nil 'android-project-history default)))

;;;###autoload
(defun android-project-compile (project)
  "Compile Android PROJECT."
  (interactive (list (android-read-project)))
  (compile (android-project-compile-command)))

;;;###autoload
(defun android-project-install (project)
  "Install output of android PROJECT."
  (interactive (list (android-read-project)))
  (compile (android-project-install-command)))

(defun android-read-activity ()
  "Read an activity string from the minibuffer."
  (let ((default (or (android-buffer-activity)
                     (car (last android-activity-history)))))
    (read-string (format "Activity (%s): " default)
                 nil 'android-activity-history default)))

;;;###autoload
(defun android-activity-start (activity)
  "Start ACTIVITY."
  (interactive (list (android-read-activity)))
  (compile (android-project-start-activity-command activity)))

;;;###autoload
(defun android-project-compile-and-start (project activity)
  "Compile Android PROJECT and start ACTIVITY."
  (interactive (list (android-read-project)
                     (android-read-activity)))
  (compile (format "%s && %s"
                   (android-project-install-command)
                   (android-project-start-activity-command activity))))

(defun android-read-tags (&optional tags)
  "Read TAGS until an empty string is entered."
  (let ((tag (read-string "Tag (empty to stop): ")))
    (if (string-empty-p tag) tags
      (android-read-tags (cons tag tags)))))

(defvar android-ndk-addr2line-search-path (list (android-ndk-library-path)))

(defvar android-ndk-addr2line-path (android-ndk-which "addr2line")
  "Path to the addr2line tool.")

(defun android-ndk-addr2line-find-binary-default (path list)
  "Match PATH in LIST based on file base name."
  (when list
    (let ((file (concat (car list) (file-name-nondirectory path))))
      (if (file-readable-p file)
          file
        (android-ndk-addr2line-find-binary-default path (cdr list))))))


(defvar android-ndk-addr2line-find-binary
  'android-ndk-addr2line-find-binary-default)

(defvar android-ndk-cache (make-hash-table :test #'equal))

(defun android-ndk-addr2line-find-binary (binary)
  "Find BINARY in PATH.

See `android-ndk-addr2line-find-binary."
  (gethash binary android-ndk-cache
           (let ((result (funcall android-ndk-addr2line-find-binary
                                  binary android-ndk-addr2line-search-path)))
             (puthash binary result  android-ndk-cache)
             result)))

(defun android-ndk-addr2line (file address)
  "Run addr2line to fetch source file and line for FILE and ADDRESS."
  (let ((binary (android-ndk-addr2line-find-binary file)))
    (when binary
      (let* ((command (format "%s -e %s %s"
                              android-ndk-addr2line-path
                              binary address))
             (result (split-string (substring (shell-command-to-string command) 0 -1) ":")))
        (unless (string= (car result) "??")
          (format "%s:%s" (file-truename (car result)) (cadr result)))))))

(defun android-logcat-filter-addr2line (line)
  "Replace stack traces in LINE with addr2line output."
  (when (string-match "\\(.*\\)#[0-9]+ +pc +\\([0-9a-f]+\\) +[^![:space:]]+!?\\([^[:space:]]+\\)\\(.*\\)" line)
    (let ((before (match-string 1 line))
          (address (match-string 2 line))
          (file (match-string 3 line))
          (after (match-string 4 line)))
      (setq line (concat before (android-ndk-addr2line file address) after))))
  line)

(defun android-addr2line (&optional buffer)
  "Try to resolve stack trace in BUFFER using addr2line."
  (interactive "bBuffer: \n")
  (save-excursion
    (with-current-buffer (or buffer (current-buffer))
      (goto-char (point-min))
      (android-logcat-filter-addr2line (point))
      (while (not (zerop (forward-line)))
        (android-logcat-filter-addr2line (point))))))

(defvar android-logcat-tags '())
(defvar android-logcat-process nil)
(defvar android-logcat-line-count 0)

(defun android-logcat-tag-filter-option (tags)
  "Return adb logcat -s option to display only TAGS."
  (if tags
      (concat " -s " (string-join (append android-mandatory-tags tags) " "))
    ""))

(defun android-logcat-start-process (tags name buffer)
  "Return a logcat process with NAME and output in BUFFER filtered with TAGS."
  (make-local-variable 'android-ndk-addr2line-find-binary)
  (make-local-variable 'android-ndk-addr2line-search-path)
  (setq-local android-ndk-addr2line-path (android-ndk-which "addr2line"))
  (let* ((default-directory (or (android-project) default-directory))
         (options (android-logcat-tag-filter-option tags))
         (command (format "%s logcat -v threadtime -T %s %s" android-adb-path pbuf-max-line-count options)))
    (start-process-shell-command "adb-logcat" "*adb-logcat*" command)))

;;;###autoload
(defun android-logcat (&optional tags)
  "Open android logs in a buffer, Possibly limit output to TAGS.

If FILTER is given, append it to the logcat command line."
  (interactive
   (list (let ((read-tags (consp current-prefix-arg)))
           (when (or (not android-logcat-tags) read-tags)
             (setq android-logcat-tags (android-read-tags android-default-tags))
             android-logcat-tags))))
  (let ((pbuf-pre-insert-functions (list #'android-logcat-filter-addr2line)))
    (pbuf-start-process "*adb-logcat*"
                        (apply-partially #'android-logcat-start-process tags))))

;;;###autoload
(defun android-send-clipboard ()
  "Send current kill ring to the Android device clipboard."
  (interactive)
  (let* ((data (current-kill 0 t))
         (size (length data)))
    (call-process "/usr/bin/env" nil "*Messages*" nil "adb" "shell"
                  "service" "call" "clipboard" "2" "i32" "1"
                  "i32" (format "%s" size) "s16" data)))

(provide 'android)
;;; android.el ends here
