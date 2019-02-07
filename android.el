;;; android.el --- Android helpers.

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

(defcustom android-sdk-home "~/android-sdk"
  "Path to the Android SDK installation."
  :group 'android
  :type 'directory)

(defcustom android-ndk-home "~/android-ndk"
  "Path to the Android NDK installation."
  :group 'android
  :type 'directory)

(defcustom android-ndk-platform "android-21"
  "The android NDK platform for this project."
  :group 'android
  :type 'string)

(defcustom android-ndk-abi "armeabi-v7a"
  "The android NDK platform for this project."
  :group 'android
  :type '(radio
          (const :tag "armeabi")
          (const :tag "armeabi-v7a")
          (const :tag "arm64-v8a")
          (const :tag "x86")
          (const :tag "x86-64")))

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
  (concat (android-ndk-var "SYSROOT_INC" abi) "/usr/include"))

(defun android-ndk-library-path (&optional abi platform)
  "Fetch library path of Android NDK for application ABI and PLATFORM."
  (concat (android-ndk-var "SYSROOT_INC" abi) "/usr/lib"))

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

(defun android-logcat-filter-carriage-returns (pos)
  "Process filter to remove carriage return up to POS."
  (save-excursion
    (while (search-backward "\r" pos t)
      (delete-char 1))))

(defvar android-ndk-addr2line-search-path (list (android-ndk-library-path)))

(defvar android-ndk-addr2line-path (android-ndk-which "addr2line")
  "Path to the addr2line tool.")

(defun android-ndk-addr2line-find-binary-default (path list)
  "Match PATH in LIST based on file base name."
  (message "finding %s in %s" path list)
  (let ((binaries (seq-map (lambda (p)
                             (format "%s%s"
                                     (file-name-as-directory p)
                                     (file-name-nondirectory path)))
                           list)))
    (seq-find 'file-readable-p binaries)))

(defvar android-ndk-addr2line-find-binary
  'android-ndk-addr2line-find-binary-default)

(defun android-ndk-addr2line (file address)
  "Run addr2line to fetch source file and line for FILE and ADDRESS."
  (let* ((binary (funcall android-ndk-addr2line-find-binary
                          file android-ndk-addr2line-search-path))
         (command (format "%s -e %s %s"
                          android-ndk-addr2line-path
                          binary address))
         (result (split-string (shell-command-to-string command) ":")))
    (unless (string= (car result) "??")
      (cons (file-truename (car result)) (cadr result)))))

(defun android-logcat-filter-addr2line (pos)
  "Process filter to replace stack traces with addr2line output up to POS."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (while (re-search-forward
            "#[0-9]\+ +pc +\\([0-9a-f]+\\) +\\([^[:space:]]+\\)" nil t)
      (let* ((data (match-data))
             (address (match-string 1))
             (file (match-string 2))
             (dest (android-ndk-addr2line file address)))
        (when dest
          (let ((replacement (format "%s:%s" (car dest) (cdr dest))))
            (set-match-data data)
            (add-text-properties 0 (length (car dest))
                                 '(face font-lock-warning-face) replacement)
            (add-text-properties (+ (length (car dest)) 1)
                                 (-(length replacement) 1)
                                 '(face font-lock-warning-face) replacement)
            (replace-match replacement t t)
            ))))))

(defun android-logcat-filter (proc string)
  "Symbolize stacktraces in PROC output STRING with addr2line."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (moving (= (point) (process-mark proc)))
            (last (process-mark proc))
            (android-ndk-addr2line-path (android-ndk-which "addr2line")))
        (save-excursion
          (goto-char last)
          (insert string)
          (android-logcat-filter-carriage-returns last)
          (android-logcat-filter-addr2line last)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun android-addr2line (&optional buffer)
  "Try to resolve stack trace in BUFFER using addr2line."
  (interactive "bBuffer: \n")
  (save-excursion
    (with-current-buffer (or buffer (current-buffer))
      (message "ADDR2LINE is %s" android-ndk-addr2line-path)
      (goto-char (point-min))
      (android-logcat-filter-addr2line (point))
      (while (not (zerop (forward-line)))
        (android-logcat-filter-addr2line (point))))))

(defvar android-logcat-tags '())
(defvar android-logcat-process nil)

(defun android-logcat-tag-filter-option (tags)
  "Return adb logcat -s option to display only TAGS."
  (if tags
      (concat " -s " (string-join (append android-mandatory-tags tags) " "))
    ""))

;;;###autoload
(defun android-logcat (&optional tags)
  "Open android logs in a buffer, Possibly limit output to TAGS.

If FILTER is given, append it to the logcat command line."
  (interactive
   (list (let ((read-tags (consp current-prefix-arg)))
           (when (or (not android-logcat-tags) read-tags)
             (setq android-logcat-tags (android-read-tags android-default-tags))
             android-logcat-tags))))
  (with-current-buffer (get-buffer-create "*adb-logcat*")
    (if (process-live-p android-logcat-process)
        (kill-process android-logcat-process))
    (read-only-mode)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))
    (goto-char (point-max))
    (let* ((default-directory (or (android-project) default-directory))
           (options (android-logcat-tag-filter-option tags))
           (command (concat "adb logcat -v threadtime" options))
           (process (start-process-shell-command
                     "adb-logcat" "*adb-logcat*" command)))
      (set-process-filter process 'android-logcat-filter)
      (message "running %s" command)
      (setq-local android-logcat-process process))))

(defun android-logcat-clean ()
  "Reset the logcat buffer."
  (when (get-buffer "*adb-logcat*")
    (with-current-buffer "*adb-logcat*"
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (goto-char (point-max))
        (if (process-live-p android-logcat-process)
            (set-marker (process-mark android-logcat-process) (point)))))))

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
