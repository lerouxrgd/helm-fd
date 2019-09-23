;;; helm-fd.el --- Helm interface for fd command. -*- lexical-binding: t -*-

;; Author: Romain Leroux <romain@leroux.dev>
;; URL: https://github.com/lerouxrgd/helm-fd
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.2") (helm "3.3"))
;; Keywords: fd, find, files, helm, fast

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Same as `helm-find' but using fd instead of find.  Also provides a
;;; project scoped search function `helm-fd-project'.

;;; Code:

(require 'helm)
(require 'helm-files)

(defgroup helm-fd nil
  "Group for `helm-fd' customizations."
  :group 'helm)

(defcustom helm-fd-cmd "fd"
  "The name to use for finding the fd executable binary."
  :group 'helm-fd
  :type 'string)

(defcustom helm-fd-bin (executable-find helm-fd-cmd)
  "The location of the fd executable binary."
  :group 'helm-fd
  :type 'string)

(defcustom helm-fd-noerrors nil
  "Prevent showing error messages in helm buffer when non nil."
  :group 'helm-fd
  :type 'boolean)

(defcustom helm-fd-relative-paths nil
  "Use relative path from `default-directory' when displaying files."
  :group 'helm-fd
  :type 'boolean)

(defvar helm-fd-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-generic-files-map)
    (define-key map (kbd "DEL") 'helm-delete-backward-no-update)
    map))

(defvar helm-fd-source
  (helm-build-async-source "fd"
    :header-name (lambda (name)
                   (concat name " in [" (helm-default-directory) "]"))
    :candidates-process 'helm-fd-shell-command-fn
    :filtered-candidate-transformer 'helm-fd-transformer
    :action-transformer 'helm-transform-file-load-el
    :persistent-action 'helm-ff-kill-or-find-buffer-fname
    :action 'helm-type-file-actions
    :help-message 'helm-generic-file-help-message
    :keymap helm-fd-map
    :candidate-number-limit 9999
    :requires-pattern 3))

(defun helm-fd-transformer (candidates _source)
  "A helm transformer to process fd result CANDIDATES."
  (let (non-essential
        (default-directory (helm-default-directory)))
    (cl-loop for i in candidates
             for abs = (if helm-fd-relative-paths
                           (helm-aif (file-remote-p default-directory)
                               (concat it (file-relative-name i))
                             (file-relative-name i))
                           (expand-file-name
                            (helm-aif (file-remote-p default-directory)
                                (concat it i) i)))
             for type = (car (file-attributes abs))
             for disp = (if (and helm-ff-transformer-show-only-basename
                                 (not (string-match "[.]\\{1,2\\}$" i)))
                            (helm-basename abs) abs)
             collect (cond ((eq t type)
                            (cons (propertize disp 'face 'helm-ff-directory)
                                  abs))
                           ((stringp type)
                            (cons (propertize disp 'face 'helm-ff-symlink)
                                  abs))
                           (t (cons (propertize disp 'face 'helm-ff-file)
                                    abs))))))

(defun helm-fd-shell-command-fn ()
  "Run and supervise the fd subprocess."
  (let* (process-connection-type
         non-essential
         (cmd (concat (helm-fd--build-cmd-line)
                      (if helm-fd-noerrors " 2> /dev/null" "")))
         (proc (start-file-process-shell-command "hfd" helm-buffer cmd)))
    (helm-log "fd command:\n%s" cmd)
    (prog1 proc
      (set-process-sentinel
       proc
       (lambda (process event)
         (helm-process-deferred-sentinel-hook
          process event (helm-default-directory))
         (if (string= event "finished\n")
             (with-helm-window
               (setq mode-line-format
                     '(" " mode-line-buffer-identification " "
                       (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                       (:eval (propertize
                               (format "[fd process finished - (%s results)]"
                                       (max (1- (count-lines
                                                 (point-min) (point-max)))
                                            0))
                               'face 'helm-locate-finish))))
               (force-mode-line-update))
           (helm-log "Error: fd %s"
                     (replace-regexp-in-string "\n" "" event))))))))

(defun helm-fd--build-cmd-line ()
  "Build the fd command line from current `helm-pattern'."
  (let* ((default-directory (or (file-remote-p default-directory 'localname)
                                default-directory))
         (patterns+options (split-string helm-pattern "\\(\\`\\| +\\)\\* +"))
         (patterns (split-string (car patterns+options)))
         (additional-options (and (cdr patterns+options)
                                  (list (concat (cadr patterns+options) " ")))))
    (concat helm-fd-bin " "
            (string-join patterns ".*?") " "
            (shell-quote-argument (expand-file-name default-directory)) " "
            (string-join additional-options " "))))

(defun helm-fd-1 (dir)
  "Setup helm fd source and start it from DIR."
  (let ((default-directory (file-name-as-directory dir)))
    (helm :sources 'helm-fd-source
          :buffer "*helm fd*"
          :ff-transformer-show-only-basename nil
          :case-fold-search helm-file-name-case-fold-search)))

;;;###autoload
(defun helm-fd-project ()
  "Preconfigured `helm-fd' for search within the current project.

If the current file isn't part of a VCS project,
`default-directory' will be the location where the current Emacs
process was started.

Also note that the displayed paths are relative to project root."
  (interactive)
  (let ((directory
         (or (cdr (project-current))
             (with-current-buffer "*Messages*" default-directory)))
        (helm-fd-relative-paths t))
    (helm-fd-1 directory)))

;;;###autoload
(defun helm-fd (arg)
  "Preconfigured `helm' for the fd shell command.

Recursively find files whose names are matched by all specified
globbing PATTERNs under the current directory using the external
program \"fd\" specified in `helm-fd-cmd'.  Input PATTERNs are
interleaved with \".*?\".

With prefix argument ARG, prompt for a directory to search.

The list of globbing PATTERNs can be followed by the separator
\"*\" plus any number of additional arguments that are passed to
\"fd\" literally."
  (interactive "P")
  (let ((directory
         (if arg
             (file-name-as-directory
              (read-directory-name "DefaultDirectory: "))
           default-directory)))
    (helm-fd-1 directory)))

(provide 'helm-fd)
;;; helm-fd.el ends here
