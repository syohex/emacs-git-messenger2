;;; git-messenger2.el --- Pop up last commit information of current line -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-git-messenger2
;; Version: 0.18
;; Package-Requires: ((emacs "26.1") (popup "0.5.0"))

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

;; This package provides a function called git-messenger2-popup-message
;; that when called will pop-up the last git commit message for the
;; current line. This uses the git-blame tool internally.
;;
;; Example usage:
;;   (require 'git-messenger2)
;;   (global-set-key (kbd "C-x v p") 'git-messenger2-popup-message)
;;

;;; Code:

(require 'cl-lib)
(require 'popup)

(defgroup git-messenger2 nil
  "git messenger"
  :group 'vc)

(defcustom git-messenger2-show-detail nil
  "Pop up commit ID and author name too"
  :type 'boolean)

(defcustom git-messenger2-before-popup-hook nil
  "Hook run before popup commit message. This hook is taken popup-ed message"
  :type 'hook)

(defcustom git-messenger2-after-popup-hook nil
  "Hook run after popup commit message. This hook is taken popup-ed message"
  :type 'hook)

(defcustom git-messenger2-popup-buffer-hook nil
  "Hook run after popup buffer(popup diff, popup show etc)"
  :type 'hook)

(defcustom git-messenger2-handled-backends '(git)
  "List of version control backends for which `git-messenger' will be used.
Entries in this list will be tried in order to determine whether a
file is under that sort of version control."
  :type '(repeat symbol))

(defvar git-messenger2-last-message nil
  "Last message displayed by git-messenger.

This is set before the pop-up is displayed so accessible in the hooks
and menus.")

(defvar git-messenger2-last-commit-id nil
  "Last commit id for the last message displayed.

This is set before the pop-up is displayed so accessible in the hooks
and menus.")

(defvar git-messenger2-vcs nil)

(defconst git-messenger2-directory-of-vcs
  '((git . ".git")))

(defun git-messenger2--blame-arguments (vcs file line)
  (let ((basename (file-name-nondirectory file)))
    (cl-case vcs
      (git (list "--no-pager" "blame" "-w" "-L"
                 (format "%d,+1" line)
                 "--porcelain" basename)))))

(defsubst git-messenger2--cat-file-arguments (commit-id)
  (list "--no-pager" "cat-file" "commit" commit-id))

(defsubst git-messenger2--vcs-command (vcs)
  (cl-case vcs
    (git "git")))

(defun git-messenger2--execute-command (vcs args output)
  (cl-case vcs
    (git (apply 'process-file "git" nil output nil args))))

(defun git-messenger2--git-commit-info-at-line ()
  (let* ((id-line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position)))
         (commit-id (car (split-string id-line)))
         (author (if (re-search-forward "^author \\(.+\\)$" nil t)
                     (match-string-no-properties 1)
                   "unknown")))
    (cons commit-id author)))

(defun git-messenger2--commit-info-at-line (vcs file line)
  (with-temp-buffer
    (let ((args (git-messenger2--blame-arguments vcs file line)))
      (unless (zerop (git-messenger2--execute-command vcs args t))
        (error "Failed: '%s blame'" (git-messenger2--vcs-command vcs)))
      (goto-char (point-min))
      (cl-case vcs
        (git (git-messenger2--git-commit-info-at-line))))))

(defsubst git-messenger2--not-committed-id-p (commit-id)
  (or (string-match-p "\\`\\(?:0+\\|-\\)\\'" commit-id)))

(defun git-messenger2--git-commit-message (commit-id)
  (let ((args (git-messenger2--cat-file-arguments commit-id)))
    (unless (zerop (git-messenger2--execute-command 'git args t))
      (error "Failed: 'git cat-file'"))
    (goto-char (point-min))
    (forward-paragraph)
    (buffer-substring-no-properties (point) (point-max))))

(defun git-messenger2--commit-message (vcs commit-id)
  (with-temp-buffer
    (if (git-messenger2--not-committed-id-p commit-id)
        "* not yet committed *"
      (cl-case vcs
        (git (git-messenger2--git-commit-message commit-id))))))

(defun git-messenger2--commit-date (commit-id)
  (let ((args (list "--no-pager" "show" "--pretty=%ad" commit-id)))
    (with-temp-buffer
      (unless (zerop (git-messenger2--execute-command 'git args t))
        (error "Failed 'git show'"))
      (goto-char (point-min))
      (buffer-substring-no-properties
       (line-beginning-position) (line-end-position)))))

(defun git-messenger2--format-detail (vcs commit-id author message)
  (cl-case vcs
    (git (let ((date (git-messenger2--commit-date commit-id)))
           (format "commit : %s \nAuthor : %s\nDate   : %s \n%s"
                   (substring commit-id 0 8) author date message)))))

(defun git-messenger2--show-detail-p (commit-id)
  (and (or git-messenger2-show-detail current-prefix-arg)
       (not (git-messenger2--not-committed-id-p commit-id))))

(defun git-messenger2--popup-close ()
  (interactive)
  (throw 'git-messenger2-loop t))

(defun git-messenger2--copy-message ()
  "Copy current displayed commit message to kill-ring."
  (interactive)
  (when git-messenger2-last-message
    (kill-new git-messenger2-last-message))
  (git-messenger2--popup-close))

(defun git-messenger2--copy-commit-id ()
  "Copy current displayed commit id to kill-ring."
  (interactive)
  (when git-messenger2-last-commit-id
    (kill-new git-messenger2-last-commit-id))
  (git-messenger2--popup-close))

(defun git-messenger2--popup-common (vcs args &optional mode)
  (with-current-buffer (get-buffer-create "*git-messenger*")
    (view-mode -1)
    (fundamental-mode)
    (erase-buffer)
    (unless (zerop (git-messenger2--execute-command vcs args t))
      (error "Failed: '%s(args=%s)'" (git-messenger2--vcs-command vcs) args))
    (pop-to-buffer (current-buffer))
    (when mode
      (funcall mode))
    (run-hooks 'git-messenger2-popup-buffer-hook)
    (view-mode +1)
    (goto-char (point-min)))
  (git-messenger2--popup-close))

(defun git-messenger2--popup-diff ()
  (interactive)
  (cl-case git-messenger2-vcs
    (git (let ((args (list "--no-pager" "diff" "--no-ext-diff"
                           (concat git-messenger2-last-commit-id "^!"))))
           (git-messenger2--popup-common 'git args 'diff-mode)))))

(defun git-messenger2--popup-show ()
  (interactive)
  (cl-case git-messenger2-vcs
    (git (let ((args (list "--no-pager" "show" "--no-ext-diff" "--stat"
                           git-messenger2-last-commit-id)))
           (git-messenger2--popup-common 'git args)))))

(defun git-messenger2--popup-show-verbose ()
  (interactive)
  (cl-case git-messenger2-vcs
    (git (let ((args (list "--no-pager" "show" "--no-ext-diff" "--stat" "-p"
                           git-messenger2-last-commit-id)))
           (git-messenger2--popup-common 'git args)))))

(defvar git-messenger2-map
  (let ((map (make-sparse-keymap)))
    ;; key bindings
    (define-key map (kbd "q") #'git-messenger2--popup-close)
    (define-key map (kbd "c") #'git-messenger2--copy-commit-id)
    (define-key map (kbd "d") #'git-messenger2--popup-diff)
    (define-key map (kbd "s") #'git-messenger2--popup-show)
    (define-key map (kbd "S") #'git-messenger2--popup-show-verbose)
    (define-key map (kbd "M-w") #'git-messenger2--copy-message)
    (define-key map (kbd ",") #'git-messenger2-show-parent)
    map)
  "Key mappings of git-messenger. This is enabled when commit message is popup-ed.")

(defun git-messenger2--find-vcs ()
  (let ((longest 0)
        result)
    (dolist (vcs git-messenger2-handled-backends result)
      (let* ((dir (assoc-default vcs git-messenger2-directory-of-vcs))
             (vcs-root (locate-dominating-file default-directory dir)))
        (when (and vcs-root (> (length vcs-root) longest))
          (setq longest (length vcs-root)
                result vcs))))))

(defvar git-messenger2-func-prompt
  '((git-messenger2--popup-show . "Show")
    (git-messenger2--popup-show-verbose . "Show verbose")
    (git-messenger2--popup-close . "Close")
    (git-messenger2--copy-commit-id . "Copy hash")
    (git-messenger2--popup-diff . "Diff")
    (git-messenger2--copy-message . "Copy message")
    (git-messenger2-show-parent . "Go Parent")
    (git-messenger2--popup-close . "Quit")))

(defsubst git-messenger2-function-to-key (func)
  (key-description (car-safe (where-is-internal func git-messenger2-map))))

(defun git-messenger2-prompt ()
  (mapconcat (lambda (fp)
               (let* ((func (car fp))
                      (desc (cdr fp))
                      (key (git-messenger2-function-to-key func)))
                 (unless(memq func '(git-messenger2--popup-show-verbose git-messenger2--popup-diff))
                   (format "[%s]%s " key desc))))
             git-messenger2-func-prompt ""))

(defun git-messenger2-show-parent ()
  (interactive)
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (cl-case git-messenger2-vcs
      (git (with-temp-buffer
             (unless (zerop (process-file "git" nil t nil
                                          "blame" "--increment" git-messenger2-last-commit-id "--" file))
               (error "No parent commit ID"))
             (goto-char (point-min))
             (when (re-search-forward (concat "^" git-messenger2-last-commit-id) nil t)
               (when (re-search-forward "previous \\(\\S-+\\)" nil t)
                 (let ((parent (match-string-no-properties 1)))
                   (setq git-messenger2-last-commit-id parent
                         git-messenger2-last-message (git-messenger2--commit-message 'git parent)))))
             (throw 'git-messenger2-loop nil)))
      (otherwise (error "%s does not support for getting parent commit ID" git-messenger2-vcs)))))

;;;###autoload
(defun git-messenger2-popup-message ()
  (interactive)
  (let* ((vcs (git-messenger2--find-vcs))
         (file (buffer-file-name (buffer-base-buffer)))
         (line (line-number-at-pos))
         (commit-info (git-messenger2--commit-info-at-line vcs file line))
         (commit-id (car commit-info))
         (author (cdr commit-info))
         (msg (git-messenger2--commit-message vcs commit-id))
         (popuped-message (if (git-messenger2--show-detail-p commit-id)
                              (git-messenger2--format-detail vcs commit-id author msg)
                            (cl-case vcs
                              (git msg)))))
    (setq git-messenger2-vcs vcs
          git-messenger2-last-message popuped-message
          git-messenger2-last-commit-id commit-id)
    (let (finish)
      (run-hook-with-args 'git-messenger2-before-popup-hook popuped-message)
      (while (not finish)
        (let ((menu (popup-tip git-messenger2-last-message :nowait t)))
          (unwind-protect
              (setq finish (catch 'git-messenger2-loop
                             (popup-menu-event-loop menu git-messenger2-map 'popup-menu-fallback
                                                    :prompt (git-messenger2-prompt))
                             t))
            (popup-delete menu)))))
    (run-hook-with-args 'git-messenger2-after-popup-hook popuped-message)))

(provide 'git-messenger2)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; git-messenger2.el ends here
