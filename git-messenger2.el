;;; git-messenger2.el --- Pop up last commit information of current line -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-git-messenger2
;; Version: 0.18
;; Package-Requires: ((emacs "26.3") (popup "0.5.0"))

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
;;   (global-set-key (kbd "C-x v p") #'git-messenger2-popup-message)
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

(defvar git-messenger2-last-message nil
  "Last message displayed by git-messenger.

This is set before the pop-up is displayed so accessible in the hooks
and menus.")

(defvar git-messenger2-last-commit-id nil
  "Last commit id for the last message displayed.

This is set before the pop-up is displayed so accessible in the hooks
and menus.")

(defun git-messenger2--blame-arguments (file line)
  (let ((basename (file-name-nondirectory file)))
    (list "--no-pager" "blame" "-w" "-L"
          (format "%d,+1" line)
          "--porcelain" basename)))

(defun git-messenger2--execute-git (args output)
  (apply #'process-file "git" nil output nil args))

(defun git-messenger2--git-commit-info-at-line ()
  (let* ((id-line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position)))
         (commit-id (car (split-string id-line)))
         (author (if (re-search-forward "^author \\(.+\\)$" nil t)
                     (match-string-no-properties 1)
                   "unknown")))
    (cons commit-id author)))

(defun git-messenger2--commit-info-at-line (file line)
  (with-temp-buffer
    (let ((args (git-messenger2--blame-arguments file line)))
      (unless (zerop (git-messenger2--execute-git args t))
        (error "Failed: 'git blame'"))
      (goto-char (point-min))
      (git-messenger2--git-commit-info-at-line))))

(defun git-messenger2--valid-commit-id-p (commit-id)
  (not (string-match-p "\\`\\(?:0+\\|-\\)\\'" commit-id)))

(defun git-messenger2--commit-message (commit-id)
  (with-temp-buffer
    (if (not (git-messenger2--valid-commit-id-p commit-id))
        "* not yet committed *"
      (let ((args (list "--no-pager" "cat-file" "commit" commit-id)))
        (unless (zerop (git-messenger2--execute-git args t))
          (error "Failed: 'git cat-file'"))
        (goto-char (point-min))
        (forward-paragraph)
        (buffer-substring-no-properties (point) (point-max))))))

(defun git-messenger2--commit-date (commit-id)
  (let ((args (list "--no-pager" "show" "--pretty=%ad" commit-id)))
    (with-temp-buffer
      (unless (zerop (git-messenger2--execute-git args t))
        (error "Failed 'git show'"))
      (goto-char (point-min))
      (buffer-substring-no-properties
       (line-beginning-position) (line-end-position)))))

(defun git-messenger2--format-detail (commit-id author message)
  (let ((date (git-messenger2--commit-date commit-id)))
    (format "commit : %s \nAuthor : %s\nDate   : %s \n%s"
            (substring commit-id 0 8) author date message)))

(defun git-messenger2--show-detail-p (commit-id)
  (and (or git-messenger2-show-detail current-prefix-arg)
       (git-messenger2--valid-commit-id-p commit-id)))

(defun git-messenger2--popup-close ()
  (interactive)
  (throw 'git-messenger2-loop t))

(defun git-messenger2--popup-common (args &optional mode)
  (with-current-buffer (get-buffer-create "*git-messenger2*")
    (view-mode -1)
    (fundamental-mode)
    (erase-buffer)
    (unless (zerop (git-messenger2--execute-git args t))
      (error "Failed: to execue git (%s)" args))
    (pop-to-buffer (current-buffer))
    (when mode
      (funcall mode))
    (run-hooks 'git-messenger2-popup-buffer-hook)
    (view-mode +1)
    (goto-char (point-min)))
  (git-messenger2--popup-close))

(defun git-messenger2--popup-diff ()
  (interactive)
  (let ((args (list "--no-pager" "diff" "--no-ext-diff"
                    (concat git-messenger2-last-commit-id "^!"))))
    (git-messenger2--popup-common args 'diff-mode)))

(defun git-messenger2--popup-show ()
  (interactive)
  (let ((args (list "--no-pager" "show" "--no-ext-diff" "--stat"
                    git-messenger2-last-commit-id)))
    (git-messenger2--popup-common args)))

(defun git-messenger2--popup-show-verbose ()
  (interactive)
  (let ((args (list "--no-pager" "show" "--no-ext-diff" "--stat" "-p"
                    git-messenger2-last-commit-id)))
    (git-messenger2--popup-common args)))

(defun git-messenger2--first-parent (file line)
  (let ((args (list "--no-pager" "blame" "-w" "-L" (format "%d,+1" line)
                    "--first-parent" (file-name-nondirectory file))))
    (with-temp-buffer
      (unless (zerop (git-messenger2--execute-git args t))
        (error "Failed to execute git blame --first-parent"))
      (goto-char (point-min))
      (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
             (parent-id (cl-first (split-string line))))
        (unless (git-messenger2--valid-commit-id-p parent-id)
          (error "This line is not committed yet"))
        parent-id))))

(defun git-messenger2--find-pr-number (commit-id)
  (let ((args (list "--no-pager" "show" "--oneline" commit-id)))
    (with-temp-buffer
      (unless (zerop (git-messenger2--execute-git args t))
        (error "Failed to execute git show --oneline"))
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward "Merge\\s-+\\(?:pull\\s-+request\\|pr\\)\\s-+#\\([0-9]+\\)")
          (string-to-number (match-string-no-properties 1)))))))

(defun git-messenger2--convert-git-url-to-https (url)
  (replace-regexp-in-string
   "\\.git\\'" ""
   (replace-regexp-in-string
    "github\\.com:" "github.com/"
    (replace-regexp-in-string "\\`git@" "https://" url))))

(defun git-messenger2--github-url ()
  (with-temp-buffer
    (unless (zerop (git-messenger2--execute-git (list "remote" "get-url" "origin") t))
      (error "Failed to get origin url"))
    (goto-char (point-min))
    (let ((url (buffer-substring-no-properties (point) (line-end-position))))
      (if (not (string-match-p "\\`git@" url))
          url
        (git-messenger2--convert-git-url-to-https url)))))

;; inspired from https://gist.github.com/kazuho/eab551e5527cb465847d6b0796d64a39
(defun git-messenger2--goto-pr-page ()
  (interactive)
  (let* ((file (buffer-file-name (buffer-base-buffer)))
         (line (line-number-at-pos))
         (parent-id (git-messenger2--first-parent file line))
         (pr-number (git-messenger2--find-pr-number parent-id))
         (base-url (git-messenger2--github-url))
         (pr-page-url (format "%s/pull/%d" base-url pr-number)))
    (browse-url pr-page-url)))

(defvar git-messenger2-map
  (let ((map (make-sparse-keymap)))
    ;; key bindings
    (define-key map (kbd "q") #'git-messenger2--popup-close)
    (define-key map (kbd "d") #'git-messenger2--popup-diff)
    (define-key map (kbd "s") #'git-messenger2--popup-show)
    (define-key map (kbd "S") #'git-messenger2--popup-show-verbose)
    (define-key map (kbd "p") #'git-messenger2--goto-pr-page)
    map)
  "Key mappings of git-messenger. This is enabled when commit message is popup-ed.")

(defvar git-messenger2-func-prompt
  '((git-messenger2--popup-show . "Show")
    (git-messenger2--popup-show-verbose . "Show verbose")
    (git-messenger2--popup-close . "Close")
    (git-messenger2--popup-diff . "Diff")
    (git-messenger2--popup-close . "Quit")
    (git-messenger2--goto-pr-page . "Pull Request page")))

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

(defun git-messenger2--strip-pgp-signature (msg)
  (with-temp-buffer
    (insert msg)
    (goto-char (point-min))
    (when (search-forward "-----BEGIN PGP SIGNATURE-----" nil t)
      (let ((start (line-beginning-position)))
        (when (search-forward "-----END PGP SIGNATURE-----" nil t)
          (delete-region start (point)))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun git-messenger2--filter-popup-message (msg)
  (let ((filter-funcs '(git-messenger2--strip-pgp-signature)))
    (dolist (func filter-funcs msg)
      (setq msg (or (funcall func msg) msg)))))

;;;###autoload
(defun git-messenger2-popup-message ()
  (interactive)
  (let* ((file (buffer-file-name (buffer-base-buffer)))
         (line (line-number-at-pos))
         (commit-info (git-messenger2--commit-info-at-line file line))
         (commit-id (car commit-info))
         (author (cdr commit-info))
         (msg (git-messenger2--commit-message commit-id))
         (popuped-message (if (git-messenger2--show-detail-p commit-id)
                              (git-messenger2--format-detail commit-id author msg)
                            msg)))
    (setq popuped-message (git-messenger2--filter-popup-message popuped-message))
    (setq git-messenger2-last-message popuped-message
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
