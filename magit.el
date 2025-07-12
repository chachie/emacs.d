;;; magit.el --- Magit helper commands
;;
;;
;;; Commentary:
;; Interactive functions for magit.
;;; Code:
(require 'magit)

(defun extract-branch-tag (branch-name)
  "Return conventional commit prefix for git based on BRANCH-NAME.
Example: for branch bugfix/tkt-123 return tkt-123(bugfix)"
  (seq-let [v1 v2] (split-string branch-name "/")
    (if v2 (format "%s(%s): " v1 (upcase v2))
      (format "%s: " v1))))

(defun git-commit-insert-branch-with-conventional-commit ()
  "Insert into current buffer a conventional commit prefix."
  (insert (extract-branch-tag (magit-get-current-branch))))


(defun git-repo-path (&optional copy)
  "Get the org/repo path.
With prefix, COPY."
  (interactive "P\n")
  (seq-let (repo proj)
      (take 2 (reverse (split-string (git-http-origin) "/")))
    (let ((result (format "%s/%s" proj repo)))
      (when copy
        (message result)
        (kill-new result))
      result)))


(defun git-http-origin (&optional copy)
  "Find http remote location from current repo.
With prefix, COPY."
  (interactive "P\n")
  (let* ((remote (magit-get "remote.origin.url"))
         (http-remote (if (string-match-p "^https://.*" remote)
                        remote
                        (replace-regexp-in-string ".*@" "https://"
                                                  (replace-regexp-in-string
                                                   ":" "/" remote))))
         (ghb-remote (replace-regexp-in-string "\.git$" "" http-remote)))
    (when copy
      (message ghb-remote)
      (kill-new ghb-remote))
    ghb-remote))

(defun current-file-path-in-repo (&optional copy)
  "Find the path of the file relative to the repo.
With prefix, COPY."
  (interactive "P\n")
  (let ((path (string-remove-prefix (magit-toplevel) (buffer-file-name))))
    (when copy (kill-new path))
    (message path)
    path))

(add-hook 'git-commit-setup-hook 'git-commit-insert-branch-with-conventional-commit)
