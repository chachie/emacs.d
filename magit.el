(defun extract-branch-tag (branch-name)
  "Return conventional commit prefix for git based on BRANCH-NAME.
Example: for branch bugfix/tkt-123 return tkt-123(bugfix)"
  (seq-let [v1 v2] (split-string branch-name "/")
    (if v2 (format "%s(%s): " v1 (upcase v2))
      (format "%s: " v1))))

(defun git-commit-insert-branch-with-conventional-commit ()
  "Insert into current buffer a conventional commit prefix."
  (insert (extract-branch-tag (magit-get-current-branch))))

(add-hook 'git-commit-setup-hook 'git-commit-insert-branch-with-conventional-commit)
