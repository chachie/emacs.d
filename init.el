;;; init.el --- Emacs configuration
;;
;;
;;; Commentary:
;; Emacs configuration starter.
;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

; modes
(recentf-mode)
(ido-mode)
(require 'ido) (ido-everywhere)
(save-place-mode)
(show-paren-mode)
(global-hl-line-mode)
(menu-bar-mode -1)
(blink-cursor-mode 0)
(global-display-line-numbers-mode)
(electric-indent-mode 0)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(set-face-attribute 'default nil :height 140)

(fset 'yes-or-no-p 'y-or-n-p)

; hooks

; custom keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-\"") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x y") 'term-paste)

; custom functions
(defun chrome (url user-data-dir)
  "Start an instance of a chrome browser at URL with USER-DATA-DIR.
If USER-DATA-DIR is not specified, choose a random one."
  (interactive
   (list
    (read-string
          "URL:" ""
          "" nil "")
    (read-directory-name
     "User data directory:" (format "/tmp/%d" (random 1000000)) nil nil "")))
  (start-process
   "" nil
   (if (string-equal system-type "gnu/linux")
       "/usr/bin/google-chrome"
     "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
   (format "--user-data-dir=%s" user-data-dir)
   "--disable-fre" "--no-default-browser-check"
   "--no-first-run" url))

(defun copy-buffer-name ()
  "Copy current buffer's file name."
  (interactive)
  (kill-new (buffer-file-name)))

;; gnus
(require 'text-property-search)

(defun get-link (url-regex &optional copy)
  "Return the first link that matches URL-REGEX in a gnus article.
or COPY the link."
  (goto-char (point-min))
  (let ((match (text-property-search-forward
                'gnus-string nil
                (lambda (_ url) (when url (string-match-p url-regex url))))))
    (when match
      (let ((s (list (prop-match-value match)
                     (buffer-substring-no-properties
                      (prop-match-beginning match)
                      (prop-match-end match))
                     )))
        (if copy (kill-new (car s)) s)))))

(defun find-my-ipv4 (arg)
  "Query Google for my ipv4 address.
With a prefix ARG, copy it."
  (interactive "P")
  (let ((ip
         (string-replace "\"" ""
          (shell-command-to-string
           "dig @ns1.google.com TXT o-o.myaddr.l.google.com +short"))))
    (when arg (kill-new ip))
    (message ip)))

;;; init.el ends here
