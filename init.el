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
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

; custom keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-\"") 'comment-or-uncomment-region)

; custom functions
(defun chrome (url)
  "Start an instance of a chrome browser at URL with no cached files."
  (interactive "surl:\n")
  (let ((path-arg (format "--user-data-dir=/tmp/%d" (random 1000000))))
    (start-process
     "" nil
     (if (string-equal system-type "gnu/linux")
         "/usr/bin/google-chrome"
       "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
     path-arg "--disable-fre" "--no-default-browser-check"
     "--no-first-run" url)))

;;; init.el ends here
