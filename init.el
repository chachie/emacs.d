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

; ansi-term remove conflicting bindings
(require 'term)
(define-key term-raw-map (kbd "M-x") 'nil)
(define-key term-raw-map (kbd "C-u") 'nil)

; hooks
(require 'cl-extra)

(defun delete-trailing-whitespace-for-source-code ()
  "This function operates on clj/el/py/c/cljc/cljs."
  (when (cl-some (lambda (v) (string-equal v (file-name-extension (buffer-file-name))))
              '("clj" "el" "py" "c" "cljc" "cljs"))
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'delete-trailing-whitespace-for-source-code)

(add-hook 'occur-mode-find-occurrence-hook (lambda () (recenter 10)))

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
   "--no-first-run" "--enable-features=WebContentsForceDark"
   "--auto-open-devtools-for-tabs" url))

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
         (replace-regexp-in-string "[\"\n]" ""
          (shell-command-to-string
           "dig @ns1.google.com TXT o-o.myaddr.l.google.com +short"))))
    (when arg (kill-new ip))
    (message ip)))

(require 'json)

(defun process-json-response (status count top-level-key result-fmt keys write-to-file)
  (goto-char (point-max))
  (mark-paragraph)
  (if (= 0  count)
      (message "Response: %s" (plist-get status :error))
    (let* ((json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string)
           (response (json-read-from-string
                      (buffer-substring (point) (point-max))))
           (extracted-response (cond ((and (not top-level-key) (hash-table-p response))
                                      response)
                                     (top-level-key
                                      (gethash top-level-key
                                               response))
                                     (response)))
           (list-response (if (listp extracted-response) extracted-response
                            (list extracted-response)))
           (f (message "%s" list-response))
           (vals
            (cl-map 'list (lambda (r)
                            (cl-map 'list
                                    (lambda (key-path)
                                      (format "%s"
                                              (cl-reduce (lambda (v k)
                                                           (if (hash-table-p v)
                                                               (gethash k v "?") v))
                                                         key-path
                                                         :initial-value r)))
                                    keys))
                    (if count (take count list-response) list-response)))
           (msg (string-join
                 (reverse
                  (cl-map 'list
                          (lambda (el)
                            (if result-fmt (apply 'format result-fmt el)
                              (string-join el "\n"))) vals))
                 "\n")))
      (when write-to-file (write-region msg nil write-to-file nil))
      (message "%s" msg)
      (kill-new msg))))

(defun read-json-vals-from-request
    (url method payload
     auth-type auth-token user-agent
     write-to-file count top-level-key result-fmt
     &rest keys)
  "Fetch TOP-LEVEL-KEY values from json KEYS returned from GET request to URL.
Produce result in given RESULT-FMT.
Use provided AUTH-TYPE, AUTH-TOKEN and USER-AGENT and limit to COUNT values.
WRITE-TO-FILE to output to filename, or nil to ignore.
"
  (let ((url-request-method method)
        (url-request-data payload)
        (url-request-extra-headers
         `(("User-Agent" . ,(if user-agent user-agent "Mozilla/5.0"))
           ("Content-Type" . "application/json")
           ("Authorization" . ,(concat auth-type " " auth-token)))))
    (message "fetching from url=%s" url)
    (url-retrieve url 'process-json-response
                  (list count top-level-key result-fmt keys write-to-file))))



;;; init.el ends here
