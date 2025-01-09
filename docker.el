;;; docker.el --- Docker helper commands
;;
;;
;;; Commentary:
;; Interactive functions for docker.
;;; Code:

(defun docker-container-name (arg search-string)
  "Find a docker container name by SEARCH-STRING.
With a prefix ARG, copy it."
  (interactive "P\nsContainer name:")
  (let ((name
         (replace-regexp-in-string
          "[\"\n]" ""
          (shell-command-to-string
           (format  "docker ps --filter \"name=%s\" --format \"{{.Names}}\""
                    search-string)))))
    (when arg (kill-new name))
    (message name)
    name))

(defun docker-ps ()
  "Docker ps."
  (interactive)
  (message (shell-command-to-string
            "docker ps --format \"table {{.Names}}\t{{.Status}}\t{{.Ports}}\"")))

(defun docker-container-logs (search-string)
  "Tail a docker container logs by SEARCH-STRING."
  (interactive "sContainer name:")
  (let* ((buffer-name (format "*k-logs-docker-container-%s*" search-string))
         (container-name (docker-container-name nil search-string)))
    (shell buffer-name)
    (comint-send-string buffer-name (format  "docker logs -f --tail 100 %s\n" container-name))))

;;; docker.el ends here
