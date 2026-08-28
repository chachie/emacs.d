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
            "docker ps --format \"table {{.Names}}\t{{.Image}}\t{{.Status}}\t{{.Ports}}\"")))

(defun docker-container-logs (search-string)
  "Tail a docker container logs by SEARCH-STRING."
  (interactive "sContainer name:")
  (let* ((buffer-name (format "*k-logs-docker-container-%s*" search-string))
         (container-name (docker-container-name nil search-string)))
    (shell buffer-name)
    (comint-send-string buffer-name (format  "docker logs -f --tail 100 %s\n" container-name))))

(defun docker-container-shell (container-name command)
  "Run a COMMAND in a docker container CONTAINER-NAME."
  (interactive "sContainer name:\nsCommand:")
  (let ((vterm-buffer-name (format "*k-shell-docker-container-%s*" container-name)))
    (vterm)
    (rename-buffer vterm-buffer-name t)
    (vterm-send-string (format  "docker exec -it %s %s\n" container-name command))
    (vterm-send-return)))

;;; docker.el ends here
