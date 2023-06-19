(defun k-event-watch (pod)
  (interactive "spod:")
  (let* ((buffer-name (format "*k-event-watch-%s*" pod))
         (cmd "kubectl get event --watch --field-selector involvedObject.name=%s\n"))
    (shell buffer-name)
    (comint-send-string buffer-name (format cmd pod))))

(defun k-restart-env (env)
  (interactive "senv: \n")
  (shell-command (format "kubectl rollout restart deployment %s" env))
)

(defun k-restart-pod (ns app)
  (interactive "sns: \nsapp:")
  (shell-command (format "kubectl -n %s rollout restart deployment/%s"
                         ns app)))

(defun k-cron-watch (filter)
  (interactive "sfilter: ")
  (let* ((buffer-name (format "*k-cron-watch-%s*" filter))
         (cmd "kubectl get jobs --watch"))
    (shell buffer-name)
    (comint-send-string buffer-name
                        (if (> (length filter) 0)
                            (format "%s | egrep -i %s\n" cmd filter)
                          (format "%s\n" cmd)))))

(defun k-cron ()
  (interactive)
  (shell-command "kubectl get cronjob -o wide")
  (switch-to-buffer "*Shell Command Output*"))

(defun k-delete-manual-cron (job)
  (interactive "sjob: \n")
  (shell-command (format "kubectl delete job manual-%s" job))
)

(defun k-run-manual-cron (job)
  (interactive "sjob: \n")
  (shell-command (format "kubectl create job --from=cronjob/%s manual-%s" job job))
)


(defun k-delete-namespace (ns)
  (interactive "sns: \n")
  (shell-command (format "kubectl delete namespace %s" ns))
)

(defun k-describe-service (service)
  (interactive "sservice: \n")
  (shell-command (format "kubectl describe service %s" service))
  (switch-to-buffer "*Shell Command Output*"))

(defun k-shell (prefix pod container cmd reuse)
  (interactive "P\nspod: \nscontainer: \nscmd: \nsreuse existing?(y/n)")
  (let* ((buffer-name (format "*k-shell-%s-%s-%s-*" pod container 
                              (if (string= reuse "n")
                                  (format-time-string "%s")
                                "0")))
         (command (format "kubectl exec --stdin --tty %s -c %s -- %s\n" 
                          pod container cmd)))
    
    (if prefix (print command)
      (shell buffer-name)
      (comint-send-string buffer-name command))))

(defun k-create-namespace (ns)
  (interactive "sns: \n")
  (shell-command (format "kubectl create namespace %s" ns))
  (switch-to-buffer "*Shell Command Output*")
)

(defun k-pods-watch (filter)
  (interactive "sfilter: ")
  (let* ((buffer-name (format "*k-pods-watch-%s*" filter))
         (filter-part (if (string-empty-p filter) ""
                        (format "| egrep -i %s" filter))))
    (shell buffer-name)
    (comint-send-string buffer-name
                        (format "kubectl get pods --watch %s \n" filter-part))))

(defun k-destinationrule ()
  (interactive)
  (switch-to-buffer "*Shell Command Output*")
  (shell-command "kubectl get destinationrule")
)

(defun k-get-pvc ()
  (interactive)
  (shell-command "kubectl get pvc"))

(defun k-virtualservice ()
  (interactive)
  (shell-command "kubectl get virtualservice")
  (switch-to-buffer "*Shell Command Output*")
)

(defun k-services ()
  (interactive)
  (shell-command "kubectl get services")
  (switch-to-buffer "*Shell Command Output*")
)

(defun k-delete-pvc (pvc)
  (interactive "spvc: \n")
  (shell-command (format "kubectl delete pvc %s" pvc))
)

(defun k-describe-pvc (pvc)
  (interactive "spvc: \n")
  (shell-command (format "kubectl describe pvc %s" pvc))
)


(defun k-delete-pod (pod)
  (interactive "spod: \n")
  (shell-command (format "kubectl delete pods/%s" pod))
)

(defun k-all ()
  (interactive)
  (shell-command "kubectl get all")
  (switch-to-buffer "*Shell Command Output*")
)

(defun k-logs-crashed-pod (pod container)
  (interactive "spod: \nscontainer: ")
  (shell-command  (format "kubectl logs %s -c %s -p\n"
                          pod container)))

(defun k-logs-pod (pod container)
  (interactive "spod: \nscontainer: ")
  (let* ((buffer-name (format "*k-logs-pod-%s-%s*" pod container)))
    (shell buffer-name)
    (comint-send-string buffer-name
                        (format "kubectl logs --follow --tail 100 pod/%s -c %s\n"
                                pod container))
    (comint-send-string buffer-name (format "echo end\n"))
    ))

(defun k-logs (app container)
  (interactive "sapp: \nscontainer: ")
  (let* ((buffer-name (format "*k-logs-%s-%s*" app container)))
    (shell buffer-name)
    (comint-send-string buffer-name
                        (format "kubectl logs --max-log-requests 6 --follow --tail 100 -l app=%s -c %s\n"
                                app container))))

(defun k-delete (yaml-file)
  (interactive "fyaml-file: \n")
  (shell-command (format "kubectl delete -f %s" yaml-file))
)

(defun k-port-forward (pod src-port dst-port)
  (interactive "spod: \nnlocal port: \nnpod port: ")
  (let* ((buffer-name (format "*k-port-forward-%s*" pod)))
    (shell buffer-name)
    (comint-send-string buffer-name
                        (format "kubectl port-forward pod/%s %s:%s\n"
                                pod src-port dst-port))))

(defun k-describe-configmap (configmap)
  (interactive "sConfigMap: \n")
  (shell-command (format "kubectl describe configmaps %s" configmap))
  (switch-to-buffer "*Shell Command Output*")
)

(defun k-describe-pod (pod)
  (interactive "spod: \n")
  (shell-command (format "kubectl describe pod/%s" pod))
  (switch-to-buffer "*Shell Command Output*")
)

(defun k-pods (filter)
  (interactive "sfilter: ")
  (let* ((buffer-name (format "*k-pods-%s*" filter))
         (cmd "kubectl get pods -o wide"))
    (shell buffer-name)
    (comint-send-string buffer-name
                        (if (> (length filter) 0)
                            (format "%s | egrep -i %s\n" cmd filter)
                          (format "%s\n" cmd)))))


(defun k-namespaces ()
  (interactive)
  (shell-command "kubectl get namespace")
  (switch-to-buffer "*Shell Command Output*")
)

(defun k-apply (yaml-file)
  (interactive "Fyaml: ")
  (shell-command (format  "kubectl apply -f %s" yaml-file))
)

(defun k-set-namespace (ns)
  (interactive "sns: ")
  (shell-command (format  "kubectl config set-context --current --namespace=%s" ns))
)

(defun k-get-namespace ()
  (interactive)
  (shell-command "kubectl config current-context")
)
