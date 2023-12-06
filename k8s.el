;;; k8s.el --- Kubernetes kubectl helper commands
;;
;;
;;; Commentary:
;; Interactive functions for kubectl.
;;; Code:

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

(defun k-restart-deployment (ns deployment)
  (interactive "sns: \nsdeployment:")
  (shell-command (format "kubectl -n %s rollout restart deployment/%s"
                         ns deployment)))

(defun k-deployments (ns)
  (interactive "sns:")
  (shell-command (format "kubectl -n %s get deployments" ns)))

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
  (shell-command (format "kubectl delete namespace %s" ns)))

(defun k-delete-ingress (ingress)
  (interactive "singress: \n")
  (shell-command (format "kubectl delete ingress %s" ingress)))

(defun k-describe-ingress (ingress)
  (interactive "singress: \n")
  (shell-command (format "kubectl describe ingress %s" ingress)))

(defun k-delete-deployment (deployment)
  (interactive "sdeployment: \n")
  (shell-command (format "kubectl delete deployment %s" deployment)))

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

(defun k-hpa ()
  (interactive)
  (shell-command "kubectl get hpa"))


(defun k-describe-hpa (hpa)
  (interactive "shpa: \n")
  (shell-command (format "kubectl describe hpa %s" hpa)))


(defun k-hpa-watch (filter)
  (interactive "sfilter: ")
  (let* ((buffer-name (format "*k-hpa-watch-%s*" filter))
         (filter-part (if (string-empty-p filter) ""
                        (format "| egrep -i %s" filter))))
    (shell buffer-name)
    (comint-send-string buffer-name
                        (format "kubectl get hpa --watch %s \n" filter-part))))

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

(defun k-describe-volume (volume)
  (interactive "svolume: \n")
  (shell-command (format "kubectl describe pv %s" volume))
)


(defun k-delete-pod (pod)
  (interactive "spod: \n")
  (shell-command (format "kubectl delete pods/%s" pod))
)

(defun k-get-serviceaccounts-all-namespaces (filter)
  (interactive "sfilter: ")
  (let* ((buffer-name (format "*k-pods-get-serviceaccounts-all-namespaces-%s*" filter))
         (cmd (format "kubectl get serviceaccounts --all-namespaces -o wide")))
    (shell buffer-name)
    (comint-send-string buffer-name
                        (if (> (length filter) 0)
                            (format "%s | egrep -i %s\n" cmd filter)
                          (format "%s\n" cmd)))))

(defun k-describe-serviceaccount (serviceaccount namespace)
  (interactive "sserviceaccount: \nsnamespace: ")
  (shell-command (format "kubectl describe serviceaccount %s -n %s" serviceaccount namespace)))

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
                        (format "kubectl logs --max-log-requests 6 --follow --tail 100 -l app.kubernetes.io/instance=%s -c %s\n"
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

(defun k-describe-node (node)
  (interactive "snode: \n")
  (shell-command (format "kubectl describe node %s" node))
  (switch-to-buffer "*Shell Command Output*")
)

(defun k-top-pod (pod-name)
  (interactive "spod-name: ")
  (let* ((buffer-name (format "*k-top-pod-%s*" pod-name))
         (cmd (if pod-name (format "kubectl top pod %s --sort-by=cpu" pod-name)
                "kubectl top pods --sort-by=cpu")))
    (shell buffer-name)
    (comint-send-string buffer-name (format "%s\n" cmd))))

(defun k-pods-all-namespaces (filter)
  (interactive "sfilter: ")
  (let* ((buffer-name (format "*k-pods-all-namespaces-%s*" filter))
         (cmd "kubectl get pods --all-namespaces -o wide"))
    (shell buffer-name)
    (comint-send-string buffer-name
                        (if (> (length filter) 0)
                            (format "%s | egrep -i %s\n" cmd filter)
                          (format "%s\n" cmd)))))

(defun k-pods (filter)
  (interactive "sfilter: ")
  (let* ((buffer-name (format "*k-pods-%s*" filter))
         (cmd "kubectl get pods -o wide"))
    (shell buffer-name)
    (comint-send-string buffer-name
                        (if (> (length filter) 0)
                            (format "%s | egrep -i %s\n" cmd filter)
                          (format "%s\n" cmd)))))

(defun k-nodes (filter)
  (interactive "sfilter: ")
  (let* ((buffer-name (format "*k-nodes-%s*" filter))
         (cmd "kubectl get nodes -o wide"))
    (shell buffer-name)
    (comint-send-string buffer-name
                        (if (> (length filter) 0)
                            (format "%s | egrep -i %s\n" cmd filter)
                          (format "%s\n" cmd)))))

(defun k-cluster-info ()
  (interactive)
  (let* ((buffer-name (format "*k-cluster-info*"))
         (cmd "kubectl cluster-info dump"))
    (shell buffer-name)
    (comint-send-string buffer-name (format "%s\n" cmd))))

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

(defun k-get-ingress ()
  (interactive)
  (shell-command "kubectl get ingress")
)

(defun k-get-secrets ()
  (interactive)
  (shell-command "kubectl get secrets")
)

(defun k-describe-secret (secret)
  (interactive "ssecret: ")
  (shell-command (format "kubectl describe secret %s" secret))
)

(defun k-get-secret (secret)
  (interactive "ssecret: ")
  (shell-command (format "kubectl get secret %s -o jsonpath='{.data}'" secret))
)
