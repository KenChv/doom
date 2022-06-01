;;; private/+git.el -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! git-link
  (setq git-link-open-in-browser t)

  (add-to-list 'git-link-remote-alist
               '("git\\.bst\\.ai" git-link-github-http))
  (add-to-list 'git-link-commit-remote-alist
               '("git\\.bst\\.ai" git-link-commit-github-http))
  (add-to-list 'git-link-remote-alist
               '("rnd-github-usa-g\\.huawei\\.com" git-link-github-http))
  (add-to-list 'git-link-commit-remote-alist
               '("rnd-github-usa-g\\.huawei\\.com" git-link-commit-github-http))

  ;; OVERRIDE
  (advice-add #'git-link--select-remote :override #'git-link--read-remote)
  )


(after! magit
  (setq magit-repository-directories '(("~/codes" . 2))
        magit-save-repository-buffers nil
        git-commit-style-convention-checks nil
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  (magit-wip-after-apply-mode t)
  (magit-wip-before-change-mode t))


(after! forge
  (push '("github.argo.ai" "github.argo.ai/api/v3"
          "github.argo.ai" forge-github-repository)
        forge-alist)

  ;; TEMP
  ;; (setq ghub-use-workaround-for-emacs-bug 'force)

  (defvar forge-show-all-issues-and-pullreqs t
    "If nil, only show issues and pullreqs assigned to me.")

  (defun +my/forge-toggle-all-issues-and-pullreqs ()
    (interactive)
    (setq forge-insert-default '(forge-insert-pullreqs forge-insert-issues))
    (setq forge-insert-assigned '(forge-insert-assigned-pullreqs forge-insert-assigned-issues))
    (if forge-show-all-issues-and-pullreqs
        (progn
          (setq forge-show-all-issues-and-pullreqs nil)
          (remove-hook! 'magit-status-sections-hook #'forge-insert-issues nil t)
          (remove-hook! 'magit-status-sections-hook #'forge-insert-pullreqs nil t)
          (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-pullreqs nil t)
          (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-issues nil t))
      (progn
        (setq forge-show-all-issues-and-pullreqs t)
        (remove-hook! 'magit-status-sections-hook #'forge-insert-assigned-issues nil t)
        (remove-hook! 'magit-status-sections-hook #'forge-insert-assigned-pullreqs nil t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues nil t)))

    ;; refresh magit-status buffer
    (magit-refresh))

  ;; Only show issues and pullreqs assigned to me
  (+my/forge-toggle-all-issues-and-pullreqs)
  )


(after! browse-at-remote
  (add-to-list 'browse-at-remote-remote-type-domains '("github.argo.ai" . "github"))
  (add-to-list 'browse-at-remote-remote-type-domains '("git.bst.ai" . "gitlab")))


(after! magit-todos
  (setq magit-todos-exclude-globs '("third-party/*" "third_party/*")))


;; magit-todos uses hl-todo-keywords
(after! hl-todo
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("HACK"  . ,(face-foreground 'warning))
          ("TEMP"  . ,(face-foreground 'warning))
          ("DONE"  . ,(face-foreground 'success))
          ("NOTE"  . ,(face-foreground 'success))
          ("DONT"  . ,(face-foreground 'error))
          ("DEBUG"  . ,(face-foreground 'error))
          ("FAIL"  . ,(face-foreground 'error))
          ("FIXME" . ,(face-foreground 'error))
          ("XXX"   . ,(face-foreground 'error))
          ("XXXX"  . ,(face-foreground 'error)))))
