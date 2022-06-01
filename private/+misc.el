;;; private/+misc.el -*- lexical-binding: t; -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Ken Zhv"
      user-mail-address "kenzhv@126.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 交換 caps 與 control 按鍵功能，並重新配置 super 代替 meta
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when IS-MAC (setq mac-option-modifier 'meta
                   mac-command-modifier 'super))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq display-line-numbers-type 'relative)

(setq-default fill-column 80
              delete-trailing-lines t)

(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RIME INPUT METHOD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; rime-show-candidate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;  nil         不展示
;;;;;;;;;;;  minibuffer  在minibuffer中展示， 推荐使用的方式
;;;;;;;;;;;  message     直接使用 message 输出，兼容控制 minibuffer 内容的插件
;;;;;;;;;;;  popup       使用 popup.el 展示跟随的候选
;;;;;;;;;;;  posframe    使用 posframe 展示跟随的候选，在不可用的时候会用 popup
;;;;;;;;;;;  sidewindow  使用 sidewindow 展示跟随的候选
;;;;;;;;;;;;;;;;;;; rime-show-candidate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! rime
  :defer t
  :custom
  (default-input-method "rime")
  :config
  (setq rime-show-candidate 'posframe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 指定 Rime 共享目录和用户目录
  ;; rime-share-data-dir 是 Rime 安装后放置配置的目录，例如 Linux 上默认为
  ;;    /usr/share/rime-data. 通常使用默认值即可。 如果使用其它的位置，可以配置该值。
  ;; rime-user-data-dir 为 emacs-rime 布署的位置，默认为 ~/.emacs.d/rime ，如果需要其它位置，可以配置该值。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq rime-user-data-dir (expand-file-name "~/.config/doom/rime"))
  ;; (setq rime-share-data-dir (expand-file-name "~/Gitfiles/codes.superspaces.cn/dot/rime"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 在 macos 系统下不能直接使用 squirrel 的配置，无法实现繁体转简体，采用的办法：
  ;; 禁用上面的配置，使用 rime 默认创建的配置文件夹： ~/.emacs.d/.local/etc/rime/，
  ;; 创建 default.custom.yaml 进行配置，即可解决 繁体 -〉简体问题。
  ;; patch:
  ;; schema_list:  # 對於列表類型，現在無有辦法指定如何添加、消除或單一修改某項，於是要在定製檔中將整個列表替換！
  ;;  - schema: luna_pinyin_simp  #默认简体输入法
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (when IS-MAC
    (setq rime-librime-root "~/.config/doom/librime/dist"))
  (set-face-attribute 'rime-default-face nil :foreground "#839496" :background "#073642")
  (setq rime-posframe-properties
        (list :background-color "#073642"
              :foreground-color "#839496"
              :font "sarasa ui sc"
              :internal-border-width 1))
  (setq rime-disable-predicates '(rime-predicate-after-alphabet-char-p
                                  rime-predicate-prog-in-code-p))
  (setq rime-inline-ascii-trigger 'shift-l)
  ;; Set Nixos env
  (when (and IS-LINUX (executable-find "nix"))
    (setq rime-emacs-module-header-root (concat (shell-command-to-string "nix eval --raw '(let pkgs = import <nixpkgs> {}; in with pkgs; lib.getLib emacs)'") "/include")
          rime-librime-root (shell-command-to-string "nix eval --raw '(let pkgs = import <nixpkgs> {}; in with pkgs; lib.getLib librime)'")
          rime-share-data-dir (concat (shell-command-to-string "nix eval --raw '(let pkgs = import <nixpkgs> {}; in with pkgs; lib.getLib brise)'") "/share/rime-data")))
  :bind
  (:map rime-mode-map
   ;; open rime menu
   ("C-`" . 'rime-send-keybinding))
  (:map rime-active-mode-map
   ("C-j" . 'rime-inline-ascii)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chinese calendar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(after! cal-china-x
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SSH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! ssh-deploy
  (setq ssh-deploy-automatically-detect-remote-changes 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq evil-cross-lines t
      evil-split-window-below t
      evil-vsplit-window-right t
      evil-emacs-state-cursor `(box ,(doom-color 'violet)))


(after! evil
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer))


(after! evil-snipe
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer)
  (push 'prodigy-mode evil-snipe-disabled-modes))


(use-package! imenu-list
  :defer t
  :config
  (set-popup-rules! '(("^\\*Ilist\\*" :side right :size 40 :select t))))


(after! nav-flash
  ;; (defun nav-flash-show (&optional pos end-pos face delay)
  ;; ...
  ;; (let ((inhibit-point-motion-hooks t))
  ;; (goto-char pos)
  ;; (beginning-of-visual-line) ; work around args-out-of-range error when the target file is not opened
  (defun +advice/nav-flash-show (orig-fn &rest args)
    (ignore-errors (apply orig-fn args)))
  (advice-add 'nav-flash-show :around #'+advice/nav-flash-show))

;; Use ) key to toggle it
(after! dired
  ;; Rust version ls
  (when-let (exa (executable-find "exa"))
    (setq insert-directory-program exa)
    (setq dired-listing-switches (string-join (list "-ahl" "--group-directories-first") " ")))
  )

(after! (:and ranger dired)
  (setq ranger-hide-cursor t
        ranger-show-hidden 'format
        ranger-deer-show-details nil)

  (defun ranger-copy-relative-path ()
    "Copy the current file path relative to `default-directory path."
    (interactive)
    (let ((current-prefix-arg 1))
      (call-interactively 'dired-copy-filename-as-kill)))

  (defun ranger-close-and-kill-inactive-buffers ()
    "ranger close current buffer and kill inactive ranger buffers"
    (interactive)
    (ranger-close)
    (ranger-kill-buffers-without-window))
  ;; do not kill buffer if exists in windows
  (defun ranger-disable ()
    "Interactively disable ranger-mode."
    (interactive)
    (ranger-revert)))


(after! dash-docs
  (setq dash-docs-use-workaround-for-emacs-bug nil)
  (setq dash-docs-browser-func 'browse-url-generic))


(use-package! highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (defun +indent-guides-init-faces-h ()
    (when (display-graphic-p)
      (highlight-indent-guides-auto-set-faces))))

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file" "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-up-directory
  (kbd "% l") 'dired-downcase
  (kbd "% u") 'dired-upcase
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emms bongo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(setq emms-source-file-default-directory "~/Music/"
      ;; emms-playlist-buffer-name "*Music*"
      emms-info-functions '(emms-info-tinytag)
      emms-browser-covers 'emms-browser-cache-thumbnail-async
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
(map! :leader
      (:prefix ("e" . "EMMS audio player")
       :desc "Go to emms playlist" "a" #'emms-playlist-mode-go
       :desc "Emms pause track" "x" #'emms-pause
       :desc "Emms stop track" "s" #'emms-stop
       :desc "Emms play previous track" "p" #'emms-previous
       :desc "Emms play next track" "n" #'emms-next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUICKRUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! quickrun
  ;; quickrun--language-alist
  (when IS-LINUX
    (quickrun-set-default "c++" "c++/g++")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keycast
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" keycast-mode-line " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
                      :height 0.9)
    '(keycast-key :inherit custom-modified
                  :height 1.1
                  :weight bold)))
