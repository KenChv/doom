;;; private/+ui.el -*- lexical-binding: t; -*-
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 配置默认的显示颜色主题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq doom-theme 'doom-solarized-light)
;;(setq doom-theme 'doom-zenburn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 配置图形界面下的显示字体及字号等
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (display-graphic-p)
  (setq user-font
        (cond
         ((find-font (font-spec :name  "CartographCF Nerd Font")) "CartographCF Nerd Font")
         ((find-font (font-spec :name  "Droid Sans Mono")) "Droid Sans Mono")
         ((find-font (font-spec :name  "Droid Sans Fallback")) "Droid Sans Fallback")))
  (cond (IS-MAC
         (setq doom-font (font-spec :family user-font :size 13 )
               doom-variable-pitch-font (font-spec :family user-font :size 15)
               doom-big-font (font-spec :family user-font :size 24)
               doom-modeline-height 24))
        (IS-LINUX
         (setq resolution-factor (eval (/ (x-display-pixel-height) 1080.0)))
         (setq doom-font (font-spec :family user-font :size (eval (round (* 13 resolution-factor))))
               doom-big-font (font-spec :family user-font :size (eval (round (* 18 resolution-factor))))
               doom-modeline-height (eval (round (* 14 resolution-factor)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 重新定义 dashboard, 改变显示效果
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! dashboard
  :init ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "\nKEYBINDINGS:\
\nFind file               (SPC .)     \
Open buffer list    (SPC b i)\
\nFind recent files       (SPC f r)   \
Open the eshell     (SPC o s)\
\nOpen dired file manager (SPC d d)   \
List of keybindings (SPC h b b)")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
 (setq dashboard-startup-banner "~/.config/doom/banners/doom-emacs-dash.png")
 (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))

(setq doom-fallback-buffer "*dashboard*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; full screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 小鹦鹉的应用 https://github.com/dp12/parrot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! parrot
  :config
  (parrot-mode))

;; apend
(dolist (entry '(
                 (:rot ("lizchicheng" "fanlingling"))
                 (:rot ("Array" "Object" "String" "Function"))
                 ))
  (add-to-list 'parrot-rotate-dict entry))
