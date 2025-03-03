;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.


;; Disabled projectile caching
(setq projectile-enable-caching nil)

(setq confirm-kill-emacs nil)

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; ;; Fix to use mouse scroll
;; (xterm-mouse-mode)
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)
(global-set-key (kbd "<wheel-up>") 'scroll-down-line)
(global-set-key (kbd "<wheel-down>") 'scroll-up-line)

;; Remove line
(defun kill-whole-line-keep-pointer()
  (interactive)
  (previous-line 1)
  (next-line 1)
  (kill-whole-line)
  (previous-line 1)
  (next-line 1))

;; Duplicate & Move up/down
(defun duplication-current-line-up()
  (interactive)
  (crux-duplicate-current-line-or-region 1)
  (previous-line 1))

(define-key input-decode-map "\e[0;5A" [M-D])
(global-set-key (kbd "<M-D>") 'kill-whole-line-keep-pointer)

(define-key input-decode-map "\e[0;4B" [M-sup])
(define-key input-decode-map "\e[0;4A" [M-sdown])

(global-set-key (kbd "M-<sup>") 'cstd-duplication-current-line-up)
(global-set-key (kbd "M-<sdown>") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "<S-mouse-1>") 'mouse-set-mark)
(global-set-key (kbd "C-x C-SPC") 'pop-to-mark-command)
(global-set-key (kbd "C-c r g") 'counsel-rg)
(global-set-key (kbd "C-c C-b") 'crux-switch-to-previous-buffer)
(global-set-key (kbd "M-@") 'er/expand-region)

;; Workspace: Auto
(with-eval-after-load 'counsel
  (defun counsel-find-file-action (x)
    (with-ivy-window
      (if (and counsel-find-file-speedup-remote
               (file-remote-p ivy--directory))
          (let ((find-file-hook nil))
            (find-file (expand-file-name x ivy--directory)))
        (if (and (bound-and-true-p persp-mode) (bound-and-true-p projectile-mode))
            (let (project-name (project-name-root (projectile-project-root (expand-file-name x))))
              (if project-name-root
                  (setq project-name (funcall projectile-project-name-function project-name-root))
                (setq project-name "main"))
              (persp-switch project-name)))
        (find-file (expand-file-name x ivy--directory))))))

;; Custom functions search & replace with selection
(defun isearch-forward-with-selection(&optional regexp-p no-recursive-edit)
  (interactive "P\np")
  (isearch-mode t (not (null regexp-p)) nil (not no-recursive-edit))
  (if (use-region-p)
      (setq string (buffer-substring (mark) (point)))
    (setq string ""))
  (save-excursion
    (isearch-yank-string string)
    (deactivate-mark)))
(advice-add 'isearch-forward :override #'isearch-forward-with-selection)

(eval-after-load "isearch" '(require 'isearch-prop))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(setq! company-idle-delay 0.1)

(use-package! jtsx
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode))
  :custom
  ;; Optional customizations
  (js-indent-level 2)
  (typescript-ts-mode-indent-offset 2)
  (jtsx-switch-indent-offset 4)
  ;; (jtsx-indent-statement-block-regarding-standalone-parent nil)
  ;; (jtsx-jsx-element-move-allow-step-out t)
  ;; (jtsx-enable-jsx-electric-closing-element t)
  ;; (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  ;; (jtsx-enable-jsx-element-tags-auto-sync nil)
  ;; (jtsx-enable-all-syntax-highlighting-features t)
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
    (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
    (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
    (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
    (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
    (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
    (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
    (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
    (define-key mode-map (kbd "C-c j d n") 'jtsx-delete-jsx-node)
    (define-key mode-map (kbd "C-c j d a") 'jtsx-delete-jsx-attribute)
    (define-key mode-map (kbd "C-c j t") 'jtsx-toggle-jsx-attributes-orientation)
    (define-key mode-map (kbd "C-c j h") 'jtsx-rearrange-jsx-attributes-horizontally)
    (define-key mode-map (kbd "C-c j v") 'jtsx-rearrange-jsx-attributes-vertically))

  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))

(use-package! tide
  :after (typescript-mode company flycheck)
  :config
  ;; Cấu hình hiển thị documentation
  (setq tide-completion-detailed t)
  (setq tide-always-show-documentation t)

  ;; Cấu hình eldoc - hiển thị thông tin ở minibuffer
  (setq tide-hl-identifier-idle-time 0.5)

  ;; Hiển thị thông tin ở bottom thay vì sidebar
  (setq tide-show-xref-function 'xref-show-definitions-buffer)

  ;; Thiết lập cho hiển thị documentation khi hover
  (setq tide-eldoc-function 'tide-eldoc-function)

  ;; Cấu hình format cho documentation
  (setq tide-doc-buffer-name "*tide-documentation*")
  (setq tide-doc-max-height 20)
  (setq tide-doc-buffer-max-lines 20))

;; Tăng giới hạn hiển thị cho eldoc
(setq eldoc-echo-area-use-multiline-p t)
(setq eldoc-echo-area-display-truncation-message nil)
(setq eldoc-echo-area-prefer-doc-buffer t)
(setq eldoc-idle-delay 0.1)

(use-package! polymode
  :init
  (define-polymode tsx-scss-mode
    :hostmode 'tsx-hostmode
    :innermodes '(scss-innermode))

  (define-hostmode tsx-hostmode
    :mode 'jtsx-tsx-mode)

  (define-innermode scss-innermode
    :mode 'scss-mode
    :head-matcher "styled\\(\\.?[a-zA-Z0-9()]+\\)*"
    :tail-matcher ";"))

;;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-scss-mode))

;; Hàm thiết lập cho Tide mode
(defun setup-tide-mode ()
  "Set up Tide mode with necessary configurations."
  (interactive)
  (tide-setup)
  (tide-hl-identifier-mode +1)
  (eldoc-mode +1)
  (flycheck-mode +1)
  (eslintd-fix-mode +1))

;; Thêm setup-tide-mode vào các hook phù hợp
(dolist (hook '(jtsx-typescript-mode-hook
                jtsx-jsx-mode-hook
                jtsx-tsx-mode-hook
                scss-mode-hook
                tsx-scss-mode-hook))
  (add-hook hook 'setup-tide-mode))

;; Đảm bảo rằng tide-mode luôn được kích hoạt khi mở file
(defun ensure-tide-mode-on-jump ()
  "Ensure Tide mode and tsx-scss-mode are active after jumping to a new file."
  (when (and buffer-file-name
             (string-match-p "\\.tsx\\'" buffer-file-name))
    (tsx-scss-mode)
    (setup-tide-mode)))

;; Thêm hook vào find-file-hook
(add-hook 'find-file-hook 'ensure-tide-mode-on-jump)

;;; SEARCHING: ripgrep, anzu, engine-mode
(use-package! isearch
  :init
  (global-set-key (kbd "M-s s") 'isearch-forward-regexp)
  (global-set-key (kbd "M-s %") 'query-replace-regexp)
  (define-key isearch-mode-map (kbd "M-s %") 'isearch-query-replace-regexp))

(use-package! vundo
  :init (global-set-key (kbd "C-x u") #'vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols) ; Sử dụng ký tự đẹp hơn
  (setq vundo-window-max-height 30)             ; Tăng chiều cao cửa sổ nếu cần
  (setq vundo-compact-display nil))             ; Tắt chế độ hiển thị nhỏ gọn
