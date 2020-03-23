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


;; ;; Fix to use mouse scroll
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)

;; Hide line numbers
;(setq display-line-numbers-type nil)

;; Remove line
(defun cstd-kill-whole-line-keep-pointer()
  (interactive)
  (previous-line 1)
  (next-line 1)
  (kill-whole-line)
  (previous-line 1)
  (next-line 1))
(global-set-key (kbd "M-D") 'cstd-kill-whole-line-keep-pointer)

;; Duplicate & Move up/down
(defun cstd-duplication-current-line-up()
  (interactive)
  (crux-duplicate-current-line-or-region 1)
  (previous-line 1))
(define-key input-decode-map "\e[2;9" [M-up])
(define-key input-decode-map "\e[2;8" [M-down])
(define-key input-decode-map "\e[2;7" [M-sup])
(define-key input-decode-map "\e[2;6" [M-sdown])
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "M-<sup>") 'cstd-duplication-current-line-up)
(global-set-key (kbd "M-<sdown>") 'crux-duplicate-current-line-or-region)

;; Macro
(fset 'cstd-username
      (lambda
        (&optional arg)
        (interactive "p")
        (kmacro-exec-ring-item (quote ("^[[1;5C^[[1;5C^[[1;5C^[[1;5D^@^A^?^[[1;5C^K^[[1;5D^N" 0 "%d")) arg)))
(global-set-key (kbd "<f6>") 'cstd-username)

;; Auto fix when save & Kbd for show debug
(with-eval-after-load 'js2-mode
  (define-key js2-mode-map (kbd "C-c C-d")
    (lambda ()
      (interactive)
      (flycheck-list-errors)
      (ace-window 0)))
  (setq js2-mode-display-warnings-and-errors t)
  (add-hook 'js2-mode-hook 'eslintd-fix-mode))
  ;(add-hook 'after-save-hook 'import-js-fix)

;; Scroll move to center
(setq scroll-conservatively 0)

;; Kbd
(define-key input-decode-map "\e[1;5C" [M-right1])
(define-key input-decode-map "\e[1;5D" [M-left1])
(global-set-key (kbd "M-<right1>") (lambda () (interactive) (right-word 1)))
(global-set-key (kbd "M-<left1>") (lambda () (interactive) (left-word 1)))
(global-set-key (kbd "C-x C-@") 'pop-to-mark-command)
(global-set-key (kbd "M-S") 'isearch-query-replace)
(global-set-key (kbd "C-c a g") 'counsel-ag)
(global-set-key (kbd "C-c r g") 'counsel-rg)
(global-set-key (kbd "C-c k o") 'doom/kill-other-buffers)
(global-set-key (kbd "C-c r b") (lambda () (interactive) (revert-buffer nil t) (message "Buffer is reverted")))
(global-set-key (kbd "C-c b n") 'next-buffer)
(global-set-key (kbd "C-c b p") 'previous-buffer)
(global-set-key (kbd "C-c C-b") 'crux-switch-to-previous-buffer)
(global-set-key (kbd "M-@") 'er/expand-region)
(global-set-key (kbd "C-c d b") 'magit-diff-buffer-file)

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

;; Turn off Paren highlight when Selecting
(defun show-paren--locate-near-paren-custom ()
  (if (not (use-region-p))
      (let* ((ind-pos (save-excursion (back-to-indentation) (point)))
             (eol-pos
              (save-excursion
                (end-of-line) (skip-chars-backward " \t" ind-pos) (point)))
             (before (show-paren--categorize-paren (1- (point))))
             (after (show-paren--categorize-paren (point))))
        (cond
         ;; Point is immediately outside a paren.
         ((eq (car before) -1) before)
         ((eq (car after) 1) after)
         ;; Point is immediately inside a paren.
         ((and show-paren-when-point-inside-paren before))
         ((and show-paren-when-point-inside-paren after))
         ;; Point is in the whitespace before the code.
         ((and show-paren-when-point-in-periphery
               (<= (point) ind-pos))
          (or (show-paren--categorize-paren ind-pos)
              (show-paren--categorize-paren (1- eol-pos))))
         ;; Point is in the whitespace after the code.
         ((and show-paren-when-point-in-periphery
               (>= (point) eol-pos))
          (show-paren--categorize-paren (1- eol-pos)))))))
(advice-add 'show-paren--locate-near-paren :override #'show-paren--locate-near-paren-custom)

;; Fuzzy search
(load-library "fuzzy")
(with-eval-after-load 'fuzzy
  (turn-on-fuzzy-isearch))

;; Disabled projectile caching
(setq projectile-enable-caching nil)
