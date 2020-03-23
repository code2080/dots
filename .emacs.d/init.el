;;; .emacs.d/init.el --- Emacs Init File
;;; Commentary:
;;; Code:

;(setq gc-cons-threshold (* 64 1024 1024))
;(setq gc-cons-threshold #x40000000)
(setq gc-cons-threshold most-positive-fixnum)

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package gcmh
  :init (gcmh-mode +1)
  )

(use-package xclip
  :init (xclip-mode +1)
  )

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  :config
  ;(doom-themes-visual-bell-config)
  (doom-themes-org-config)
  ;; improve integration w/ org-mode
  ;(add-hook 'doom-load-theme-hook #'doom-themes-org-config)
  )

(use-package which-key
  :init (which-key-mode +1)
  )

(use-package fuzzy
  :defer t
  :config (turn-on-fuzzy-isearch)
  )

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package company
  :defer t
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes
        '(not erc-mode message-mode help-mode gud-mode eshell-mode)
        company-backends '(company-capf)
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))
  (global-company-mode +1)
  )

(use-package yasnippet
  :defer t
  :bind
  (("C-c y i" . yas-insert-snippet)
  ("C-c y n" . yas-new-snippet)
  ("C-c y v" . yas-visit-snippet-file))
  )

(use-package magit
  :bind
  (("C-c v g" . magit-status)
   ("C-c f d" . magit-diff-buffer-file))
  )

(use-package flycheck
  :defer t
  :bind ("C-c C-d" . flycheck-list-errors)
  :init
    ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.25)
  
  (global-flycheck-mode +1)
  )

;; (use-package web-mode
;;   )

(use-package ivy
  :init (ivy-mode +1)
  )

(use-package ivy-rich
  ;:requires (counsel)
  :after (counsel)
  :config (ivy-rich-mode +1)
  )

(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode)
  :init
  (setq prescient-filter-method '(literal regexp initialism) ivy-prescient-retain-classic-highlighting t)
  :config
  (prescient-persist-mode +1)
)

(use-package counsel
  :config
  (defcustom counsel-recentf-include-xdg-list nil
    "Include recently used files listed by XDG-compliant environments, e.g. GNOME and KDE.
https://www.freedesktop.org/wiki/Specifications/desktop-bookmark-spec/."
    :type 'boolean
    )
  
  ; Sort counsel-recentf by time
  (add-to-list 'ivy-sort-functions-alist
	       '(counsel-recentf . file-newer-than-file-p))
  
  ;; Don't use ^ as initial input. Set this here because `counsel' defines more
  ;; of its own, on top of the defaults.
  (setq ivy-initial-inputs-alist nil)
  :bind
  (("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x b" . counsel-switch-buffer)
  ("C-x B" . counsel-projectile-switch-to-buffer)
  ("C-c s i" . counsel-imenu)
  ("C-c b m" . counsel-bookmark)
  ("C-c f r" . counsel-recentf)
  ("C-c a g" . counsel-ag)
  ("C-c r g" . counsel-rg))
  )

(use-package projectile
  :init
  (setq projectile-mode-line-prefix "")
  (setq projectile-dynamic-mode-line nil)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  :bind
  ("C-c p" . projectile-command-map)
  )

(use-package counsel-projectile
  :after (projectile)
  :init
  (define-key projectile-mode-map [remap projectile-find-file] #'counsel-projectile-find-file)
  (define-key projectile-mode-map [remap projectile-find-dir] #'counsel-projectile-find-dir)
  (define-key projectile-mode-map [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer)
  (define-key projectile-mode-map [remap projectile-grep] #'counsel-projectile-grep)
  (define-key projectile-mode-map [remap projectile-ag] #'counsel-projectile-ag)
  (define-key projectile-mode-map [remap projectile-switch-project] #'counsel-projectile-switch-project)  
  )

(use-package undo-tree
  :defer t
  :init
  (setq undo-tree-mode-lighter ""
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
        ;; truncating the undo history and corrupting the tree. See
        ;; https://github.com/syl20bnr/spacemacs/issues/12110
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000
        undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory))
        )
  (global-undo-tree-mode +1)
  )

(use-package crux
  :init
  (define-key input-decode-map "\e[2;7" [M-sup])
  (define-key input-decode-map "\e[2;6" [M-sdown])
  (defun crux-duplication-current-line-or-region-up()
    (interactive)
    (crux-duplicate-current-line-or-region 1)
    (forward-line -1))
  :bind
  (("M-<sup>" . crux-duplication-current-line-or-region-up)
  ("M-<sdown>" . crux-duplicate-current-line-or-region)
  ("C-c C-b" . crux-switch-previous-buffer)
  ("C-c f s" . crux-create-scratch-buffer)
  ("C-c k o" . crux-kill-other-buffers)
  ("C-a" . crux-move-beginning-of-line))
  )

(use-package move-text
  :init
  (define-key input-decode-map "\e[2;9" [M-up])
  (define-key input-decode-map "\e[2;8" [M-down])
  :bind
  (("M-<up>" . move-text-up)
  ("M-<down>" . move-text-down))
  )

(use-package multiple-cursors
  :bind
  (("C-c m t" . mc/mark-all-like-this)
  ("C-c m n" . mc/mark-next-like-this)
  ("C-c m p" . mc/mark-previous-like-this)
  ("C-c m l" . mc/edit-lines))
  )

(use-package doom-modeline
  :defer t
  :init
  (unless after-init-time
    ;; prevent flash of unstyled modeline at startup
    (setq-default mode-line-format nil))
  ;; We display project info in the modeline ourselves
  (setq projectile-dynamic-mode-line nil)
  ;; Set these early so they don't trigger variable watchers
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project)
  (add-hook 'after-init-hook #'doom-modeline-mode)
  (size-indication-mode +1) ; filesize in modeline
  (column-number-mode +1)   ; cursor column in modeline
  )

(defvar hidden-minor-modes
  '(whitespace-mode ivy-mode gcmh-mode eldoc-mode company-mode))
(defun purge-minor-modes ()
  "Dont show on modeline."
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg (setcar trg "")))))
(add-hook 'after-change-major-mode-hook #'purge-minor-modes)

(defun share-to-transfer_sh (downloads)
  "Share file to transfer.sh.
DOWNLOADS: The max-downloads"
  (interactive "p")
  (let ((temp-file
         (make-temp-file ".sharing." nil (file-name-extension (buffer-name) t)))
        (url "https://transfersh.com")
        (msg "") file-hash)
    (if (region-active-p)
        (write-region (point) (mark) temp-file)
      (write-region (point-min) (point-max) temp-file))
    (when (yes-or-no-p (format "Share to %s (%d)?" url downloads))
      (when (yes-or-no-p "Encrypt?")
        (let ((file-hash (md5 (buffer-string))))
          (shell-command (format "openssl aes-256-cbc -md md5 -k %s -in '%s' -out '%s.enc'"
                                 file-hash temp-file temp-file))
          (dired-delete-file temp-file)
          (setq temp-file (format "%s.enc" temp-file))
          (setq msg (format "| openssl aes-256-cbc -d -md md5 -k %s -in - 2>/dev/null"
                            file-hash))))
      (message "curl -L %s 2>/dev/null %s"
               (shell-command-to-string
                (format "curl -q -H 'Max-Downloads: %d' --upload-file '%s' %s 2>/dev/null"
                        downloads temp-file url)) msg)
      (dired-delete-file temp-file))))

(defun kill-whole-line-keep-pointer()
  "Kill while line and keep pointer on current position."
  (interactive)
  (forward-line -1)
  (forward-line 1)
  (kill-whole-line)
  (forward-line -1)
  (forward-line 1))
(global-set-key (kbd "M-D") 'kill-whole-line-keep-pointer)

(define-key input-decode-map "\e[1;5C" [M-right1])
(define-key input-decode-map "\e[1;5D" [M-left1])
(global-set-key (kbd "M-<right1>") (lambda () (interactive) (right-word 1)))
(global-set-key (kbd "M-<left1>") (lambda () (interactive) (left-word 1)))
(global-set-key [mouse-5] 'scroll-up-line)
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key (kbd "C-x C-@") 'pop-to-mark-command)
(global-set-key (kbd "M-S") 'isearch-query-replace)
(global-set-key (kbd "C-x K") 'kill-this-buffer)
(global-set-key (kbd "C-c r b") (lambda () (interactive) (revert-buffer nil t) (message "Buffer is reverted")))

(xterm-mouse-mode)
(show-paren-mode)
(global-display-line-numbers-mode)
(menu-bar-mode -1)
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)

(defun emacs-init-time ()
  "Return a string giving the duration of the Emacs initialization."
  (interactive)
  (let ((str
	 (format "%.03f seconds"
		 (float-time
		  (time-subtract after-init-time before-init-time)))))
    (if (called-interactively-p 'interactive)
        (message "%s" str)
      str)))

(run-with-idle-timer
 0.1 nil
 (lambda ()
   ; Open most recent file at startup
   ;(require 'recentf)
   (recentf-mode 1)
   (recentf-open-most-recent-file 1)

   (let ((str
	  (format "%.03f seconds"
		  (float-time
		   (time-subtract after-init-time before-init-time)))))
     (message "Emacs loaded in %s" str))
   )
 )



;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;; (provide 'init.el)
;;; init.el ends here
