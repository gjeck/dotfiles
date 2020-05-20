(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; add all theme files
(let ((basedir "~/.emacs.d/themes/"))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
	     (file-directory-p (concat basedir f)))
	(add-to-list 'custom-theme-load-path (concat basedir f)))))

;; load the solarized theme
(load-theme 'solarized t)
(set-terminal-parameter nil 'background-mode 'dark)
(enable-theme 'solarized)

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; Show line numbers
(global-linum-mode)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; enable spell check
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; enable evil mode (vim key-bindings)
(require 'evil)
(evil-mode 1)
(setq-default evil-escape-key-sequence "jk")

;; enable ivy mode (fuzzy search)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
	(t      . ivy--regex-fuzzy)))

;; counsel global key bindings
(global-set-key (kbd "C-s") 'swiper)

;; projectile
(projectile-mode +1)

;; helm-projectile (fuzzy search for projectile)
(require 'helm-projectile)
(helm-projectile-on)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c f") 'helm-projectile-find-file)
(define-key projectile-mode-map (kbd "C-c a") 'helm-projectile-ag)

(require 'smartparens-config)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'aggressive-indent-mode)
(add-hook 'prog-mode-hook #'smartparens-mode)

;; hack to get rainbow-delimiters to work with solarized theme
(outline-minor-mode t)
(outline-minor-mode nil)

;; move between windows with shift + arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(add-hook 'cider-repl-mode-hook
	  '(lambda () (define-key cider-repl-mode-map (kbd "C-l")
			'cider-repl-clear-buffer)))

;; guided key-binding for incomplete commands
(require 'which-key)
(which-key-mode)

;; disable emacs backup and auto-save files
(setq backup-directory-alist nil)
(setq auto-save-file-name-transforms nil)

;; neotree
(require 'neotree)
(global-set-key (kbd "C-c t") 'neotree-toggle)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-ag helm-projectile neotree yasnippet which-key web-mode use-package swift-mode smex smartparens rainbow-delimiters projectile prodigy popwin pallet evil counsel cider aggressive-indent ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; overwrite some ivy highlighting for mini-buffer
 ;; use `'M-x list-faces-display` to show examples' 
 '(ivy-minibuffer-match-face-1 ((t (:background "#404040")))) 
 '(ivy-minibuffer-match-face-2 ((t (:background "#555555")))) 
 '(ivy-minibuffer-match-face-3 ((t (:background "#6B396B")))) 
 '(ivy-minibuffer-match-face-4 ((t (:background "#804480")))) 
 )
