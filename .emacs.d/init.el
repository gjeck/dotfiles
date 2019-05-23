;; my packages
(setq my-package-list '(ag
			aggressive-indent
			cider
			clojure-mode
			counsel
			evil
			projectile
			rainbow-delimiters
			smartparens
			swift-mode
			which-key))

;; load packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install any missing packages
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

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
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c f") 'projectile-find-file)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ag projectile cider which-key swift-mode aggressive-indent smartparens rainbow-delimiters counsel evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

