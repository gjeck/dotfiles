;; my packages
(setq my-package-list '(aggressive-indent
			cider
			clojure-mode
			counsel
			evil
			rainbow-delimiters
			smartparens))

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

;; enable evil mode (vim key-bindings)
(require 'evil)
(evil-mode 1)

;; enable ivy mode (fuzzy search)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(require 'smartparens-config)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'aggressive-indent-mode)
(add-hook 'prog-mode-hook #'smartparens-mode)

;; hack to get rainbow-delimiters to work with solarized theme
(outline-minor-mode t)
(outline-minor-mode nil)

;; search for file with C-s
(global-set-key (kbd "C-s") 'counsel-find-file)

;; move between windows with shift + arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(add-hook 'cider-repl-mode-hook
	  '(lambda () (define-key cider-repl-mode-map (kbd "C-l")
			'cider-repl-clear-buffer)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (aggressive-indent smartparens rainbow-delimiters counsel evil cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
      
