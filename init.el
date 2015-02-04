;;; init.el --- my emacs init things
;;; Commentary: hi!
;; Some helper fns. Blatantly copied from overtone's live-coding

;; Create a variable to store the path to this dotfile directory
;; (Usually ~/.emacs.d)

;;; Code:
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Create helper fns for loading dotfile paths and files
(defun add-dotfile-path (p)
  (add-to-list 'load-path (concat dotfiles-dir p)))

(defun load-dotfile (f)
  (load-file (concat dotfiles-dir f)))

(defun my-add-path (path-element)
  "Add the specified path element to the Emacs PATH"
  (interactive "Enter directory to be added to path: ")
  (if (file-directory-p path-element)
      (setenv "PATH"
              (concat (expand-file-name path-element)
                      path-separator (getenv "PATH")))))

;; ;; init the packages system

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; ;; ;; Add in your own as you wish:
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      color-theme
                      markdown-mode
                      auto-complete
                      multiple-cursors
                      cider
                      ac-cider
                      clojure-mode
                      clj-refactor
                      flx-ido
                      column-enforce-mode
                      powerline
                      dakrone-theme
                      leuven-theme
                      gist)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq mac-allow-anti-aliasing 't)
(set-frame-font "-apple-Anonymous_Pro-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(set-frame-font "-apple-PT_Mono-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")

;; custom paths added to the default PATH
(my-add-path "/usr/local/bin/")
(my-add-path "/Users/ulises/bin/")

(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;;; Erlang things
(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

;; erlang-mode
(add-to-list 'load-path "/usr/local/Cellar/erlang/R16B03-1/lib/erlang/lib/tools-2.6.13/emacs/")

(setq erlang-root-dir "/usr/local/Cellar/erlang/R16B03-1")
(setq exec-path (cons "/usr/local/Cellar/erlang/R16B03-1/bin" exec-path))
(require 'erlang-start)

;; ;; Some Erlang customizations
;; (defun erlang-opts ()
;;   (interactive)
;;   (setq inferior-erlang-machine-options
;;         (append '("-sname" "emacs")
;;                 (if (y-or-n-p "Use dbcore settings?")
;;                     '("-remsh" "dev1@127.0.0.1"
;;                       "-setcookie" "monster" "-hidden")))))

;; (add-hook 'erlang-mode-hook
;; 	  (lambda ()
;; 	    ;; when starting an Erlang shell in Emacs, default in the node name
;; 	    ;; (setq inferior-erlang-machine-options '("-sname" "emacs@hubert-cumberdale"
;;         ;;                                         "-remsh" "dev1@hubert-cumberdale"
;;         ;;                                         "-setcookie" "monster" "-hidden"
;;         ;;                                         "-boot" "/Users/ulises/development/dbcore/rel/dev1/releases/BUILD_NUMBER_GOES_HERE/start_clean"))
;; 	    ;; add Erlang functions to an imenu menu
;; 	    (imenu-add-to-menubar "imenu")))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;;; ac-complete customisations
(setq ac-auto-start 4)
(global-set-key (kbd "M-TAB") 'ac-start)

(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'ac-modes 'erlang-mode)
(add-to-list 'ac-modes 'clojure-mode)

;;; set the indentation to spaces *AFTER* loading python things :/
(setq-default indent-tabs-mode nil)

;;; delete trailing space in various languages
(add-hook 'python-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

(add-hook 'erlang-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

(add-hook 'clojure-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (delete-trailing-whitespace))))))

;; ;; generic stuff global to pretty much everything
(menu-bar-mode)
(setq-default show-trailing-whitespace t)

;; multiple-cursors
(require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C-c C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-previous-like-this)

;; (autoload 'jedi:setup "jedi" nil t)

;; (setq jedi:setup-keys t)
;; (add-hook 'python-mode-hook 'jedi:setup)

(global-set-key (kbd "C-c f .") 'flymake-goto-next-error)
(global-set-key (kbd "C-c f ,") 'flymake-goto-prev-error)

(put 'narrow-to-region 'disabled nil)

;; flx stuff
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
;; (setq ido-use-faces nil)

;; clj-refactor
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               ;; insert keybinding setup here
                               (cljr-add-keybindings-with-prefix "C-c")

                               (yas/minor-mode 1)
                               (auto-complete-mode)))

(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)

(eval-after-load 'clojure-mode
  '(define-key clojure-mode-map (kbd "C-c a l") 'align-cljlet))

;; ;; cider tuning
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-print-length 100) ; the default is nil, no limit
(setq cider-repl-wrap-history t)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'auto-complete-mode)

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(setq ac-quick-help-delay 0.5)

;; (require 'ac-nrepl)
;; (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'cider-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'cider-repl-mode))

;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions '(auto-complete)))
;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; (add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-cider-popup-doc))

(add-hook 'clojure-mode-hook 'column-enforce-mode)

(require 'powerline)
(powerline-default-theme)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop-all)

(load-theme 'leuven t)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(eval-after-load 'haskell-mode
          '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(custom-set-variables
 '(haskell-process-type 'cabal-repl)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-tags-on-save t))
(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map
       (kbd "C-c C-l")
       'haskell-process-load-or-reload)
     (define-key haskell-mode-map
       (kbd "C-c C-z")
       'haskell-interactive-switch)
     (define-key haskell-mode-map
       (kbd "C-c C-n C-t")
       'haskell-process-do-type)
     (define-key haskell-mode-map
       (kbd "C-c C-n C-i")
       'haskell-process-do-info)
     (define-key haskell-mode-map
       (kbd "C-c C-n C-c")
       'haskell-process-cabal-build)
     (define-key haskell-mode-map
       (kbd "C-c C-n c")
       'haskell-process-cabal)
     (define-key haskell-mode-map
       (kbd "SPC")
       'haskell-mode-contextual-space)
     (define-key haskell-mode-map
       (kbd "C-c C-o")
       'haskell-compile)))
(eval-after-load 'haskell-cabal
  '(progn
     (define-key haskell-cabal-mode-map
       (kbd "C-c C-z")
       'haskell-interactive-switch)
     (define-key haskell-cabal-mode-map
       (kbd "C-c C-k")
       'haskell-interactive-mode-clear)
     (define-key haskell-cabal-mode-map
       (kbd "C-c C-c")
       'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map
       (kbd "C-c c")
       'haskell-process-cabal)
     (define-key haskell-cabal-mode-map
       (kbd "C-c C-o")
       'haskell-compile)))



;; start the emacs server
(server-start)

;; ;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("96b023d1a6e796bab61b472f4379656bcac67b3af4e565d9fb1b6b7989356610" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
