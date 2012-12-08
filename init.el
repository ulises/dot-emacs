;; Some helper fns. Blatantly copied from overtone's live-coding

;; Create a variable to store the path to this dotfile directory
;; (Usually ~/.emacs.d)
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

;; init the packages system

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; ;; Add in your own as you wish:
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      nrepl
                      scala-mode
                      color-theme
                      color-theme-solarized
                      color-theme-monokai
                      erlang
                      markdown-mode
                      auto-complete
                      flymake
                      flymake-cursor
                      ac-nrepl
                      starter-kit-js
                      multiple-cursors
                      magithub
                      magit-gh-pulls
                      nrepl-ritz)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; work with utf in slime
(setq slime-net-coding-system 'utf-8-unix)

(setq mac-allow-anti-aliasing t)
;; (set-default-font "-apple-Inconsolata-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")
;; (set-default-font "-apple-inconsolata-regular-r-normal--18-130-72-72-m-130-iso10646-1")
;; (set-default-font "-apple-Ubuntu_Mono-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")
;; (set-default-font "-apple-Menlo-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
(set-default-font "-apple-Source_Code_Pro_for_Powerline-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
(color-theme-solarized-dark)
(color-theme-monokai)

;; custom paths added to the default PATH
(my-add-path "/usr/local/bin/")
(my-add-path "/Users/ulises/bin/")

(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; custom modes for some file extensions

(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; distel for more erlang goodness
(add-to-list 'load-path "/Users/ulises/development/distel/elisp")
(require 'distel)
(distel-setup)

;; Some Erlang customizations
(add-hook 'erlang-mode-hook
	  (lambda ()
	    ;; when starting an Erlang shell in Emacs, default in the node name
	    (setq inferior-erlang-machine-options '("-sname" "emacs"))
	    ;; add Erlang functions to an imenu menu
	    (imenu-add-to-menubar "imenu")))

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind))
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
	  (lambda ()
	    ;; add some Distel bindings to the Erlang shell
	    (dolist (spec distel-shell-keys)
	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;; ac-complete customisations
(setq ac-auto-start 4)
(global-set-key (kbd "M-TAB") 'ac-start)

(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'ac-modes 'erlang-mode)

(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

;; set the indentation to spaces *AFTER* loading python things :/
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; space indentation and level in various languages
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 4))))

(add-hook 'javascript-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))
(add-hook 'js-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

(add-hook 'erlang-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 4))))

;;; delete trailing space in various languages
(add-hook 'python-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

(add-hook 'javascript-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

(add-hook 'js-mode-hook
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

(add-hook 'scala-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (delete-trailing-whitespace))))))

;; automatically check with flymake

(add-hook 'find-file-hook 'flymake-find-file-hook)
(load-library "flymake-cursor")

(when (load "flymake" t)
  (defun flymake-erlang-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name temp-file
                                           (file-name-directory buffer-file-name))))
      (list "/Users/ulises/bin/check-erl.sh" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))

  ;; (defun flymake-jslint-init ()
  ;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
  ;;                      'flymake-create-temp-inplace))
  ;;          (local-file (file-relative-name
  ;;                       temp-file
  ;;                       (file-name-directory buffer-file-name))))
  ;;     (list "/usr/local/bin/jshint" (list local-file))))

  ;; (add-to-list 'flymake-allowed-file-name-masks '("\\.js\\'" flymake-jslint-init))

  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/Users/ulises/bin/check-py.sh"  (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pyflakes-init))

  (defun flymake-scala-init ()
    (let* ((text-of-first-line (buffer-substring-no-properties (point-min) (min 20 (point-max)))))
      (progn
        (remove-hook 'after-save-hook 'flymake-after-save-hook t)
        (save-buffer)
        (add-hook 'after-save-hook 'flymake-after-save-hook nil t)
        (if (string-match "^//script" text-of-first-line)
            (list "/usr/local/bin/fsc" (list "-Xscript" "MainScript" "-d" "/tmp/" buffer-file-name))
          (list "/usr/local/bin/fsc" (list "-d" "/tmp/" buffer-file-name))))))

  (push '(".+\\.scala$" flymake-scala-init) flymake-allowed-file-name-masks)
  (push '("^\\(.*\\):\\([0-9]+\\): error: \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((nil (:underline t :slant italic))))
 '(flymake-warnline ((nil (:underline t :slant italic)))))

;; (setq flymake-no-changes-timeout 2)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; generic stuff global to pretty much everything
(menu-bar-mode)
(setq-default show-trailing-whitespace t)

;; start the emacs server
(server-start)

;; autocomplete in nREPL
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; multiple-cursors(require 'multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; midje mode cannot be installed from marmalade
;; see https://github.com/dnaumov/midje-mode/issues/14
(add-dotfile-path "plugin/midje-mode/")
(require 'midje-mode)
(require 'clojure-jump-to-file)
