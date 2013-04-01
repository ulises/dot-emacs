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
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
;;                      erlang
                      markdown-mode
                      auto-complete
                      flymake
                      flymake-cursor
                      ac-nrepl
                      starter-kit-js
                      multiple-cursors
                      magithub
                      magit-gh-pulls
                      nrepl-ritz
                      midje-mode
                      hideshowvis
                      jedi
                      nose)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; work with utf in slime
(setq slime-net-coding-system 'utf-8-unix)

(setq mac-allow-anti-aliasing 't)
;; (set-default-font "-apple-Inconsolata-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")
;; (set-default-font "-apple-inconsolata-regular-r-normal--16-130-72-72-m-130-iso10646-1")
;; (set-default-font "-apple-Ubuntu_Mono-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")
;; (set-default-font "-apple-Menlo-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
(set-default-font "-apple-Source_Code_Pro_for_Powerline-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;; (set-default-font "-apple-PT_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;; (color-theme-solarized-dark)
;; (color-theme-taylor)
(and (boundp 'custom-safe-themes)
     (load-theme 'deeper-blue t))

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


;; erlang-mode
(add-to-list 'load-path "/usr/local/Cellar/erlang/R15B03/lib/erlang/lib/tools-2.6.8/emacs/")

(setq erlang-root-dir "/usr/local/Cellar/erlang/R15B03/")
(setq exec-path (cons "/usr/local/Cellar/erlang/R15B03/bin" exec-path))
(require 'erlang-start)
(require 'erlang-flymake)

(defun erlang-flymake-bigcouch-get-include-dirs ()
  (list (concat (erlang-flymake-get-app-dir) "include")
        "/Users/ulises/development/dbcore/deps"
        "/Users/ulises/development/sneezy/deps"))

(setq erlang-flymake-get-include-dirs-function
      'erlang-flymake-bigcouch-get-include-dirs)

(erlang-flymake-only-on-save)

;; edts
;; (setq erlang-root-dir "/usr/local/Cellar/erlang/R15B03/")
;; (add-to-list 'load-path "/Users/ulises/development/edts/")
;; (require 'edts-start)

;; (setq edts-projects
;;       '(( ;; LYSE distributed reminder thing
;;          (root       . "~/development/lyse-organiser"))
;;         ( ;; My awesome project.
;;          (name       . "sneezy")
;;          (root       . "~/development/sneezy")
;;          (node-sname . "sneezy")
;;          (start-command . "make run")))

;; ;; distel for more erlang goodness
;; (add-to-list 'load-path "/Users/ulises/development/jixiuf-distel/elisp")
(add-to-list 'load-path "/Users/ulises/development/distel/elisp")
(setq erlang-indent-level 4)
(setq erlang-tab-always-indent t)
(setq erlang-electric-commands t)

(require 'distel)
(distel-setup)

;; (add-to-list 'load-path "/Users/ulises/development/wrangler/elisp")
;; should add my own version of distel *after* wrangler's since
;; add-to-list prepends entries
;; (add-to-list 'load-path "/Users/ulises/development/jixiuf-distel/elisp")
;; (require 'wrangler)

;; eunit support
(add-to-list 'load-path "/usr/local/Cellar/erlang/R15B03/lib/erlang/lib/tools-2.6.8/emacs/")
(require 'erlang-eunit)

;; Some Erlang customizations
(add-hook 'erlang-mode-hook
	  (lambda ()
	    ;; when starting an Erlang shell in Emacs, default in the node name
	    (setq inferior-erlang-machine-options '("-name" "emacs"))
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

(add-to-list 'load-path "~/.emacs.d/plugin/")
;; (require 'ac-python)
;; (add-to-list 'ac-modes 'python-mode)

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

(add-hook 'clojure-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (delete-trailing-whitespace))))))
(add-hook 'clojure-mode-hook 'hideshowvis-enable)

;; automatically check with flymake

(add-hook 'find-file-hook 'flymake-find-file-hook)
(load-library "flymake-cursor")

(when (load "flymake" t)
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
 '(safe-local-variable-values (quote ((erlang-indent-level . 4) (whitespace-line-column . 80) (lexical-binding . t))))
 '(wrangler-search-paths (quote ("/Users/ulises/development/"))))

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

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C-c C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-previous-like-this)

;; Integrate terminal emacs with OSX pasteboard
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; quickfix mode
(add-to-list 'load-path "/Users/ulises/development/quickfix-mode")
(require 'quickfix-mode)
(load-file "~/development/quickfix-mode/quickfix-erlang.el")

;; full-screen support \o/
(global-set-key [s-return] 'ns-toggle-fullscreen)

;; Haskell things
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; hslint on the command line only likes this indentation mode;
;; alternatives commented out below.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

(setq my-centred
      [?\C-x ?1 ?\C-x ?3 ?\C-x ?3 ?\C-x ?o ?\C-x ?o ?\C-x ?3 ?\C-x ?0 ?\C-x ?o ?\C-x ?b ?e ?m ?p ?t ?y ?b ?u ?f ?f ?e ?r return return ?\C-x ?o ?\C-x ?b ?e ?m ?p ?t ?y return ?\C-x ?o])
(put 'my-centred 'kmacro t)
(defun centre-buffer ()
  (interactive)
  (execute-kbd-macro my-centred))
(global-set-key (kbd "C-c c") 'centre-buffer)

(autoload 'jedi:setup "jedi" nil t)

(setq jedi:setup-keys t)
(add-hook 'python-mode-hook 'jedi:setup)
(require 'nose)

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-ca" 'nosetests-all)
            (local-set-key "\C-cm" 'nosetests-module)
            (local-set-key "\C-c." 'nosetests-one)))

(defun run-nosetests-if-python-file ()
  (if (string-match ".py$" (buffer-name))
      (nosetests-all)))

(add-hook 'after-save-hook
          (lambda ()
            (run-nosetests-if-python-file)))

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings."
  (if (and
       (string-match "nosetests" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "FAILED" nil t))))
      (progn
        (message "TESTS PASSED.")
        (bury-buffer buffer)
        (switch-to-prev-buffer (get-buffer-window buffer) 'kill))))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(setq nose-use-verbose nil) ; default is t

(global-set-key (kbd "C-c f .") 'flymake-goto-next-error)
(global-set-key (kbd "C-c f ,") 'flymake-goto-prev-error)

;;; mu4e stuff

(defun email ()
  (interactive)

  (when (not (featurep 'mu4e))
    (add-to-list 'load-path
                 "/usr/local/Cellar/mu/0.9.9/share/emacs/site-lisp/mu4e/")
    (require 'mu4e)

    (setq mu4e-maildir "~/Maildir")
    (setq mu4e-drafts-folder "/Cloudant/[Gmail].Drafts")
    (setq mu4e-sent-folder   "/Cloudant/[Gmail].Sent Mail")
    (setq mu4e-trash-folder  "/Cloudant/[Gmail].Trash")

    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
    (setq mu4e-sent-messages-behavior 'delete)

    ;; setup some handy shortcuts
    ;; you can quickly switch to your Inbox -- press ``ji''
    ;; then, when you want archive some messages, move them to
    ;; the 'All Mail' folder by pressing ``ma''.

    (setq mu4e-maildir-shortcuts
          '( ("/Cloudant/INBOX"               . ?i)
             ("/Cloudant/[Gmail].IMPORTANT"   . ?!)
             ("/Cloudant/[Gmail].Sent Mail"   . ?s)
             ("/Cloudant/[Gmail].Trash"       . ?t)
             ("/Cloudant/[Gmail].All Mail"    . ?a)))

    ;; allow for updating mail using 'U' in the main view:
    ;; I have this running in the background anyway
    (setq mu4e-get-mail-command "offlineimap")

    ;; something about ourselves
    (setq
     user-mail-address "ulises@cloudant.com"
     user-full-name  "Ulises Cervino"
     message-signature nil)

    ;; sending mail -- replace USERNAME with your gmail username
    ;; also, make sure the gnutls command line utils are installed
    ;; package 'gnutls-bin' in Debian/Ubuntu

    (require 'smtpmail)

    (setq sendmail-program (executable-find "msmtp"))

    ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)

    ;; show images
    (setq mu4e-show-images t)

    ;; use imagemagick, if available
    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))

    ;;; message view action
    (defun mu4e-msgv-action-view-in-browser (msg)
      "View the body of the message in a web browser."
      (interactive)
      (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
            (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
        (unless html (error "No html part for this message"))
        (with-temp-file tmpfile
          (insert
           "<html>"
           "<head><meta http-equiv=\"content-type\""
           "content=\"text/html;charset=UTF-8\">"
           html))
        (browse-url (concat "file://" tmpfile))))

    (add-to-list 'mu4e-view-actions
                 '("View in browser" . mu4e-msgv-action-view-in-browser) t)

    ;; need this to convert some e-mails properly
    (setq mu4e-html2text-command "elinks -dump"))
  (mu4e))

(put 'narrow-to-region 'disabled nil)
