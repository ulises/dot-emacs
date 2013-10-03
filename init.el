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
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; ;; Add in your own as you wish:
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      nrepl
                      ;;                      scala-mode
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
                      magit
                      magithub
                      magit-gh-pulls
                      hideshowvis
                      jedi
                      nose
                      base16-theme
                      wisp-mode)
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
;; (set-default-font "-apple-Menlo-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
(set-default-font "-apple-Anonymous_Pro-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;; (set-default-font "-apple-Source_Code_Pro_for_Powerline-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;; (set-default-font "-apple-Source_Code_Pro-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;; (set-default-font "-apple-PT_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;; (color-theme-solarized-dark)
;; (color-theme-taylor)
;; (and (boundp 'custom-safe-themes)
;;      (load-theme 'solarized-dark t))

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
;; (require 'erlang-flymake)

;; (defun erlang-flymake-bigcouch-get-include-dirs ()
;;   (list (concat (erlang-flymake-get-app-dir) "include")
;;         "/Users/ulises/development/dbcore/deps"
;;         "/Users/ulises/development/sneezy/deps"))

;; (setq erlang-flymake-get-include-dirs-function
;;       'erlang-flymake-bigcouch-get-include-dirs)

;; (erlang-flymake-only-on-save)

;; edts
(add-to-list 'load-path "/Users/ulises/development/edts/")
(require 'edts-start)

;; ;; distel for more erlang goodness
;; (add-to-list 'load-path "/Users/ulises/development/distel-orig/elisp")
;; ;; ;; (setq erlang-indent-level 4)
;; ;; ;; (setq erlang-tab-always-indent t)
;; ;; ;; (setq erlang-electric-commands t)
;; ;; (require 'erlang-start)
;; (require 'distel)
;; (distel-setup)

;; eunit support
;; (add-to-list 'load-path "/usr/local/Cellar/erlang/R15B03/lib/erlang/lib/tools-2.6.8/emacs/")
;; (require 'erlang-eunit)

;; Some Erlang customizations

(defun erlang-opts ()
  (interactive)
  (setq inferior-erlang-machine-options
        (append '("-sname" "edts@hubert-cumberdale")
                (if (y-or-n-p "Use dbcore settings?")
                    '("-remsh" "dev1@hubert-cumberdale"
                      "-setcookie" "monster" "-hidden"
                      "-boot"
                      "/Users/ulises/development/dbcore/rel/dev1/releases/BUILD_NUMBER_GOES_HERE/start_clean")))))

(add-hook 'erlang-mode-hook
	  (lambda ()
	    ;; when starting an Erlang shell in Emacs, default in the node name
	    (setq inferior-erlang-machine-options '("-sname" "emacs@hubert-cumberdale"
                                                "-remsh" "dev1@hubert-cumberdale"
                                                "-setcookie" "monster" "-hidden"
                                                "-boot" "/Users/ulises/development/dbcore/rel/dev1/releases/BUILD_NUMBER_GOES_HERE/start_clean"))
	    ;; add Erlang functions to an imenu menu
	    (imenu-add-to-menubar "imenu")))

;; A number of the erlang-extended-mode key bindings are useful in the shell too
;; (defconst distel-shell-keys
;;   '(("\C-\M-i"   erl-complete)
;;     ("\M-?"      erl-complete)
;;     ("\M-."      erl-find-source-under-point)
;;     ("\M-,"      erl-find-source-unwind)
;;     ("\M-*"      erl-find-source-unwind))
;;   "Additional keys to bind when in Erlang shell.")

;; (add-hook 'erlang-shell-mode-hook
;; 	  (lambda ()
;; 	    ;; add some Distel bindings to the Erlang shell
;; 	    (dolist (spec distel-shell-keys)
;; 	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

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
 '(Linum-format "%7i ")
 '(ansi-color-names-vector ["#272822" "#F92672" "#A6E22E" "#FD971F" "#66D9EF" "#AE81FF" "#A1EFE4" "#F8F8F2"])
 '(ansi-term-color-vector [unspecified "#110F13" "#b13120" "#719f34" "#ceae3e" "#7c9fc9" "#7868b5" "#009090" "#F4EAD5"])
 '(background-color "#042028")
 '(background-mode dark)
 '(column-number-mode t)
 '(cursor-color "#708183")
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("8260d6fbe31eaff0636b18c0d970ebcc7d9413afc40df9e16a2662d5bea45b59" "a68fa33e66a883ce1a5698bc6ff355b445c87da1867fdb68b9a7325ee6ea3507" "fa189fcf5074d4964f0a53f58d17c7e360bb8f879bd968ec4a56dc36b0013d29" "76b9b3780c4844712e4a3ab05b8669eecd56a3864aae29e54005ffc68c24414c" "f89e21c3aef10d2825f2f079962c2237cd9a45f4dc1958091be8a6f5b69bb70c" "383806d341087214fd44864170161c6bf34a41e866f501d1be51883e08cb674b" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "60e70079a187df634db25db4bb778255eaace1ef4309e56389459fb9418b4840" "c377a5f3548df908d58364ec7a0ee401ee7235e5e475c86952dc8ed7c4345d8e" "8f6537eb6f9d66b060c736f5f680f5c661e0a6b311b86defa293bc5ba104a030" "1278386c1d30fc24b4248ba69bc5b49d92981c3476de700a074697d777cb0752" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "ca3bf8a7c831776c77d09ded89f2f0993dbdd9cb0765d8db061d1ebff806f41c" "b1cbf9910beb0e3655a779d1d4db5b4892a9e9968c7166be2c3f4c6574055fa8" "60a2ebd7effefeb960f61bc4772afd8b1ae4ea48fae4d732864ab9647c92093a" "5ce9c2d2ea2d789a7e8be2a095b8bc7db2e3b985f38c556439c358298827261c" "4be0cb1919fc15bfb879960ac270da77bf8a5d162fd2b4db7ce8969d188eeb3a" "f8d59d5af01b435272cc3a537f7a239b823b47d9d18b4721f9f474f2a21c5abc" "6f3060ac8300275c990116794e1ba897b6a8af97c51a0cb226a98759752cddcf" "978bd4603630ecb1f01793af60beb52cb44734fc14b95c62e7b1a05f89b6c811" "3341f6db5ac17e4174f7488c40676e7f0464f1e88519a59303dc7e7774245bbf" "8874901e0011a7b07e546b65be1726c4cc3f35cf1a60f8805e6cb5bb59ba305c" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "d0ff5ea54497471567ed15eb7279c37aef3465713fb97a50d46d95fe11ab4739" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(edts-man-root "~/.emacs.d/edts/doc/R15B01")
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#383838")
 '(foreground-color "#708183")
 '(fringe-mode 4 nil (fringe))
 '(main-line-color1 "#1e1e1e")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(powerline-color1 "#1e1e1e")
 '(powerline-color2 "#111111")
 '(safe-local-variable-values (quote ((allout-layout . t) (erlang-indent-level . 4) (whitespace-line-column . 80) (lexical-binding . t))))
 '(send-mail-function (quote sendmail-send-it))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(wrangler-search-paths (quote ("/Users/ulises/development/")))
 '(znc-servers (quote (("znc.cloudant.net" 55556 t ((network-slug "ulises" "truc0znc")))))))

;; generic stuff global to pretty much everything
(menu-bar-mode)
(setq-default show-trailing-whitespace t)

;; start the emacs server
(server-start)

;; autocomplete in nREPL
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
      (with-current-buffer buffer
        (beginning-of-buffer)
        (let ((_ignored (re-search-forward "Ran [0-9]+ tests"))
              (results (match-string-no-properties 0)))
          (message "%s: ALL GOOD" results))
        (bury-buffer buffer)
        (switch-to-prev-buffer (get-buffer-window buffer) 'kill))))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(setq nose-use-verbose nil) ; default is t

(global-set-key (kbd "C-c f .") 'flymake-goto-next-error)
(global-set-key (kbd "C-c f ,") 'flymake-goto-prev-error)

(put 'narrow-to-region 'disabled nil)

;; Teach compile the syntax of the kibit output
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
         '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'kibit)

;; A convenient command to run "lein kibit" in the project to which
;; the current emacs buffer belongs to.
(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))

(defun kibit-current-file ()
  "Run kibit on the current file.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile (concat "lein kibit " buffer-file-name)))
