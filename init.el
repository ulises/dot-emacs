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
                      color-theme
                      color-theme-solarized
                      color-theme-monokai
                      markdown-mode
                      auto-complete
                      starter-kit-js
                      multiple-cursors
                      magit
                      jedi
                      nose
                      base16-theme
                      cider
                      rainbow-mode
                      git-gutter+)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq mac-allow-anti-aliasing 't)
(set-frame-font "-apple-Source_Code_Pro-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
(set-frame-font "-apple-Andale_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
(set-frame-font "-apple-Inconsolata-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")

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
(add-to-list 'load-path "/opt/erlang/r16b/lib/tools-2.6.10/emacs")

(setq erlang-root-dir "/opt/erlang/r16b/")
(setq exec-path (cons "/opt/erlang/r16b/" exec-path))
(require 'erlang-start)

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

;; ac-complete customisations
(setq ac-auto-start 4)
(global-set-key (kbd "M-TAB") 'ac-start)

(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'ac-modes 'erlang-mode)
(add-to-list 'ac-modes 'clojure-mode)

(add-to-list 'load-path "~/.emacs.d/plugin/")

;; set the indentation to spaces *AFTER* loading python things :/
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; space indentation and level in various languages
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 4))))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#272822" "#F92672" "#A6E22E" "#FD971F" "#66D9EF" "#AE81FF" "#A1EFE4" "#F8F8F2"])
 '(ansi-term-color-vector [unspecified "#110F13" "#b13120" "#719f34" "#ceae3e" "#7c9fc9" "#7868b5" "#009090" "#F4EAD5"])
 '(background-color "#042028")
 '(background-mode dark)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cursor-color "#708183")
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes (quote ("968d1ad07c38d02d2e5debffc5638332696ac41af7974ade6f95841359ed73e3" "9c26d896b2668f212f39f5b0206c5e3f0ac301611ced8a6f74afe4ee9c7e6311" "8eef22cd6c122530722104b7c82bc8cdbb690a4ccdd95c5ceec4f3efa5d654f5" "91b5a381aa9b691429597c97ac56a300db02ca6c7285f24f6fe4ec1aa44a98c3" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "29a4267a4ae1e8b06934fec2ee49472daebd45e1ee6a10d8ff747853f9a3e622" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "7d4d00a2c2a4bba551fcab9bfd9186abe5bfa986080947c2b99ef0b4081cb2a6" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "eacfc96fbe418c017f4a00fdde5d5029db8e2800a46251eb2174484fa431917e" "7694bfb46ec19cfc47093783633d8cd4df208d620104910bf5c1c840528a8dd1" "f3ceb7a30f6501c1093bc8ffdf755fe5ddff3a85437deebf3ee8d7bed8991711" "47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" "4e1a057fe9a9981e886bd0673d131da904e3306c884715b8bee333ef95303e39" "dedecec68626c17a40f7cd4e41383348d232676b95ab92486157bc4d38c877ce" "92777c893be4ba0a3082a8c76ef840b892d3959fd652d3c97dce21167cdc5dcb" "543976df2de12eb2ac235c79c7bc1dac6c58f4a34ae6f72237d6e70d8384f37a" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "8260d6fbe31eaff0636b18c0d970ebcc7d9413afc40df9e16a2662d5bea45b59" "a68fa33e66a883ce1a5698bc6ff355b445c87da1867fdb68b9a7325ee6ea3507" "fa189fcf5074d4964f0a53f58d17c7e360bb8f879bd968ec4a56dc36b0013d29" "76b9b3780c4844712e4a3ab05b8669eecd56a3864aae29e54005ffc68c24414c" "f89e21c3aef10d2825f2f079962c2237cd9a45f4dc1958091be8a6f5b69bb70c" "383806d341087214fd44864170161c6bf34a41e866f501d1be51883e08cb674b" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "60e70079a187df634db25db4bb778255eaace1ef4309e56389459fb9418b4840" "c377a5f3548df908d58364ec7a0ee401ee7235e5e475c86952dc8ed7c4345d8e" "8f6537eb6f9d66b060c736f5f680f5c661e0a6b311b86defa293bc5ba104a030" "1278386c1d30fc24b4248ba69bc5b49d92981c3476de700a074697d777cb0752" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "ca3bf8a7c831776c77d09ded89f2f0993dbdd9cb0765d8db061d1ebff806f41c" "b1cbf9910beb0e3655a779d1d4db5b4892a9e9968c7166be2c3f4c6574055fa8" "60a2ebd7effefeb960f61bc4772afd8b1ae4ea48fae4d732864ab9647c92093a" "5ce9c2d2ea2d789a7e8be2a095b8bc7db2e3b985f38c556439c358298827261c" "4be0cb1919fc15bfb879960ac270da77bf8a5d162fd2b4db7ce8969d188eeb3a" "f8d59d5af01b435272cc3a537f7a239b823b47d9d18b4721f9f474f2a21c5abc" "6f3060ac8300275c990116794e1ba897b6a8af97c51a0cb226a98759752cddcf" "978bd4603630ecb1f01793af60beb52cb44734fc14b95c62e7b1a05f89b6c811" "3341f6db5ac17e4174f7488c40676e7f0464f1e88519a59303dc7e7774245bbf" "8874901e0011a7b07e546b65be1726c4cc3f35cf1a60f8805e6cb5bb59ba305c" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "d0ff5ea54497471567ed15eb7279c37aef3465713fb97a50d46d95fe11ab4739" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(edts-man-root "~/.emacs.d/edts/doc/R15B01")
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#383838")
 '(foreground-color "#708183")
 '(fringe-mode 4 nil (fringe))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors (quote (("#49483E" . 0) ("#67930F" . 20) ("#349B8D" . 30) ("#21889B" . 50) ("#968B26" . 60) ("#A45E0A" . 70) ("#A41F99" . 85) ("#49483E" . 100))))
 '(linum-format " %7i ")
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#1e1e1e")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(powerline-color1 "#1e1e1e")
 '(powerline-color2 "#111111")
 '(safe-local-variable-values (quote ((allout-layout . t) (erlang-indent-level . 4) (whitespace-line-column . 80) (lexical-binding . t))))
 '(send-mail-function (quote sendmail-send-it))
 '(syslog-debug-face (quote ((t :background unspecified :foreground "#A1EFE4" :weight bold))))
 '(syslog-error-face (quote ((t :background unspecified :foreground "#F92672" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#A6E22E"))))
 '(syslog-info-face (quote ((t :background unspecified :foreground "#66D9EF" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#E6DB74"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#FD5FF0"))))
 '(syslog-warn-face (quote ((t :background unspecified :foreground "#FD971F" :weight bold))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list (quote (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))

;; generic stuff global to pretty much everything
(menu-bar-mode)
(setq-default show-trailing-whitespace t)

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

(global-set-key (kbd "C-c f .") 'flymake-goto-next-error)
(global-set-key (kbd "C-c f ,") 'flymake-goto-prev-error)

(put 'narrow-to-region 'disabled nil)

(defun helm-clojure-headlines ()
  "Display headlines for the current Clojure file."
  (interactive)
  (helm :sources '(((name . "Clojure Headlines")
                    (volatile)
                    (headline "^[;(]")))))

(add-hook 'after-init-hook #'global-flycheck-mode)

;; flx stuff
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; haskell stuff
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; clj-refactor
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               ;; insert keybinding setup here
                               (cljr-add-keybindings-with-prefix "C-c")))
(add-hook 'clojure-mode-hook (lambda () (yas/minor-mode 1)))
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)


;; cider tuning
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-print-length 100) ; the default is nil, no limit
(setq cider-repl-wrap-history t)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))


(add-hook 'clojure-mode-hook 'column-enforce-mode)

;; git-gutter
(global-git-gutter+-mode t)

;; (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
;; (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

(setq git-gutter+-modified-sign " ") ;; two space
(setq git-gutter+-added-sign " ")  ;; multiple character is OK
(setq git-gutter+-deleted-sign " ")
(setq git-gutter+-unchanged-sign " ")
(setq git-gutter+-hide-gutter t)


(set-face-background 'git-gutter+-unchanged "grey10")
(set-face-foreground 'git-gutter+-unchanged "grey10")

;; (set-face-background 'git-gutter:modified "grey10")
;; (set-face-foreground 'git-gutter:modified "blue")
;; (set-face-background 'git-gutter:added "grey10")
;; (set-face-foreground 'git-gutter:added "green")
;; (set-face-background 'git-gutter:deleted "grey10")
;; (set-face-foreground 'git-gutter:deleted "red")

;; modifique aca
;; y aca

;; start the emacs server
(server-start)

;;; init.el ends here
