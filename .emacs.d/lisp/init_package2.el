;;; init_package --- settings about packages
;;; Commentary:
;;; Code:

(unless (package-installed-p 'use-package) (package-install 'use-package))

;; use-packageの:diminishを有効にし、モードラインをスッキリさせる
(use-package diminish :ensure t :defer t)

;; clangがあるとより便利らしいので、aptでclangをいれておく
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (global-company-mode 1)
  (setq completion-ignore-case t)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  :bind (("C-M-i" . company-complete)
         (:map company-active-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("<tab>" . company-complete-common-or-cycle)
               ("<backtab>" . company-select-previous)
               ("C-f" . company-complete-selection)
               ("C-d" . company-show-doc-buffer)
               ("C-s" . company-filter-candidates)
               ("C-h" . backward-delete-char)
               )))

(use-package company-quickhelp
  :ensure t
  :defer t
  :after company
  :init
  (company-quickhelp-mode t)
  :custom
  (company-quickhelp-delay 1)
  (company-transformers '(company-sort-by-backend-importance))
  )

(use-package company-irony-c-headers
  :ensure t
  :defer t
  :after (company irony)
  )

(use-package company-irony
  :ensure t
  :defer t
  :after (company irony)
  )

(use-package company-arduino
  :ensure t
  :defer t
  :after company
  )

;; aptかpipでjediを入れておく
;; aptでpython3-jediをいれておき、初回起動時にjedi:install-serverする
(use-package company-jedi
  :ensure t
  :defer t
   )

(use-package flycheck
   :ensure t
   :diminish flycheck-mode
   :init
   ;; 対応するメジャーモードでオート起動する
   (global-flycheck-mode)
   ;; エラー箇所に背景色をつける
   (set-face-background 'flycheck-error "pink")
   :bind (:map flycheck-mode-map
               ("M-p" . flycheck-previous-error)
               ("M-n" . flycheck-next-error)
               )
)

;; libclangが必要
(use-package irony
  :ensure t
  :defer t
  )

(use-package flycheck-irony
  :ensure t
  :defer t
  :after (flycheck irony)
  )

;; aptかpipでflake8を入れておく
;; どちらを使うかを選択しないといけない、闇
;; aptでflake8をいれておく
(use-package flycheck-pyflakes
  :ensure t
  :defer t
  :if (= 0 (shell-command "flake8 --version 1>/dev/null 2>/dev/null"))
  :init
  ;; (require 'flycheck-pyflakes)
  )

(use-package flycheck-ocaml
  :ensure t
  :defer t
  )

(use-package cc-mode
  :init
  (defun my-c-mode-hook ()
    "Setting for c-mode."
    (c-set-style "k&r")
    (setq c-basic-offset 2)
    (setq tab-width c-basic-offset)
    (setq indent-tabs-mode nil))
  (add-hook 'c-mode-hook 'my-c-mode-hook)
  (defun my-c++-mode-hook ()
    "Setting for c++-mode."
    (c-set-style "k&r")
    (setq c-basic-offset 2)
    (setq tab-width c-basic-offset)
    (setq indent-tabs-mode nil)
    (setq flycheck-gcc-language-standard "c++11")
    (setq flycheck-clang-language-standard "c++11"))
  (add-hook 'c++-mode-hook 'my-c++-mode-hook)
  )

;; (use-package tuareg
;;   :ensure t
;;   )
;; (use-package yasnippet
;;   :ensure t
;;   )
;; (use-package arduino-mode
;;   :ensure t
;;   )
;; (use-package rtags
;;   :ensure t
;;   )
;; (use-package python-mode
;;   :ensure t
;;   )
;; (use-package py-autopep8
;;   :ensure t
;;   )
;; (use-package quickrun
;;   :ensure t
;;   )

;; markdownコマンドをいれておく
(use-package markdown-mode
  :ensure t
  :defer t
  :if (= 0 (shell-command "markdown --version 1>/dev/null 2>/dev/null"))
  :init
  ;; ファイルロック機能と競合してハングするため、leoさんの松葉杖対処を導入
  ;; https://groups.google.com/forum/#!topic/gnu.emacs.help/AIy5megeSHA
  (defun leo-markdown-fontify-buffer-wiki-links-empty ()
    "Empty replacement for `markdown-fontify-buffer-wiki-links` due to hanging bug."
    (interactive))
  (eval-after-load "markdown-mode"
    '(progn (fset 'markdown-fontify-buffer-wiki-links
                  'leo-markdown-fontify-buffer-wiki-links-empty)))
  :custom
  (markdown-command "markdown")
  ;; style sheetは生成HTMLと同フォルダにあるstyle.cssにする
  (markdown-css-paths '("style.css"))
  )


;; markdownでコードブロックの編集のために必要
(use-package edit-indirect
  :ensure t
  :defer t
  )

;; (use-package csv-mode
;;   :ensure t
;;   )

(use-package sml-mode
  :ensure t
  :mode ("\\.smi\\'" "\\.ppg\\'")
  :interpreter "smlsharp"
  :custom
  (sml-indent-level 2)
  (sml-indent-args 2)
  ;; sml-modeのrun-smlでデフォルトSMLコマンドをsmlsharpにする
  (sml-program-name "smlsharp")
  )

;; aptでgnupgを入れておく
;; alpaca.elが必要
(use-package twittering-mode
  :ensure t
  :init
  ;;(setq twittering-suffix-space-size 8)
  ;; URLを青文字にする
  (add-hook 'twittering-mode-hook
            (lambda () (set-face-foreground 'twittering-uri-face "blue")))
  :custom
  ;; use master passworad compressed by GnuPG
  (twittering-use-master-password t)
  (twittering-private-info-file "~/.emacs.d/twittering-mode.gpg")
  (twittering-use-icon-storage t)
  (twittering-icon-storage-file "~/.emacs.d/icons.gz")
  (twittering-status-format
   (concat
    "%i %S(@%s) "
    "[%FACE[underline]{%@{%Y-%m-%d %H:%M}}]"
    "%FACE[twittering-timeline-footer-face]{"
    "%FIELD-IF-NONZERO[  ↩%s]{retweet_count}"
    " %FIELD-IF-NONZERO[♡%s]{favorite_count}"
    "}"
    "\n"
    "%RT{ %FACE[bold]{RT} by %S(@%s)\n}"
    "%FOLD[]{%T}\n"
    "-------------------------------------------------------------------------------"))
  ;;(twittering-convert-fix-size 24)
  (twittering-timer-interval 600)
  (twittering-number-of-tweets-on-retrieval 100)
  (twittering-display-remaining t)
  (twittering-initial-timeline-spec-string
   '(
     "keita44_f4/friend"
     ":replies"
     ":home"
     ))
  ;;(twittering-tinyurl-service 'goo.gl)
  ;;(twittering-retweet-format " RT @%s %t")
  (twittering-retweet-format " %u")
  (twittering-fill-column 80)
  (twittering-edit-skeleton 'inherit-mentions)
  :config
  ;; use icon
  (twittering-icon-mode t)
  :bind (:map twittering-mode-map
              ("R" . twittering-native-retweet)
              ("r" . twittering-enter)
              ("T" . twittering-organic-retweet)
              ("t" . twittering-update-status-interactive)
              ("o" . twittering-goto-next-uri)
              ("O" . twittering-push-uri-onto-kill-ring)
              ("J" . end-of-buffer)
              ("K" . twittering-goto-first-status)
              ("u" . twittering-toggle-show-replied-statuses)
              ))

;; (use-package gnuplot-mode
;;   :ensure t
;;   )
;; (use-package graphviz-dot-mode
;;   :ensure t
;;   )
;; (use-package google-translate
;;   :ensure t
;;   )
;; (use-package bash-completion
;;   :ensure t
;;   )
;; (use-package haxe-mode
;;   :ensure t
;;   )
;; (use-package proof-general
;;   :ensure t
;;   )
;; (use-package popwin
;;   :ensure t
;;   )
;; (use-package image-dired+
;;   :ensure t
;;   )
;; (use-package image+
;;   :ensure t
;;   )
;; (use-package fill-column-indicator
;;   :ensure t
;;   )
;; (use-package hiwin
;;   :ensure t
;;   )
;; (use-package highlight-current-line
;;   :ensure t
;;   )

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :commands rainbow-mode
  :hook (c++-mode arduino-mode)
  :custom
  (rainbow-r-colors t)    ; R color listを使う
  (rainbow-html-colors t) ; html color listを使う
  )

;; (use-package w3m
;;   :ensure t
;;   )

(use-package smartparens
  :ensure t
  :defer t
  :diminish smartparens-mode
  :init
  (smartparens-global-mode t)
  ;; 一部のモードでは'での補完を行わない
  (sp-local-pair '(emacs-lisp-mode) "'" nil :actions nil)
  (sp-local-pair '(lisp-mode) "'" nil :actions nil)
  (sp-local-pair '(sml-mode) "'" nil :actions nil)
  (sp-local-pair '(inferior-sml-mode) "'" nil :actions nil)
  (sp-local-pair '(tuareg-mode) "'" nil :actions nil)
  )

;; (use-package browse-kill-ring
;;   :ensure t
;;   )

(use-package ivy
  :ensure t
  :defer t
  :custom
  (ivy-count-format "(%d/%d) ")
  )

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("M-r" . counsel-command-history)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-b" . counsel-ibuffer)
         ("C-x b" . counsel-switch-buffer)
         ("C-c C-d" . counsel-describe-function)
         ("C-c C-g" . counsel-git-grep)
         )
  :bind (:map counsel-find-file-map
              ("C-h" . counsel-up-directory)
              ))

(use-package swiper
  :ensure t
  :custom
  (swiper-include-line-number-in-search t)
  :bind (("C-s" . swiper)
         ("C-c s" . isearch-forward)
         )
  :bind (:map swiper-map
              ("M-%" . swiper-query-replace))
  )

;; (use-package esup
;;   :ensure t
;;   )

(use-package shell
  :init
  (bash-completion-setup)
  :custom
  ;; Emacsを起動したshellを使用する（bashからの起動を前提）
  (explicit-shell-file-name (getenv "SHELL"))
  (explicit-bash-args '("--login" "-i"))
  ;; shell-modeでのファイル名補完
  (shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")
  :hook (shell-mode . (lambda ()
                        ;; SHELL で ^M が付く場合は ^M を削除する
                        (set-buffer-process-coding-system
                         'undecided-dos 'sjis-unix)))
)

(use-package dired
  :init
  ;; サイズや拡張子による並び替えを追加する．
  ;; http://d.hatena.ne.jp/mooz/20091207/p1
  (defvar dired-various-sort-type
    '(("S" . "size")
      ("X" . "extension")
      ("v" . "version")
      ("t" . "date")
      (""  . "name")))
  (defun dired-various-sort-change (sort-type-alist &optional prior-pair)
    "Dired various sort change by SORT-TYPE-ALIST and PRIOR-PAIR."
    (when (eq major-mode 'dired-mode)
      (let* (case-fold-search
             get-next
             (options
              (mapconcat 'car sort-type-alist ""))
             (opt-desc-pair
              (or prior-pair
                  (catch 'found
                    (dolist (pair sort-type-alist)
                      (when get-next
                        (throw 'found pair))
                      (setq get-next
                            (string-match (car pair) dired-actual-switches)))
                    (car sort-type-alist)))))
        (setq dired-actual-switches
              (concat "-l" (dired-replace-in-string (concat "[l" options "-]")
                                                    ""
                                                    dired-actual-switches)
                      (car opt-desc-pair)))
        (setq mode-name
              (concat "Dired by " (cdr opt-desc-pair)))
        (force-mode-line-update)
        (revert-buffer))))
  (defun dired-various-sort-change-or-edit (&optional arg)
    "Dired various sort change or edit by ARG."
    (interactive "P")
    (when dired-sort-inhibit
      (error "Cannot sort this dired buffer"))
    (if arg
        (dired-sort-other
         (read-string "ls switches (must contain -l): " dired-actual-switches))
      (dired-various-sort-change dired-various-sort-type)))

  ;; diredでディレクトリを移動してもバッファを新規に作成しない
  (defun dired-my-advertised-find-file ()
    (interactive)
    (let ((kill-target (current-buffer))
          (check-file (dired-get-filename nil t)))
      (funcall 'dired-find-file)
      (if (file-directory-p check-file)
          (kill-buffer kill-target))))
  (defun dired-my-up-directory (&optional other-window)
    "Run dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer.
Creates a buffer if necessary."
    (interactive "P")
    (let* ((dir (dired-current-directory))
           (up (file-name-directory (directory-file-name dir))))
      (or (dired-goto-file (directory-file-name dir))
          ;; Only try dired-goto-subdir if buffer has more than one dir.
          (and (cdr dired-subdir-alist)
               (dired-goto-subdir up))
          (progn
            (if other-window
                (dired-other-window up)
              (progn
                (kill-buffer (current-buffer))
                (dired up))
              (dired-goto-file dir))))))

  ;; dired-modeがlsコマンドに渡すオプションを設定する
  ;; l: 長い表示、dired-modeに必須のオプション
  ;; g: ユーザ名を非表示
  ;; G: グループ名を非表示
  ;; h: kbyte・Mbyteの使用
  ;; F: ディレクトリに「/」を表示
  ;; A: 「.」と「..」を非表示でドットファイルを表示
  ;;(setq dired-listing-switches "-gGhFA")
  (setq dired-listing-switches "-lgGhF")

  ;; C-.でドットファイルの表示と非表示を切り替える
  (defun reload-current-dired-buffer ()
    "Reload current `dired-mode' buffer."
    (let* ((dir (dired-current-directory)))
      (progn (kill-buffer (current-buffer))
             (dired dir))))
  (defun toggle-dired-listing-switches ()
    "Toggle `dired-mode' switch between with and without 'A' option to show or hide dot files."
    (interactive)
    (progn
      (if (string-match "[Aa]" dired-listing-switches)
          (setq dired-listing-switches "-lgGhF")
        (setq dired-listing-switches "-lgGhFA"))
      (reload-current-dired-buffer))
    )

  :bind (:map dired-mode-map
              ("s" . dired-various-sort-change-or-edit)
              ("C-m" . dired-my-advertised-find-file)
              ("^" . dired-my-up-directory)
              ("C-." . toggle-dired-listing-switches)
              ))

;;; init_package2.el
