;;; init_package --- settings about packages
;;; Commentary:
;;; Code:

(package-install 'use-package)

;; use-packageの:diminishを有効にし、モードラインをスッキリさせる
(use-package diminish :ensure t)

;; clangがあるとより便利らしいので、aptでclangをいれておく
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (global-company-mode 1)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
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
  :after company
  :init
  (company-quickhelp-mode t)
  (setq company-quickhelp-delay 1)
  (setq company-transformers '(company-sort-by-backend-importance))
  )

(use-package company-irony-c-headers
  :ensure t
  :after (company irony)
  )

(use-package company-irony
  :ensure t
  :after (company irony)
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
  )

(use-package flycheck-irony
  :ensure t
  :after (flycheck irony)
  )

;; aptかpipでflake8を入れておく
;; どちらを使うかを選択しないといけない、闇
;; aptでflake8をいれておく
(use-package flycheck-pyflakes
  :ensure t
  :if (= 0 (shell-command "flake8 --version 1>/dev/null 2>/dev/null"))
  :init
  ;; (require 'flycheck-pyflakes)
  )

(use-package flycheck-ocaml
  :ensure t
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
;; (use-package company-arduino arduino-mode
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
;; (use-package company-jedi
;;   :ensure t
;;   )

;; markdownコマンドをいれておく
(use-package markdown-mode
  :ensure t
  :if (= 0 (shell-command "markdown --version 1>/dev/null 2>/dev/null"))
  :init
  (setq markdown-command "markdown")
  ;; style sheetは生成HTMLと同フォルダにあるstyle.cssにする
  (setq markdown-css-paths '("style.css"))
  ;; ファイルロック機能と競合してハングするため、leoさんの松葉杖対処を導入
  ;; https://groups.google.com/forum/#!topic/gnu.emacs.help/AIy5megeSHA
  (defun leo-markdown-fontify-buffer-wiki-links-empty ()
    "Empty replacement for `markdown-fontify-buffer-wiki-links` due to hanging bug."
    (interactive))
  (eval-after-load "markdown-mode"
    '(progn (fset 'markdown-fontify-buffer-wiki-links
                  'leo-markdown-fontify-buffer-wiki-links-empty)))
  )

;; markdownでコードブロックの編集のために必要
(use-package edit-indirect
  :ensure t
  )

;; (use-package csv-mode
;;   :ensure t
;;   )

(use-package sml-mode
  :ensure t
  :mode ("\\.smi\\'" "\\.ppg\\'")
  :interpreter "smlsharp"
  :init
  (setq sml-indent-level 2)
  (setq sml-indent-args 2)
  ;; sml-modeのrun-smlでデフォルトSMLコマンドをsmlsharpにする
  (setq syml-program-name "smlsharp")
  :config
  )

;; (use-package twittering-mode
;;   :ensure t
;;   )
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
  :init
  (setq rainbow-r-colors t) ; R color listを使う
  (setq rainbow-html-colors t) ; html color listを使う
  )

;; (use-package w3m
;;   :ensure t
;;   )

(use-package smartparens
  :ensure t
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
  :init
  (setq ivy-count-format "(%d/%d) ")
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
  :init
  (setq swiper-include-line-number-in-search t)
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
  ;; Emacsを起動したshellを使用する（bashからの起動を前提）
  (setq explicit-shell-file-name (getenv "SHELL"))
  (setq explicit-bash-args '("--login" "-i"))
  ;; SHELL で ^M が付く場合は ^M を削除する
  (add-hook 'shell-mode-hook
            (lambda ()
              (set-buffer-process-coding-system 'undecided-dos 'sjis-unix)))
  ;; shell-modeでのファイル名補完
  (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")
  (bash-completion-setup)
)

;;; init_package2.el
