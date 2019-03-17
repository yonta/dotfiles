;;; init_package --- settings about packages
;;; Commentary:
;;; Code:

(package-install 'use-package)

; clangがあるとより便利
(use-package company
  :ensure t
  :if (locate-library "company")
  :init
  (global-company-mode 1)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  :bind (("C-M-i" . company-complete))
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("<tab>" . company-complete-common-or-cycle)
              ("<backtab>" . company-select-previous)
              ("C-f" . company-complete-selection)
              ("C-d" . company-show-doc-buffer)
              ("C-s" . company-filter-candidates)
              ("C-h" . backward-delete-char)
              ))

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

;; pipでflake8を入れておく
(use-package flycheck-pyflakes
  :ensure t
  :init
  ; (require 'flycheck-pyflakes)
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
;; (use-package markdown-mode
;;   :ensure t
;;   )
;; (use-package edit-indirect
;;   :ensure t
;;   )
;; (use-package csv-mode
;;   :ensure t
;;   )

(use-package sml-mode
  :ensure t
  :mode "\\.smi\\'"
  :mode "\\.ppg\\'"
  :init
  (setq sml-indent-level 2)
  (setq sml-indent-args 2)
  ;; sml-modeのrun-smlでデフォルトSMLコマンドをsmlsharpにする
  (setq sml-program-name "smlsharp")
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
;; (use-package rainbow-mode
;;   :ensure t
;;   )
;; (use-package w3m
;;   :ensure t
;;   )
;; (use-package smartparens
;;   :ensure t
;;   )
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

;;; init_package2.el
