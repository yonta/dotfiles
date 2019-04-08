;;; init_package --- settings about packages

;;; Commentary:
;; This is settings about package.
;; It is written using `use-package` package.

;;; Code:

(unless (package-installed-p 'use-package) (package-install 'use-package))

;; use-packageの:diminishを有効にし、モードラインをスッキリさせる
(use-package diminish :ensure t :defer t)

;; 起動時間を計測するには、以下を有効にして`use-package-report`を実行する
;; (setq use-package-compute-statistics t)

;; clangがあるとより便利らしいので、aptでclangをいれておく
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq completion-ignore-case t)
  (global-company-mode 1)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-require-match 'never)
  (company-dabbrev-ignore-case t)
  (company-dabbrev-code-ignore-case t)
  (company-etags-ignore-case t)
  (company-transformers '(company-sort-by-occurrence))
  :config
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  (setq company-tooltip-align-annotations t)
  :bind (("C-M-i" . company-complete)
         (:map company-active-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("<tab>" . company-complete-common-or-cycle)
               ("<backtab>" . company-select-previous)
               ("C-f" . company-complete-selection)
               ("C-d" . company-show-doc-buffer)
               ("C-s" . company-filter-candidates)
               ("C-o" . company-other-backend))))

(use-package company-quickhelp
  :ensure t
  :after company
  :init
  (company-quickhelp-mode 1)
  :custom
  (company-quickhelp-delay 1))

(use-package company-c-headers
  :ensure t
  :defer t
  :after company)

(use-package company-arduino
  :ensure t
  :defer t
  :after company)

;; aptかpipでjediを入れておく
;; aptでpython3-jediをいれておき、初回起動時にjedi:install-serverする
(use-package company-jedi
  :ensure t
  :defer t
  :after (company jedi-core))

;; aptかpipでflake8を入れておく
;; どちらを使うかを選択しないといけない、闇
;; aptでflake8をいれておく
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-python-flake8-executable "flake8")
  :config
  (set-face-background 'flycheck-error "pink") ; エラー箇所は背景色をつける
  :bind (:map flycheck-mode-map
              ("M-p" . flycheck-previous-error)
              ("M-n" . flycheck-next-error)))

(use-package flycheck-ocaml
  :ensure t
  :defer t)

(use-package cc-mode
  :defer t
  :after (company-clang company-c-headers)
  :init
  (defun my-c-mode-hook ()
    "Setting for c-mode."
    (c-set-style "k&r")
    (setq c-basic-offset 2)
    (setq tab-width c-basic-offset)
    (setq indent-tabs-mode nil)
    (set (make-local-variable 'company-backends)
         '((company-clang
            :with company-c-headers company-files company-dabbrev-code
            company-yasnippet))))
  (add-hook 'c-mode-hook #'my-c-mode-hook)
  (defun my-c++-mode-hook ()
    "Setting for c++-mode."
    (c-set-style "k&r")
    (setq c-basic-offset 2)
    (setq tab-width c-basic-offset)
    (setq indent-tabs-mode nil)
    (setq flycheck-gcc-language-standard "c++11")
    (setq flycheck-clang-language-standard "c++11")
    (set (make-local-variable 'company-backends)
         '((company-clang
            :with company-c-headers company-files company-dabbrev-code
            company-yasnippet))))
  (add-hook 'c++-mode-hook #'my-c++-mode-hook))

(use-package tuareg
  :ensure t
  :defer t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :after company
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'company-backends #'company-yasnippet t))

;; elpa/yasnippet-snippets-20190316.1019/snippets/ に以下をcloneしておく
;; https://github.com/jasperla/yasnippet-sml-mode
;; 使いづらいので、今後自作したい
(use-package yasnippet-snippets
  :ensure t)

;; TODO: set compiler and libraries path by environment
(use-package arduino-mode
  :ensure t
  :defer t)

(use-package python-mode
  :ensure t
  :defer t
  :custom
  (indent-tabs-mode nil)
  (indent-level 4)
  (python-indent 4)
  (tab-width 4)
  :config
  ;; local-variableだとうまくいかなかった
  (add-to-list 'company-backends
               '(company-jedi
                 :with company-files company-dabbrev-code company-yasnippet))
  (define-key python-mode-map (kbd "C-c C-c") nil)
  :bind (:map python-mode-map
              ("C-c c" . py-execute-buffer)
              ("<tab>" . indent-for-tab-command)))

;; aptかpipでautopep8を入れておく
;; aptでautopep8をいれておく
(use-package py-autopep8
  :ensure t
  :if (executable-find "autopep8")
  :hook (python-mode . py-autopep8-enable-on-save))

(use-package quickrun
  :ensure t
  :custom
  ;; タイムアウトで処理を中止させない
  (quickrun-timeout-seconds -1)
  :bind (("C-c C-c" . quickrun)))

;; markdownコマンドをいれておく
(use-package markdown-mode
  :ensure t
  :defer t
  :if (executable-find "markdown")
  :mode ("README\\.md\\'" . gfm-mode)
  :config
  ;; ファイルロック機能と競合してハングするため、leoさんの松葉杖対処を導入
  ;; https://groups.google.com/forum/#!topic/gnu.emacs.help/AIy5megeSHA
  (defun leo-markdown-fontify-buffer-wiki-links-empty ()
    "Empty replacement for `markdown-fontify-buffer-wiki-links` due to hanging bug."
    (interactive))
  (fset #'markdown-fontify-buffer-wiki-links
        #'leo-markdown-fontify-buffer-wiki-links-empty)
  :custom
  (markdown-command "markdown")
  ;; style sheetは生成HTMLと同フォルダにあるstyle.cssにする
  (markdown-css-paths '("style.css")))

;; markdownでコードブロックの編集のために必要
(use-package edit-indirect
  :ensure t
  :defer t)

(use-package csv-mode
  :ensure t
  :defer t)

(use-package sml-mode
  :ensure t
  :mode ("\\.smi\\'" "\\.ppg\\'")
  :after company-mlton
  :interpreter "smlsharp"
  :custom
  (sml-indent-level 2)
  (sml-indent-args 2)
  ;; sml-modeのrun-smlでデフォルトSMLコマンドをsmlsharpにする
  (sml-program-name "smlsharp")
  :config
  (add-hook 'sml-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-mlton-keyword
                      company-mlton-basis
                      :with company-files company-dabbrev-code
                      company-yasnippet))))))

(use-package company-mlton
  :config
  (company-mlton-basis-autodetect))

;; aptでgnupgを入れておく
;; alpaca.elが必要
(use-package twittering-mode
  :ensure t
  :commands twit
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
   '("keita44_f4/friend" ":replies" ":home"))
  ;;(twittering-retweet-format " RT @%s %t")
  (twittering-retweet-format " %u")
  (twittering-fill-column 80)
  (twittering-edit-skeleton 'inherit-mentions)
  :config
  (twittering-icon-mode t) ; use icon
  ;; URLを青文字にする
  (add-hook 'twittering-mode-hook
            (lambda () (set-face-foreground 'twittering-uri-face "blue")))
  :bind (:map twittering-mode-map
              ("R" . twittering-native-retweet)
              ("r" . twittering-enter)
              ("T" . twittering-organic-retweet)
              ("t" . twittering-update-status-interactive)
              ("o" . twittering-goto-next-uri)
              ("O" . twittering-push-uri-onto-kill-ring)
              ("J" . end-of-buffer)
              ("K" . twittering-goto-first-status)
              ("u" . twittering-toggle-show-replied-statuses)))

(use-package gnuplot-mode
  :ensure t
  ;; .gpl .plt、.gp .gnuplotはautoloadで登録済み
  :mode ("\\.gpl\\'" "\\.plt\\'"))

(use-package graphviz-dot-mode
  :ensure t
  :defer t)

(use-package google-translate
  :ensure t
  :commands (google-translate google-translate-smooth-ui)
  :bind (("C-c C-t" . google-translate-smooth-translate)
         ("C-c t" . google-translate-query-translate)))

(use-package google-translate-smooth-ui
  :requires google-translate
  :config
  (setq google-translate-translation-directions-alist
        '(("en" . "ja") ("ja" . "en"))))

(use-package bash-completion
  :ensure t
  :commands shell)

(use-package haxe-mode
  :ensure t
  :defer t
  :custom
  (tab-width 4)
  (indent-tabs-mode nil)
  (fill-column 80))

(use-package proof-general
  :ensure t
  :mode "\\.v\\'")

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  ;; popwin対象
  (setq popwin:special-display-config
        '(("*quickrun*" :stick t)
          ("*Google Translate*")
          (completion-list-mode :noselect t) ;; 全completionを対象
          ("*Warnings*")
          (" *auto-async-byte-compile*")
          ("*Kill Ring*"))))

;; ImageMagickをaptでいれておく
;; 非同期でimage-diredを動作させ、大量画像でフリーズしないようにするパッケージ
;; BUG: ディレクトリを開く初回時にサムネイル作成に失敗する。
;;      diredバッファでimage-dired-create-thumbsを実行して手動でサムネイル
;;      を作ると、image-diredが問題なく動くようになる。
;;      --no-initを使って、image-dired+だけで動かすと問題は起こらない。
;;      何らかの自分のinitファイルが問題を引き起こしている。
;;      Error-log
;;      image-diredx--invoke-process: Wrong type argument: processp, [nil 23723 12045 294055 nil image-dired-thumb-queue-run nil nil 600000]
(use-package image-dired+
  :ensure t
  :if (executable-find "convert")
  :commands image-dired
  :config
  ;; Emacs26からは非同期なimage-diredがあり、コンフリクトするのでオンしない
  (if (version< emacs-version "26") ; Emacs25以下
      (progn (image-diredx-async-mode 1)
             (image-diredx-adjust-mode 1)))
  ;; lrでサムネイルが回転するのを削除
  (define-key image-map (kbd "r") nil)
  (define-key image-dired-thumbnail-mode-map (kbd "r") nil)
  :bind (:map image-dired-thumbnail-mode-map
              ("C-n" . image-diredx-next-line)
              ("C-p" . image-diredx-previous-line)
              ("<down>" . image-diredx-next-line)
              ("<up>" . image-diredx-previous-line)
              ("j" . image-diredx-next-line)
              ("k" . image-diredx-previous-line)
              ("h" . image-dired-backward-image)
              ("l" . image-dired-forward-image)
              ("g" . revert-buffer) ; 更新
         :map image-dired-display-image-mode-map
              ("f" . image-dired-display-current-image-full)
              ("0" . image-dired-display-current-image-sized)))

;; アクティブかどうかでバッファーのモードラインの色を変える
(use-package hiwin
  :ensure t
  :init
  ;; (set-face-background 'hiwin-face "gray92")
  (set-face-attribute 'mode-line nil :background "light sky blue")
  (set-face-attribute 'mode-line-inactive nil
                      :background "light gray"
                      :foreground "dim gray"))

;; "#ff0000"などに色をつける
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook (c++-mode arduino-mode)
  :custom
  (rainbow-r-colors t)    ; R color listを使う
  (rainbow-html-colors t)) ; html color listを使う

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (smartparens-global-mode t)
  ;; 一部のモードでは'での補完を行わない
  (sp-local-pair '(emacs-lisp-mode) "'" nil :actions nil)
  (sp-local-pair '(lisp-mode) "'" nil :actions nil)
  (sp-local-pair '(lisp-mode) "`" nil :actions nil)
  (sp-local-pair '(sml-mode) "'" nil :actions nil)
  (sp-local-pair '(inferior-sml-mode) "'" nil :actions nil)
  (sp-local-pair '(tuareg-mode) "'" nil :actions nil))

(use-package browse-kill-ring
  :ensure t
  :bind (("C-M-y" . browse-kill-ring)))

;; cmigemoをいれておく
;; https://github.com/koron/cmigemo
(use-package migemo
  :ensure t
  :if (executable-find "cmigemo")
  :custom
  (migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-init))

(use-package avy-migemo
  :ensure t
  :after migemo
  :init
  (avy-migemo-mode 1)
  (require 'avy-migemo-e.g.swiper)
  (require 'avy-migemo-e.g.counsel))

(use-package ivy
  :ensure t
  :after ivy-rich
  :custom
  (ivy-count-format "(%d/%d) ")
  :bind ("C-x b" . ivy-switch-buffer))

(use-package counsel
  :ensure t
  :after ivy swiper avy-migemo ivy-rich
  ;; dotファイルとコンパイルファイルなどを無視する
  ;; .キーを押せばdotスタートファイルは表示される
  :custom (counsel-find-file-ignore-regexp
           (concat "\\(\\`\\.\\)\\|"
                   (regexp-opt completion-ignored-extensions)))
  :config
  :bind (("M-x" . counsel-M-x)
         ("M-r" . counsel-command-history)
         ("C-x C-f" . counsel-find-file)
         ("C-c C-d" . counsel-describe-function)
         ("C-c C-g" . counsel-git-grep)
         (:map counsel-find-file-map
               ("^" . counsel-up-directory))))

(use-package swiper
  :ensure t
  :after ivy avy-migemo migemo
  :custom
  (swiper-include-line-number-in-search t)
  :bind (("C-s" . swiper)
         ("C-c s" . isearch-forward)
         (:map swiper-map
               ("M-%" . swiper-query-replace)
               ("C-w" . ivy-yank-word)
               ("C-M-y" . ivy-yank-char))))

;; バグで動かない
;; https://github.com/jschaf/esup/issues/54
(use-package esup :disabled)

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-delete-old-versions t)
  ;; (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package auto-async-byte-compile
  :ensure t
  ;; :custom
  ;; (auto-async-byte-compile-exclude-files-regexp "init*")
  :hook (emacs-lisp-mode . enable-auto-async-byte-compile-mode))

(use-package ivy-rich
  :ensure t
  :defer t
  :init
  (ivy-rich-mode 1)
  :custom
  (ivy-format-function #'ivy-format-function-line))

(use-package git-gutter-fringe+
  :ensure t
  :diminish git-gutter+-mode
  :init
  (global-git-gutter+-mode 1)
  :config
  (set-face-foreground 'git-gutter+-added "lime green")
  (set-face-foreground 'git-gutter+-modified "blue")
  (fringe-helper-define 'git-gutter-fr+-modified nil
  "X......."
  "XXXX...."
  "XXXXXX.."
  "XXXXXXXX"
  "XXXXXXXX"
  "XXXXXX.."
  "XXXX...."
  "XX......")
  (fringe-helper-define 'git-gutter-fr+-deleted nil
  "........"
  "........"
  "........"
  "XXXXXXXX"
  "XXXXXXXX"
  "XXXXXXXX"
  "........"
  "........"))

(use-package dumb-jump
  :ensure t
  :after ivy
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt))
  :config
  (setq dumb-jump-selector 'ivy))

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  (set-face-background 'vhl/default-face "light cyan"))

(use-package expand-region
  :ensure t
  :bind (("C-`" . er/expand-region)))

(use-package async
  :ensure t
  :no-require
  :custom
  (async-bytecomp-allowed-packages '(all))
  :config
  (async-bytecomp-package-mode 1))

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
                         'undecided-dos 'sjis-unix))))

(use-package dired
  :custom
  ;; dired-modeがlsコマンドに渡すオプションを設定する
  ;; l: 長い表示、dired-modeに必須のオプション
  ;; g: ユーザ名を非表示
  ;; G: グループ名を非表示
  ;; h: kbyte・Mbyteの使用
  ;; F: ディレクトリに「/」を表示
  ;; A: 「.」と「..」を非表示でドットファイルを表示
  ;;(setq dired-listing-switches "-gGhFA")
  (dired-listing-switches "-lgGhF")
  :config
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
      (funcall #'dired-find-file)
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
      (reload-current-dired-buffer)))
  :bind (:map dired-mode-map
              ("s" . dired-various-sort-change-or-edit)
              ("C-m" . dired-my-advertised-find-file)
              ("^" . dired-my-up-directory)
              ("C-." . toggle-dired-listing-switches)
              ("r" . wdired-change-to-wdired-mode)))

(use-package help-mode
  ;; Alt+左右でヘルプの進む・戻るを行う、デフォルトはl/r
  :bind (:map help-mode-map
              ("M-<left>" . help-go-back)
              ("M-<right>". help-go-forward)))

;; コマンドラインと同じ色付けを使う
(use-package ansi-color
  :commands ansi-color-for-comint-mode-on
  :hook (shell-mode . ansi-color-for-comint-mode-on))

(use-package ibuffer
  :config
  ;; キロメガでサイズ表示する
  ;; 参考： https://www.emacswiki.org/emacs/IbufferMode
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only locked " " (name 18 18 :left :elide) " "
                (size-h 9 -1 :right) " " (mode 16 16 :left :elide) " "
                filename-and-process)))
  ;; ibuffer-find-fileを使わずにcounselを使う
  (defun ibuffer-find-file-by-counsel ()
    "Like `counsel-find-file', but default to the directory of the buffer
at point."
    (interactive)
    (let ((default-directory
            (let ((buf (ibuffer-current-buffer)))
              (if (buffer-live-p buf)
                  (with-current-buffer buf
                    default-directory)
                default-directory))))
      (counsel-find-file default-directory)))
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map ("C-x C-f" . 'ibuffer-find-file-by-counsel)))

(use-package whitespace
  :diminish global-whitespace-mode
  :custom
  ;; 空白などの可視化、対象はタブ文字、80文字超え部、行末の空白、全角スペース
  (whitespace-style '(face tabs lines-tail trailing spaces empty))
  ;; 保存前に自動でクリーンアップ、対象はwhitespace-styleでセットしたもの
  (whitespace-action '(auto-cleanup))
  ;; spacesの対象は全角スペースのみ
  (whitespace-space-regexp "\\(　+\\)")
  :config
  ;; white spaceをオン
  (global-whitespace-mode t)
  ;; 行末スペースの色
  (set-face-attribute 'whitespace-trailing nil :background "Lavender")
  ;; 全角スペースの色
  (set-face-attribute 'whitespace-space nil :background "DarkSeaGreen1")
  ;; タブの色
  (set-face-attribute 'whitespace-tab nil :background "LightGoldenrodYellow")
  ;; 空行の色
  (set-face-attribute 'whitespace-empty nil :background nil)
  ;; 80文字オーバーの色
  (set-face-attribute 'whitespace-line nil :foreground nil
                                           :background "khaki")
  ;; java-modeではカラムオーバーの限界をデフォルトの80から100に変更する
  (defun set-whitespace-line-column-80 () (setq whitespace-line-column 80))
  (defun set-whitespace-line-column-100 () (setq whitespace-line-column 100))
  (add-hook 'java-mode-hook #'set-whitespace-line-column-100)
  (add-hook 'change-major-mode-hook #'set-whitespace-line-column-80))

(use-package elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-capf
                      :with company-files company-dabbrev-code
                      company-yasnippet))))))

(use-package winner
  :init
  (winner-mode 1)
  :bind (("C-M-<left>" . winner-undo)
         ("C-M-<right>" . winner-redo)))

;; hl-line+.elを手に入れてlispフォルダにいれておく
;; https://www.emacswiki.org/emacs/download/hl-line%2b.el
(use-package hl-line+
  :config
  (toggle-hl-line-when-idle 1)
  (hl-line-when-idle-interval 4)
  (set-face-background hl-line-face "light cyan"))

(use-package display-line-numbers
  :if (version<= "26" emacs-version) ; Emacs26以降
  :init
  (global-display-line-numbers-mode 1))

(use-package linum
  :if (version< emacs-version "26") ; Emacs25以下
  :init
  (global-linum-mode 1)
  (setq linum-format "%4d "))

(use-package autoinsert
  :init
  (auto-insert-mode 1)
  (defvar template-replacements-alists
    '(("%file%" . (lambda () (file-name-nondirectory (buffer-file-name))))
      ("%without-test%" .
       (lambda ()
         ((lambda (arg)(replace-regexp-in-string "Test$" "" arg))
          (file-name-sans-extension
           (file-name-nondirectory (buffer-file-name))))))
      ("%file-without-ext%" .
       (lambda ()
         (file-name-sans-extension
          (file-name-nondirectory (buffer-file-name)))))
      ("%include-guard%" .
       (lambda ()
         (format "%s_H"
                 (upcase (file-name-sans-extension
                          (file-name-nondirectory buffer-file-name))))))))
  (defun my-template ()
    "Add template string to file."
    (mapc #'(lambda(c) (progn (goto-char (point-min))
                              (replace-string (car c) (funcall (cdr c)) nil)))
          template-replacements-alists)
    (goto-char (point-min))
    (message "done."))
  (setq auto-insert-directory "~/.emacs.d/autoinsert/")
  (setq auto-insert-alist
        (nconc '(("Test\\.\\(cpp\\|cc\\|cxx\\)$" .
                  ["templateTest.cpp" my-template])
                 ("\\.\\(cpp\\|cc\\|cxx\\)$" . ["template.cpp" my-template])
                 ("\\.\\(hpp\\|hh\\|hxx\\)$"   . ["template.hpp" my-template])
                 ("\\.c$"   . ["template.c" my-template])
                 ("\\.ino$" . ["template.ino" my-template])
                 ("\\.py$" . ["template.py" my-template]))
               auto-insert-alist)))

;;; init_package.el ends here
