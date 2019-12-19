;;; init_package --- settings about packages -*- lexical-binding: t; -*-

;;; Commentary:
;; This is settings about package.
;; It is written using `leaf.el`.

;;; Code:

(eval-and-compile (package-initialize))

(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

(eval-and-compile
  (leaf leaf-keywords :ensure t
    :config
    (leaf el-get :ensure t
      :custom ((el-get-git-shallow-clone . t)))
    ;; :diminishを有効にし、モードラインをスッキリさせる
    (leaf diminish :ensure t)
    (leaf-keywords-init)))

(leaf bind-key :ensure t :require t)

(leaf util :defun call-with-region-or-line) ; dummy, init_util.el

;; clangがあるとより便利らしいので、aptでclangをいれておく
(leaf company :ensure t
  :diminish company-mode
  :defvar (company-mode-map company-tooltip-align-annotations company-backends)
  :custom
  (company-idle-delay . 0)
  (company-minimum-prefix-length . 2)
  (company-selection-wrap-around . t)
  (company-require-match . 'never)
  (company-dabbrev-ignore-case . t)
  (company-dabbrev-code-ignore-case . t)
  (company-etags-ignore-case . t)
  (company-transformers . '(company-sort-by-occurrence))
  :config
  (setq completion-ignore-case t)
  (global-company-mode 1)
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  (setq company-tooltip-align-annotations t)
  :bind (("C-M-i" . company-complete)
         (:company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-common-or-cycle)
          ("<backtab>" . company-select-previous)
          ("C-f" . company-complete-selection)
          ("C-d" . company-show-doc-buffer)
          ("C-s" . company-filter-candidates)
          ("C-o" . company-other-backend)))

  :config
  (leaf company-quickhelp :ensure t
    :config
    (company-quickhelp-mode 1)
    :custom
    (company-quickhelp-delay . 1))

  (leaf company-prescient :ensure t
    :config
    (company-prescient-mode 1))

  (leaf company-c-headers :ensure t)

  (leaf company-arduino :ensure t)

  ;; aptかpipでvirtualenvを入れておく
  ;; aptでvirtualenvをいれておき、
  ;; 初回起動時にjedi:install-serverする
  (leaf company-jedi :ensure t
    :init
    (leaf jedi-core :ensure t
      :hook (python-mode-hook . jedi:setup)
      ;; 関数の引数の情報が便利なので、ミニバッファに表示する
      :custom ((jedi:tooltip-method . nil)
               (jedi:use-shortcuts . t)))) ; M-,/M-.にjediを使う

  (leaf yasnippet :ensure t
    :diminish yas-minor-mode
    :defvar yas-minor-mode-map
    :init
    (leaf yasnippet-snippets :ensure t)
    :config
    (yas-global-mode 1)
    ;; yas-expandは使わず、companyからyasを使う。
    (unbind-key "<tab>" yas-minor-mode-map)
    (unbind-key "TAB" yas-minor-mode-map)
    (add-to-list 'company-backends #'company-yasnippet)))

;; aptかpipでflake8を入れておく
;; どちらを使うかを選択しないといけない、闇
;; aptでflake8をいれておく
(leaf flycheck :ensure t
  :diminish flycheck-mode
  :defvar (flycheck-gcc-language-standard flycheck-clang-language-standard)
  :custom
  (flycheck-python-flake8-executable . "flake8")
  (flycheck-checker-error-threshold . 250)
  :config
  (global-flycheck-mode)
  (set-face-background 'flycheck-error "pink") ; エラー箇所は背景色をつける
  :bind (:flycheck-mode-map
         ("M-p" . flycheck-previous-error)
         ("M-n" . flycheck-next-error))

  :config
  ;; aptかpipでmypyを入れておく
  ;; aptでmypyをいれておく
  (leaf flycheck-mypy :ensure t
    ;; 「変数の再定義が禁止」など、pepに従ったflake8よりエラーが厳しい
    ;; 必要なときにだけflycheck-select-checkerで利用する
    ;; :hook (python-mode-hook . (lambda ()
    ;;                             (setq-local flycheck-checker 'python-mypy))))
    )

  (leaf flycheck-popup-tip :ensure t
    :hook (flycheck-mode-hook . flycheck-popup-tip-mode))

  (leaf flycheck-ocaml :ensure t))

(leaf cc-mode
  :defvar c-basic-offset
  :init

  ;; "#ff0000"などに色をつける
  (leaf rainbow-mode :ensure t
    :diminish t
    :custom
    (rainbow-r-colors . t)                ; R color listを使う
    (rainbow-html-colors . t)             ; html color listを使う
    :hook (c++-mode-hook arduino-mode-hook))

  (defun my-c-mode-hook ()
    "Setting for c-mode."
    (c-set-style "k&r")
    (setq c-basic-offset 2)
    (setq tab-width c-basic-offset)
    (setq indent-tabs-mode nil)
    (setq-local company-backends
                '((company-clang
                   :with company-c-headers company-files company-dabbrev-code
                   company-yasnippet))))
  (defun my-c++-mode-hook ()
    "Setting for c++-mode."
    (c-set-style "k&r")
    (setq c-basic-offset 2)
    (setq tab-width c-basic-offset)
    (setq indent-tabs-mode nil)
    (setq flycheck-gcc-language-standard "c++11")
    (setq flycheck-clang-language-standard "c++11")
    (setq-local company-backends
                '((company-clang
                   :with company-c-headers company-files company-dabbrev-code
                   company-yasnippet))))
  :hook ((c-mode-hook . my-c-mode-hook)
         (c++-mode-hook . my-c++-mode-hook)))

(leaf tuareg :ensure t)

;; TODO: set compiler and libraries path by environment
(leaf arduino-mode :ensure t)

(leaf quickrun :ensure t
  :custom
  (quickrun-timeout-seconds . -1)       ; タイムアウトで処理を中止させない
  :config
  ;; python-modeでpython3を使う
  (quickrun-add-command "python"
    '((:command . "python3")
      (:exec . "%c %s")
      (:compile-only . "pyflakes %s"))
    :mode 'python-mode)
  :bind ("C-c c" . quickrun))

;; markdownコマンドをいれておく
(leaf markdown-mode :ensure t
  :if (executable-find "markdown")
  :defun (markdown-fontify-buffer-wiki-links
          leo-markdown-fontify-buffer-wiki-links-empty)
  :mode ("README\\.md\\'" . gfm-mode)

  :init
  ;; markdownでコードブロックの編集のために必要
  (leaf edit-indirect :ensure t)

  :config
  ;; ファイルロック機能と競合してハングするため、leoさんの松葉杖対処を導入
  ;; https://groups.google.com/forum/#!topic/gnu.emacs.help/AIy5megeSHA
  (defun leo-markdown-fontify-buffer-wiki-links-empty ()
    "Empty replacement for `markdown-fontify-buffer-wiki-links` due to hanging bug."
    (interactive))
  (fset #'markdown-fontify-buffer-wiki-links
        #'leo-markdown-fontify-buffer-wiki-links-empty)
  :custom
  (markdown-command . "markdown")
  ;; style sheetは生成HTMLと同フォルダにあるstyle.cssにする
  (markdown-css-paths . '("style.css")))

(leaf csv-mode :ensure t)

(leaf sml-mode
  :el-get (sml-mode
           :url "https://github.com/yonta/sml-mode.git"
           :branch "add-smlsharp")
  :mode ("\\.smi\\'" "\\.ppg\\'")
  :interpreter "smlsharp"
  :defun sml-prog-proc-send-region

  :init
  (leaf company-mlton
    :el-get (company-mlton
             :url "https://github.com/yonta/company-mlton.git"
             :branch "add-smlsharp")
    :defun company-mlton-basis-autodetect
    :config
    (company-mlton-basis-autodetect))

  (leaf flycheck-smlsharp
    :el-get (flycheck-smlsharp
             :url "https://github.com/yonta/flycheck-smlsharp.git")
    :hook (sml-mode-hook
           . (lambda () (require 'flycheck-smlsharp))))

  :init
  (defun sml-prog-proc-send-region-or-line ()
    "Call REPL with active region or current line."
    (interactive) (call-with-region-or-line #'sml-prog-proc-send-region))
  :custom
  (sml-indent-level . 2)
  (sml-indent-args . 2)
  ;; sml-modeのrun-smlでデフォルトSMLコマンドをsmlsharpにする
  (sml-program-name . "smlsharp")
  :hook (sml-mode-hook
         . (lambda ()
             (setq-local company-backends
                         '((company-mlton-keyword
                            company-mlton-basis
                            :with company-files company-dabbrev-code
                            company-yasnippet)))))
  :bind (:sml-mode-map
         ("C-c C-r" . sml-prog-proc-send-region-or-line)
         ("C-c C-p" . sml-run)))

;; aptでgnupgを入れておく
;; alpaca.elが必要
(leaf twittering-mode :ensure t
  :defun twittering-icon-mode
  :commands twit
  :init
  (defvar my-twittering-status-format
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
  :custom
  ;; use master passworad compressed by GnuPG
  (twittering-use-master-password . t)
  (twittering-private-info-file . "~/.emacs.d/twittering-mode.gpg")
  (twittering-use-icon-storage . t)
  (twittering-icon-storage-file . "~/.emacs.d/icons.gz")
  (twittering-status-format . my-twittering-status-format)
  ;;(twittering-convert-fix-size . 24)
  (twittering-timer-interval . 600)
  (twittering-number-of-tweets-on-retrieval . 100)
  (twittering-display-remaining . t)
  (twittering-initial-timeline-spec-string
   . '("keita44_f4/friend" ":replies" ":home"))
  ;;(twittering-retweet-format . " RT @%s %t")
  (twittering-retweet-format . " %u")
  (twittering-fill-column . 80)
  (twittering-edit-skeleton . 'inherit-mentions)
  :custom-face
  ;; URLを青文字にする
  (twittering-uri-face . '((t (:foreground "blue" :underline t))))
  :config
  (twittering-icon-mode t) ; use icon
  :bind (:twittering-mode-map
         ("R" . twittering-native-retweet)
         ("r" . twittering-enter)
         ("T" . twittering-organic-retweet)
         ("t" . twittering-update-status-interactive)
         ("o" . twittering-goto-next-uri)
         ("O" . twittering-push-uri-onto-kill-ring)
         ("J" . end-of-buffer)
         ("K" . twittering-goto-first-status)
         ("u" . twittering-toggle-show-replied-statuses)))

(leaf gnuplot-mode :ensure t
  ;; .gpl .plt、.gp .gnuplotはautoloadで登録済み
  :mode ("\\.gpl\\'" "\\.plt\\'"))

(leaf graphviz-dot-mode :ensure t)

(leaf google-translate-smooth-ui
  :defvar google-translate-translation-directions-alist
  :init
  (leaf google-translate :ensure t
    :defun google-translate-translate)

  (defun google-translate-smooth-translate-region (&optional text)
    "Translate a text in selected region using translation directions.

If optional argument TEXT is non-nil or region is active, translate text
immediately. Otherwise, call `google-translate-smooth-translate`.

A different between `google-translate-smooth-translate`, this function
translates a text directly without enter key press. And, this function
changes source and target language automaticaly."
    (interactive)
    (if (not (or (stringp text) (use-region-p))) ; if both empty
        (google-translate-smooth-translate)
      (let* ((text
              (cond ((stringp text) text)
                    ((use-region-p) (buffer-substring (region-beginning)
                                                      (region-end)))))
             (from-english (string-match "\\`[[:ascii:]‘’“”]+\\'" text))
             (source (if from-english "en" "ja"))
             (target (if from-english "ja" "en")))
        (google-translate-translate source target text))))
  :config
  (setq google-translate-translation-directions-alist
        '(("en" . "ja") ("ja" . "en")))
  :bind (("C-c C-t" . google-translate-smooth-translate-region)
         ("C-c t" . google-translate-smooth-translate)))

(leaf shell
  :init

  (leaf bash-completion :ensure t
    :commands shell)

  ;; コマンドラインと同じ色付けを使う
  (leaf ansi-color
    :commands ansi-color-for-comint-mode-on
    :hook (shell-mode-hook . ansi-color-for-comint-mode-on))

  (leaf sh-script
    :config
    (unbind-key "C-c C-d" sh-mode-map)
    :bind (:sh-mode-map ("C-c C-p" . sh-cd-here)))

  (bash-completion-setup)
  (defvar my-shell-file-name (getenv "SHELL"))
  :custom
  ;; Emacsを起動したshellを使用する（bashからの起動を前提）
  (explicit-shell-file-name . my-shell-file-name)
  (explicit-bash-args . '("--login" "-i"))
  ;; shell-modeでのファイル名補完
  (shell-file-name-chars . "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")

  :hook (shell-mode-hook . (lambda ()
                             ;; SHELL で ^M が付く場合は ^M を削除する
                             (set-buffer-process-coding-system
                              'undecided-dos 'sjis-unix))))

(leaf haxe-mode :ensure t
  :custom
  (tab-width . 4)
  (indent-tabs-mode . nil)
  (fill-column . 80))

(leaf proof-general :ensure t)

(leaf popwin :ensure t :require t
  :defun popwin-mode
  :custom
  ;; popwin対象
  (popwin:special-display-config
   . '(("*quickrun*" :stick t)
       ("*Google Translate*")
       (completion-list-mode :noselect t) ;; 全completionを対象
       ("*Warnings*")
       (" *auto-async-byte-compile*")
       ("*Kill Ring*")
       (" *undo-tree*")
       ("*Help*")
       ("*xref*")
       ("*Backtrace*")))
  :config
  (popwin-mode 1))

;; ImageMagickをaptでいれておく
;; 非同期でimage-diredを動作させ、大量画像でフリーズしないようにするパッケージ
;; BUG: ディレクトリを開く初回時にサムネイル作成に失敗する。
;;      diredバッファでimage-dired-create-thumbsを実行して手動でサムネイル
;;      を作ると、image-diredが問題なく動くようになる。
;;      --no-initを使って、image-dired+だけで動かすと問題は起こらない。
;;      何らかの自分のinitファイルが問題を引き起こしている。
;;      Error-log
;;      image-diredx--invoke-process: Wrong type argument: processp, [nil 23723 12045 294055 nil image-dired-thumb-queue-run nil nil 600000]
(leaf image-dired+ :ensure t
  :if (executable-find "convert")
  :commands image-dired
  :config
  ;; Emacs26からは非同期なimage-diredがあり、コンフリクトするのでオンしない
  (if (version< emacs-version "26") ; Emacs25以下
      (progn (image-diredx-async-mode 1)
             (image-diredx-adjust-mode 1)))
  ;; lrでサムネイルが回転するのを削除
  (if (version<= "26" emacs-version) (unbind-key "r" image-map)) ; Emacs26以上
  (unbind-key "r" image-dired-thumbnail-mode-map)
  :bind ((:image-dired-thumbnail-mode-map
          :package image-dired
          ("C-n" . image-diredx-next-line)
          ("C-p" . image-diredx-previous-line)
          ("<down>" . image-diredx-next-line)
          ("<up>" . image-diredx-previous-line)
          ("j" . image-diredx-next-line)
          ("k" . image-diredx-previous-line)
          ("h" . image-dired-backward-image)
          ("l" . image-dired-forward-image)
          ("g" . revert-buffer)) ; 更新
         (:image-dired-display-image-mode-map
          :package image-dired
          ("f" . image-dired-display-current-image-full)
          ("0" . image-dired-display-current-image-sized))))

;; アクティブかどうかでバッファーのモードラインの色を変える
(leaf hiwin :ensure t
  :config
  ;; (set-face-background 'hiwin-face "gray92")
  (set-face-attribute 'mode-line nil :background "light sky blue")
  (set-face-attribute 'mode-line-inactive nil
                      :background "light gray"
                      :foreground "dim gray"))

(leaf smartparens :ensure t
  :diminish smartparens-mode
  :defun sp-local-pair
  :config
  (smartparens-global-mode t)
  ;; 一部のモードでは'での補完を行わない
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" "'")
  (sp-local-pair 'lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-mode "`" nil :actions nil)
  (sp-local-pair 'sml-mode "'" nil :actions nil)
  (sp-local-pair 'inferior-sml-mode "'" nil :actions nil)
  (sp-local-pair 'tuareg-mode "'" nil :actions nil))

(leaf ivy :ensure t
  :defvar ivy-height-alist
  :custom
  (ivy-count-format . "(%d/%d) ")
  :config

  (leaf swiper :ensure t
    :custom
    (swiper-include-line-number-in-search . t)
    :bind (("C-s" . swiper)
           ("C-M-s" . swiper-all)
           ("C-c s" . isearch-forward)
           (:swiper-map
            ("M-%" . swiper-query-replace)
            ("C-w" . ivy-yank-word)
            ("C-M-y" . ivy-yank-char))))

  (leaf counsel :ensure t
    :init
    ;; dotファイルとコンパイルファイルなどを無視する
    ;; .キーを押せばdotスタートファイルは表示される
    (defvar my-counsel-find-file-ignore-regexp
      (concat "\\(\\`\\.\\)\\|" (regexp-opt completion-ignored-extensions)))
    :custom
    (counsel-find-file-ignore-regexp . my-counsel-find-file-ignore-regexp)
    (counsel-mark-ring-sort-selections . nil)
    :config
    ;; counsel-yank-popの高さをデフォルト5から10に拡大する
    (setq ivy-height-alist
          (cons '(counsel-yank-pop . 10)
                (assq-delete-all 'counsel-yank-pop ivy-height-alist)))
    :bind (("M-x" . counsel-M-x)
           ("M-r" . counsel-command-history)
           ("C-x C-f" . counsel-find-file)
           ("C-x f" . counsel-recentf)
           ("C-c C-d" . counsel-describe-function)
           ("C-c g" . counsel-git-grep)
           ("C-x b" . counsel-switch-buffer)
           ("C-M-y" . counsel-yank-pop)
           ("C-c C-SPC" . counsel-mark-ring)
           (:counsel-find-file-map
            ("^" . counsel-up-directory))))

  (leaf ivy-rich :ensure t :require t
    :after ivy
    :defvar ivy-rich-display-transformers-list
    :custom
    (ivy-format-function . #'ivy-format-function-line)
    (ivy-rich-path-style . 'abbrev)
    :config
    ;; ivy-switch-bufferと同じrichをcounsel-switch-bufferでも使う
    (let ((plist ivy-rich-display-transformers-list))
      (unless (plist-member plist #'counsel-switch-buffer)
        (plist-put plist #'counsel-switch-buffer
                   (plist-get plist #'ivy-switch-buffer))))
    (ivy-rich-mode 1))

  (leaf ivy-prescient :ensure t
    :after ivy
    :config
    (ivy-prescient-mode 1))

  ;; 2019/05/11のswiperアップデートでswiperとavy-migemoの関係が壊れている
  ;; 暫定的にavy-migemoをpackageからアインストールし、PRのcommitを採用している。
  (leaf avy-migemo :require t
    :el-get (avy-migemo
             :url "https://github.com/yonta/avy-migemo.git"
             :branch "fix-tam171ki-and-obsolute")
    ;; cmigemoをいれておく
    ;; https://github.com/koron/cmigemo
    :if (executable-find "cmigemo")
    :after swiper
    :defun avy-migemo-mode
    :init

    (leaf avy :ensure t)

    (leaf migemo :ensure t :require t
      :defun migemo-init
      :custom
      (migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict")
      :config
      (migemo-init))

    :config
    (avy-migemo-mode 1)
    (require 'avy-migemo-e.g.swiper)
    (require 'avy-migemo-e.g.counsel)))

;; バグで動かない
;; https://github.com/jschaf/esup/issues/54
(leaf esup :disabled t)

(leaf auto-package-update :ensure t
  :custom
  (auto-package-update-delete-old-versions . t)
  (auto-package-update-prompt-before-update . t)
  ;; (auto-package-update-hide-results . t)
  :config
  (auto-package-update-maybe))

(leaf git-gutter-fringe :ensure t :require t
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode)
  :config
  (set-face-foreground 'git-gutter-fr:added "lime green")
  (set-face-background 'git-gutter-fr:added "white smoke")
  (set-face-foreground 'git-gutter-fr:modified "blue")
  (set-face-background 'git-gutter-fr:modified "white smoke")
  (eval-when-compile (require 'fringe-helper))
  (fringe-helper-define 'git-gutter-fr:modified nil
  "X......."
  "XXXX...."
  "XXXXXX.."
  "XXXXXXXX"
  "XXXXXXXX"
  "XXXXXX.."
  "XXXX...."
  "XX......")
  (fringe-helper-define 'git-gutter-fr:deleted nil
  "........"
  "........"
  "........"
  "XXXXXXXX"
  "XXXXXXXX"
  "XXXXXXXX"
  "........"
  "........"))

(leaf dumb-jump :ensure t
  :defvar dumb-jump-selector
  :config
  (setq dumb-jump-selector 'ivy)
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)))

(leaf volatile-highlights :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  (set-face-background 'vhl/default-face "light cyan"))

(leaf expand-region :ensure t
  :bind ("C-`" . er/expand-region))

(leaf async :ensure t
  :no-require
  :custom
  (async-bytecomp-allowed-packages . '(all))
  :config
  (async-bytecomp-package-mode 1))

(leaf which-key :ensure t
  :diminish which-key-mode
  :defvar which-key-side-window-max-height
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-side-window-max-height 0.4))

(leaf sudo-edit :ensure t)

(leaf visual-regexp :ensure t
  :bind ("M-&" . vr/query-replace))

;; package.elのリストを綺麗で便利にする
(leaf paradox :ensure t
  :custom
  (paradox-execute-asynchronously t)
  :config
  (paradox-enable))

(leaf auto-highlight-symbol :ensure t
  :diminish auto-highlight-symbol-mode
  :custom
  (ahs-idle-interval . 2.0)
  :custom-face
  ((ahs-plugin-defalt-face
    . '((t (:foreground "black" :background "light sky blue"))))
   (ahs-face . '((t (:foreground "ghost white" :background "slate gray")))))
  :config
  (global-auto-highlight-symbol-mode t))

(leaf highlight-parentheses :ensure t
  :diminish highlight-parentheses-mode
  :hook (prog-mode-hook . highlight-parentheses-mode))

(leaf rainbow-delimiters :ensure t
  :defvar rainbow-delimiters-max-face-count
  :init

  (leaf color  :require t
    :after rainbow-delimiters
    :defun color-saturate-name)

  ;; 括弧の色をより強くする
  ;; https://qiita.com/megane42/items/ee71f1ff8652dbf94cf7
  (defun rainbow-delimiters-using-stronger-colors ()
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30))))

  :hook ((emacs-startup-hook . rainbow-delimiters-using-stronger-colors)
         (prog-mode-hook . rainbow-delimiters-mode)))

(leaf undo-tree :ensure t
  :bind ("C-c C-/" . undo-tree-visualize))

(leaf dockerfile-mode :ensure t)

(leaf yaml-mode :ensure t)

(leaf jenkinsfile-mode
  :el-get (jenkinsfile-mode
           :url "https://github.com/spotify/dockerfile-mode.git")

  :init
  ;; jenkinsfile-modeに必要
  (leaf groovy-mode :ensure t)
  :mode "^Jenkinsfile\\'")

(leaf elisp-mode
  :init

  (leaf auto-async-byte-compile :ensure t
    :hook (emacs-lisp-mode-hook . enable-auto-async-byte-compile-mode))

  (leaf lispxmp :ensure t
    :bind ("C-M-;" . lispxmp))

  (leaf macrostep :ensure t
    :bind ((:emacs-lisp-mode-map ("C-c e" . macrostep-expand))
           (:lisp-interaction-mode-map ("C-c e" . macrostep-expand))))

  (defun eval-region-or-line ()
    "Eval active region or current line."
    (interactive) (call-with-region-or-line #'eval-region))
  :hook (emacs-lisp-mode-hook
         . (lambda ()
             (setq-local company-backends
                         '((company-capf
                            :with company-files company-dabbrev-code
                            company-yasnippet)))))
  :bind ((:emacs-lisp-mode-map ("C-c C-r" . eval-region-or-line))
         (:lisp-interaction-mode-map ("C-c C-r" . eval-region-or-line))))

(leaf python
  :defun python-shell-send-region
  :init

  ;; aptかpipでautopep8を入れておく
  ;; aptでpython-autopep8をいれておく
  (leaf py-autopep8 :ensure t
    :if (executable-find "autopep8")
    :hook (python-mode-hook . py-autopep8-enable-on-save))

  (leaf highlight-indentation :ensure t
    :diminish highlight-indentation-mode
    ;; インデントに意味のあるPythonでとりあえず使う
    :hook (python-mode-hook . highlight-indentation-mode))

  (leaf jedi-direx :ensure t
    :init

    (leaf jedi :ensure t
      ;; jedi-direxの依存関係にjediがあるためいれる。
      ;; しかし、jediはauto-completeのためのパッケージであり、
      ;; company-jediとコンフリクトする。
      ;; そのため、jediが行うjedi-core.elのへ変数の登録をnilに上書きする。
      :init
      (setq jedi:setup-function nil)
      (setq jedi:mode-function nil)
      :config
      (setq jedi:setup-function nil)
      (setq jedi:mode-function nil))

    :hook (jedi-mode-hook . jedi-direx:setup)
    :bind (:jedi-mode-map
           ("C-c x" . jedi-direx:pop-to-buffer)
           ("C-c C-x" . jedi-direx:switch-to-buffer)))

  (defun python-shell-send-region-or-line ()
    "Call REPL with active region or current line."
    (interactive) (call-with-region-or-line #'python-shell-send-region))
  :custom
  (python-shell-interpreter . "python3")
  (python-indent-offset . 4)
  :hook ((python-mode-hook
          . (lambda ()
              (setq-local company-backends
                          '((company-jedi :with company-yasnippet)
                            ;; string内で補完する
                            company-yasnippet
                            company-files))))
         (inferior-python-mode-hook
          . (lambda ()
              (setq-local company-backends
                          '((company-capf
                             :with company-files company-dabbrev-code
                             company-yasnippet))))))
  :bind (:python-mode-map
         ("C-c C-r" . python-shell-send-region-or-line)
         ("<backtab>" . python-indent-shift-left)))

(leaf hl-line+
  :el-get (hl-line+
           :url "https://github.com/emacsmirror/hl-line-plus.git")
  :defvar hl-line-face
  :defun (toggle-hl-line-when-idle hl-line-when-idle-interval)
  :config
  (toggle-hl-line-when-idle 1)
  (hl-line-when-idle-interval 4)
  (set-face-background hl-line-face "light cyan"))

(leaf dired
  :defun (dired-various-sort-change reload-current-dired-buffer)
  :custom
  ;; dired-modeがlsコマンドに渡すオプションを設定する
  ;; l: 長い表示、dired-modeに必須のオプション
  ;; g: ユーザ名を非表示
  ;; G: グループ名を非表示
  ;; h: kbyte・Mbyteの使用
  ;; F: ディレクトリに「/」を表示
  ;; A: 「.」と「..」を非表示でドットファイルを表示
  ;;(setq dired-listing-switches "-gGhFA")
  (dired-listing-switches . "-lgGhF")
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
  :bind (:dired-mode-map
         ("s" . dired-various-sort-change-or-edit)
         ("C-m" . dired-my-advertised-find-file)
         ("^" . dired-my-up-directory)
         ("C-." . toggle-dired-listing-switches)
         ("r" . wdired-change-to-wdired-mode)))

(leaf help-mode
  ;; Alt+左右でヘルプの進む・戻るを行う、デフォルトはl/r
  :bind (:help-mode-map
         ("M-<left>" . help-go-back)
         ("M-<right>". help-go-forward)))

(leaf ibuffer
  :defvar (ibuffer-inline-columns ibuffer-formats)
  :defun ibuffer-current-buffer
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
         (:ibuffer-mode-map ("C-x C-f" . ibuffer-find-file-by-counsel))))

(leaf whitespace
  :diminish global-whitespace-mode
  :defvar whitespace-line-column
  :init
  ;; white spaceをオン
  (global-whitespace-mode t)
  :custom
  ;; 空白などの可視化、対象はタブ文字、80文字超え部、行末の空白、全角スペース
  (whitespace-style . '(face tabs lines-tail trailing spaces empty))
  ;; 保存前に自動でクリーンアップ、対象はwhitespace-styleでセットしたもの
  (whitespace-action . '(auto-cleanup))
  ;; spacesの対象は全角スペースのみ
  (whitespace-space-regexp . "\\(　+\\)")
  :custom-face
  ((whitespace-trailing . '((t (:background "lavender"))))   ; 行末スペース
   (whitespace-space . '((t (:background "DarkSeaGreen1")))) ; 全角スペース
   (whitespace-tab . '((t (:background "light goldenrod yellow")))) ; タブ
   (whitespace-empty . '((t (:background nil))))             ; 空行
   (whitespace-line . '((t (:foreground nil
                            :background "khaki")))))         ; 80文字オーバー
  ;; java-modeではカラムオーバーの限界をデフォルトの80から100に変更する
  :hook ((java-mode-hook . (lambda () (setq whitespace-line-column 100)))
         (change-major-mode-hook
          . (lambda () (setq whitespace-line-column 80)))))

(leaf winner
  :config
  (winner-mode 1)
  :bind (("C-M-<left>" . winner-undo)
         ("C-M-<right>" . winner-redo)))

(leaf dummy-line-num :require nil ; dummy
  :init

  (leaf display-line-numbers
    :if (version<= "26" emacs-version) ; Emacs26以降
    :init
    (global-display-line-numbers-mode 1)
    (set-face-background 'line-number "white smoke"))

  (leaf linum
    :if (version< emacs-version "26") ; Emacs25以下
    :defvar linum-format
    :init
    (global-linum-mode 1)
    (setq linum-format "%4d ")))

(leaf autoinsert
  :defvar (auto-insert-directory auto-insert-alist)
  :init
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
  (defun replace-template ()
    "Add template string to file."
    (mapc (lambda (template-replacement)
            (goto-char (point-min))
            (while (search-forward (car template-replacement) nil t)
              (replace-match (funcall (cdr template-replacement)))))
          template-replacements-alists)
    (goto-char (point-min))
    (message "done."))
  :config
  (auto-insert-mode 1)
  (setq auto-insert-directory "~/.emacs.d/autoinsert/")
  (setq auto-insert-alist
        (nconc
         '(("Test\\.\\(cpp\\|cc\\|cxx\\)$" .
            ["templateTest.cpp" replace-template])
           ("\\.\\(cpp\\|cc\\|cxx\\)$" . ["template.cpp" replace-template])
           ("\\.\\(hpp\\|hh\\|hxx\\)$" . ["template.hpp" replace-template])
           ("\\.c$" . ["template.c" replace-template])
           ("\\.ino$" . ["template.ino" replace-template]))
         auto-insert-alist)))

(leaf recentf
  :defvar (recentf-save-file recentf-max-saved-items recentf-auto-cleanup)
  :config
  ;; 最近使ったファイルをrecentfファイルに保存する
  ;; counsel-recentfで呼び出せる
  (setq recentf-save-file "~/.emacs.d/.recentf")
  (setq recentf-max-saved-items 100)
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1)

  (leaf recentf-ext :ensure t))

(leaf subword
  :diminish subword-mode
  :config
  (global-subword-mode 1))

;;; WSLでのブラウザ設定
;; aptでubuntu-wslをいれておく
(leaf browse-url
  :if (getenv "WSLENV")
  :custom
  (browse-url-browser-function . #'browse-url-generic)
  (browse-url-generic-program . "wslview"))

;;; init_package.el ends here
