;;; init_package --- settings about packages -*- lexical-binding: t; -*-

;;; Commentary:
;; This is settings about package.
;; It is written using `leaf.el`.
;; See also ../themes/original-theme.el for settings of faces.

;;; Code:

;; パッケージの全てをclからcl-libに移行しきれないので警告を無視する
(with-no-warnings (require 'cl))

(require 'package)
;; パッケージアーカイブの順番は関係ない。
;; 優先度はバージョン番号が大きい方が優先されるため、
;; melpaが常にmarmaladeよりも優先される。
;; melpaよりもmelpa-stableを優先するなどの別途優先度をつけるには、
;; `package-archive-priorities'を使って設定する。
(add-to-list 'package-archives ; MELPAを追加
             '("melpa" . "https://melpa.org/packages/"))
(setq package-quickstart t)
(eval-and-compile (package-initialize))

(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

(eval-and-compile
  (leaf leaf-keywords
    ;; :diminishを有効にし、モードラインをスッキリさせる
    :ensure t diminish
    :config
    (leaf el-get :ensure t
      :custom ((el-get-git-shallow-clone . t)))
    (leaf diminish :ensure t)
    (leaf-keywords-init)))

(leaf bind-key :ensure t :require t)

(leaf util :defun call-with-region-or-line) ; dummy, init_util.el

(leaf initchart
  :el-get (initchart
           :url "https://github.com/yuttie/initchart.git"))

;;; Company and Flycheck

(leaf company
  :req "clangがあるとより便利らしいので、aptでclangをいれておく"
  :init
  (leaf company :ensure t
    :defvar (company-mode-map company-backends)
    :global-minor-mode global-company-mode
    :custom
    (company-idle-delay . 0)
    (company-minimum-prefix-length . 2)
    (company-selection-wrap-around . t)
    (company-require-match . 'never)
    (company-dabbrev-code-other-buffers . 'code)
    (company-dabbrev-code-everywhere . t)
    (company-dabbrev-ignore-case . t)
    (company-dabbrev-code-ignore-case . t)
    (company-etags-ignore-case . t)
    (company-transformers
     . '(company-sort-by-occurrence company-sort-by-backend-importance))
    (company-tooltip-align-annotations . t)
    (company-lighter-base . "")
    :config
    (setq completion-ignore-case t)
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    (unbind-key "RET" company-active-map)
    (unbind-key [return] company-active-map)
    :bind* ("C-M-i" . company-complete)
    :bind ((:company-active-map
            ("C-n" . company-select-next)
            ("C-p" . company-select-previous)
            ("<tab>" . company-complete-common-or-cycle)
            ("<backtab>" . company-select-previous)
            ("C-f" . company-complete-selection)
            ("C-d" . company-show-doc-buffer)
            ("C-s" . company-filter-candidates)
            ("C-o" . company-other-backend))))

  (leaf company-quickhelp :ensure t
    :global-minor-mode company-quickhelp-mode
    :custom
    (company-quickhelp-delay . 0.5))

  (leaf company-prescient :ensure t
    :global-minor-mode company-prescient-mode)

  (leaf yasnippet :disabled t
    :ensure t yasnippet-snippets
    :defvar yas-minor-mode-map
    :global-minor-mode yas-global-mode
    :diminish yas-minor-mode
    :config
    ;; yas-expandは使わず、companyからyasを使う。
    (unbind-key "<tab>" yas-minor-mode-map)
    (unbind-key "TAB" yas-minor-mode-map))

  ;; 絵文字入力2種を試してみる 🚀
  (leaf company-emoji :ensure t
    ;; :symbol 入力時に補完されるのが邪魔なので、backendsには入れない
    ;; :config
    ;; (add-to-list 'company-backends 'company-emoji)
    :preface
    (defun company-emoji-only ()
      "Complete only with emoji."
      (interactive)
      (let ((company-backends '(company-emoji)))
        (company-complete)))
    :bind* ("C-M-o" . company-emoji-only))

  (leaf ivy-emoji :ensure t
    :after ivy)

  (leaf company-c-headers :ensure t :disabled t)
  (leaf company-arduino :ensure t :disabled t))

;; メジャーモードによってlspの次のcheckerを切り替える
;; https://github.com/flycheck/flycheck/issues/1762
(defvar-local flycheck-local-checkers nil)
(leaf flycheck
  :init
  (leaf flycheck :ensure t
    :req "pipでflake8とmypyをいれておく"
    :defvar (flycheck-checker
             flycheck-gcc-language-standard
             flycheck-clang-language-standard)
    :global-minor-mode global-flycheck-mode
    ;; 先にflycheck-local-checkersを探索する
    :preface
    (defun +flycheck-checker-get(fn checker property)
      (or (alist-get property (alist-get checker flycheck-local-checkers))
          (funcall fn checker property)))
    :advice
    (:around flycheck-checker-get +flycheck-checker-get)
    :custom
    (flycheck-python-flake8-executable . "flake8")
    (flycheck-checker-error-threshold . 250)
    (flycheck-mode-line-prefix . "f")
    :bind (:flycheck-mode-map
           ("M-p" . flycheck-previous-error)
           ("M-n" . flycheck-next-error)))

  :init
  (leaf pos-tip :ensure t
    :config
    ;; HiDPIでpos-tipのフォントサイズが小さくなる問題に暫定対処
    ;; pos-tipはframeのデフォルトフォントを使用するので、
    ;; 強制的に書き換えて対処する
    (set-frame-font
     "-VL  -VL ゴシック-normal-normal-normal-*-29-*-*-*-*-0-iso10646-1"
     nil t)
    :custom
    (pos-tip-use-relative-coordinates . t)) ; pos-tipをフレームに収める

  (leaf flycheck-pos-tip :ensure t
    :after flycheck
    :global-minor-mode flycheck-pos-tip-mode
    :custom
    ;; pos-tipを自動で消さない
    (flycheck-pos-tip-timeout . 0))

  (leaf flycheck-color-mode-line :ensure t
    :after flycheck
    :hook (flycheck-mode-hook . flycheck-color-mode-line-mode))

  (leaf flycheck-ocaml :ensure t))

;;; MODE

(leaf emacs-lisp
  :init
  (leaf elisp-mode
    :defer-config
    (defun my-eval-region-or-line ()
      "Eval active region or current line."
      (interactive) (call-with-region-or-line #'eval-region))
    :bind ((:lisp-mode-shared-map ("C-c C-r" . my-eval-region-or-line))))

  (leaf eldoc :diminish eldoc-mode)

  (leaf auto-async-byte-compile :ensure t
    :hook (emacs-lisp-mode-hook . enable-auto-async-byte-compile-mode))

  (leaf auto-compile :ensure t
    :doc "設定はinit.elに")

  (leaf lispxmp :ensure t
    :bind (:lisp-mode-shared-map
           :package elisp-mode
           ("C-M-;" . lispxmp)))

  (leaf macrostep
    :el-get (macrostep
             :url "https://github.com/justbur/macrostep.git"
             :branch "fix-missing-map")
    :bind (:lisp-mode-shared-map
           :package elisp-mode
           ("C-c e" . macrostep-expand))))

(leaf cc-mode
  :init
  (leaf c-mode
    :preface
    :custom
    ;; 基本的にK&Rスタイルを使う
    (c-default-style . '((java-mode . "java")
                         (awk-mode . "awk")
                         (other . "k&r")))
    :defer-config
    (add-to-list 'company-backends
                 '(company-clang :with company-dabbrev-code))
    (require 'smartparens-c))

  (leaf c++-mode
    :preface
    (defun my-c++-mode-hook ()
      "Setting for c++-mode."
      (setq-local flycheck-gcc-language-standard "c++11")
      (setq-local flycheck-clang-language-standard "c++11"))
    :hook (c++-mode-hook . my-c++-mode-hook)
    :defer-config
    (add-to-list 'company-backends
                 '(company-clang ;; company-c-headers
                   :with company-dabbrev-code))
    (require 'smartparens-c))

  (leaf rainbow-mode :ensure t
    :doc "#ff0000などに色をつける"
    :diminish t
    :custom
    (rainbow-r-colors . t)                ; R color listを使う
    (rainbow-html-colors . t)             ; html color listを使う
    :hook (c++-mode-hook arduino-mode-hook)))

(leaf tuareg :ensure t
  :doc "ocaml mode"
  :req "opam install tuareg")

(leaf arduino-mode :ensure t :disabled t
  :doc "TODO: set compiler and libraries path by environment")

(leaf quickrun :ensure t
  :custom
  (quickrun-timeout-seconds . -1)       ; タイムアウトで処理を中止させない
  :defer-config
  ;; python-modeでpython3を使う
  (quickrun-add-command "python"
    '((:command . "python3")
      (:exec . "%c %s")
      (:compile-only . "pyflakes %s"))
    :mode 'python-mode)
  :bind ("C-c c" . quickrun))

(leaf markdown-mode :ensure t
  :req "aptでmarkdown、pipでgripをいれておく"
  :if (executable-find "markdown") (executable-find "grip")
  :defvar markdown-mode-map
  :mode ("README\\.md\\'" . gfm-mode)
  :defer-config
  (unbind-key "C-c '" gfm-mode-map)
  :bind (:gfm-mode-map ("C-c `" . markdown-edit-code-block))

  :init
  (leaf edit-indirect :ensure t
    :doc "markdownでコードブロックの編集のために必要")

  (leaf grip-mode :ensure t
    :doc "GitHubのSettings/Developer settings/Personal access tokensでつくった"
    :doc "空権限のtokenをcustom.elのgrip-github-passwordに書き込む"
    :doc "gfm-modeのときにgrip-modeで起動する"
    :custom
    (grip-github-password . ""))

  :custom
  (markdown-command . "markdown")
  ;; style sheetは生成HTMLと同フォルダにあるstyle.cssにする
  (markdown-css-paths . '("style.css"))
  :defer-config
  (require 'smartparens-markdown)
  (unbind-key "C-c C-f" markdown-mode-map))

(leaf csv-mode :ensure t)

(leaf sml-mode
  :init
  (leaf sml-mode
    :el-get (sml-mode
             :url "https://github.com/yonta/sml-mode.git"
             :branch "add-smlsharp")
    :mode ("\\.smi\\'" "\\.ppg\\'")
    :interpreter "smlsharp"
    :defun (sml-prog-proc-proc
            sml-prog-proc-send-string
            my-sml-prog-proc-send-region-by-string)
    :defvar company-minimum-prefix-length
    :preface
    (defun my-sml-set-company-settings ()
      "Set company settings for SML mode."
      (setq-local completion-ignore-case nil)
      (setq-local company-minimum-prefix-length 3))
    :hook ((sml-mode-hook . my-sml-set-company-settings)
           (inferior-sml-mode-hook . my-sml-set-company-settings))
    :custom
    (sml-indent-level . 2)
    (sml-indent-args . 2)
    (sml-struct-indent-level . 2)
    ;; sml-modeのrun-smlでデフォルトSMLコマンドをsmlsharpにする
    (sml-program-name . "smlsharp")
    (sml-electric-pipe-mode . nil)
    :defer-config
    (defun my-sml-prog-proc-send-region-by-string (begin end)
      (interactive "r")
      (let ((proc (sml-prog-proc-proc))
            (code (buffer-substring begin end)))
        (sml-prog-proc-send-string proc code)))
    (defun my-sml-prog-proc-send-region-or-line ()
      "Call REPL with active region or current line."
      (interactive)
      (call-with-region-or-line #'my-sml-prog-proc-send-region-by-string))
    :bind (:sml-mode-map
           ("C-c C-r" . my-sml-prog-proc-send-region-or-line)
           ("C-c C-p" . sml-run)))

  (leaf company-mlton
    :el-get (company-mlton
             :url "https://github.com/yonta/company-mlton.git"
             :branch "add-smlsharp")
    :defun company-mlton-basis-autodetect
    :custom
    (company-mlton-modes . '(sml-mode inferior-sml-mode))
    ;; MLtonのbasisを除き、SMLのbasisを使う
    (company-mlton-basis-file
     . "~/.emacs.d/el-get/company-mlton/sml-basis-lib.basis")
    :defer-config
    (add-to-list 'company-backends
                 '(company-mlton-keyword
                   company-mlton-basis
                   :with company-dabbrev-code))
    :hook
    (sml-mode-hook . company-mlton-basis-autodetect))

  (leaf flycheck-smlsharp
    :el-get (flycheck-smlsharp
             :url "https://github.com/yonta/flycheck-smlsharp.git")
    :after sml-mode
    :require t)

  (leaf flycheck-mlton
    :el-get gist:80c938a54f4d14a1b75146e9c0b76fc2:flycheck-mlton
    :hook (sml-mode-hook . (lambda () (require 'flycheck-mlton))))

  (leaf sml-eldoc :disabled t
    :el-get (sml-eldoc
             :url "https://raw.githubusercontent.com/xuchunyang/emacs.d/master/lisp/sml-eldoc.el")
    :hook (sml-mode-hook . sml-eldoc-turn-on)))

(leaf python
  :init
  (leaf python
    :defun python-shell-send-region
    :defer-config
    (defun my-python-shell-send-region-or-line ()
      "Call REPL with active region or current line."
      (interactive) (call-with-region-or-line #'python-shell-send-region))
    ;; 「変数の再定義が禁止」など、pepに従ったflake8よりエラーが厳しい
    ;; 必要なときにだけflycheck-select-checkerで利用する
    ;; :hook (python-mode-hook
    ;;        . (lambda () (setq-local flycheck-checker 'python-mypy))))
    :bind (:python-mode-map
           ("C-c C-r" . my-python-shell-send-region-or-line)
           ("<backtab>" . python-indent-shift-left))
    :custom
    (python-shell-interpreter . "python3")
    (python-indent-offset . 4)
    :defer-config
    (add-to-list 'company-backends
                 '(company-jedi
                   :with company-dabbrev-code company-dabbrev))
    (require 'smartparens-python))

  (leaf pip-requirements :ensure t)

  (leaf jedi-core :ensure t
    :hook (python-mode-hook . jedi:setup)
    ;; 関数の引数の情報が便利なので、ミニバッファに表示する
    :custom ((jedi:tooltip-method . nil)
             (jedi:use-shortcuts . t) ; M-,/M-.にjediを使う
             (jedi:environment-root . "python3-default")))

  ;; pipでvirtualenvを入れておく
  ;; Ubuntu bionicのpythonは2.7なので、予め以下コマンドでPython3の環境を作る
  ;; Ubuntu focalではpython3なので必要ない
  ;;   virtualenv -p python3 .python-environment/python3-default
  ;; その後、初回起動時にjedi:install-serverする
  ;; 必要に応じて補完したいライブラリを、activateしてpip installする
  ;;   source ~/.emacs.d/.python-environments/python3-default/bin/activate
  ;;   pip install -r ~/.emacs.d/requirements.txt
  (leaf company-jedi :ensure t)

  (leaf py-autopep8 :ensure t
    :req "pipでautopep8をいれておく"
    :if (executable-find "autopep8")
    :hook (python-mode-hook . py-autopep8-enable-on-save))

  (leaf highlight-indentation :ensure t
    :diminish highlight-indentation-mode
    ;; インデントに意味のあるPythonでとりあえず使う
    :hook (python-mode-hook . highlight-indentation-mode))

  (leaf importmagic :ensure t
    :req "pipでimportmagic3とepcをいれておく"
    :hook (python-mode-hook . importmagic-mode))

  (leaf py-isort :ensure t
    :req "pipでisortをいれておく"
    :hook (before-save-hook . py-isort-before-save)))

(leaf ruby
  :init
  (leaf ruby-electric :ensure t
    :doc "def/doなどに自動でendを挿入する"
    :diminish t
    :hook (ruby-mode-hook . ruby-electric-mode))

  (leaf inf-ruby :ensure t
    :hook (ruby-mode-hook . inf-ruby-minor-mode)
    :bind ((:ruby-mode-map
            :package ruby-mode
            ("C-c C-p" . inf-ruby)
            ("C-c p" . inf-ruby-console-auto))
           (:web-mode-map
            :package web-mode
            ("C-c C-p" . inf-ruby)
            ("C-c p" . inf-ruby-console-auto)))
    :custom
    (inf-ruby-console-environment . "development"))

  (leaf rubocop :ensure t :if (executable-find "rubocop")
    :req "gemでrubocopを入れておく"
    :req "gem install rubocop")

  (leaf rufo :ensure t
    :req "gemでrufoを入れておく"
    :req "gem install rufo"
    :doc "TODO: rufoやめてrubocop -aに移行したい"
    :if (executable-find "rufo")
    :diminish rufo-minor-mode
    :hook (ruby-mode-hook . rufo-minor-mode))

  (leaf rubocopfmt :ensure t :disabled t
    :if (executable-find "rubocop")
    :diminish rubocopfmt-mode
    :hook (ruby-mode-hook . rubocopfmt-mode))

  (leaf rspec-mode :ensure t :diminish t)

  (leaf yard-mode :ensure t
    :diminish t
    :hook (ruby-mode-hook . yard-mode))

  (leaf ruby-tools :ensure t
    :hook (ruby-mode-hook . ruby-tools-mode))

  (leaf company-ignore
    :defun company-ignore
    :el-get (company-ignore
             :url "https://github.com/yonta/company-ignore.git")
    :hook (ruby-mode-hook
           . (lambda ()
               (company-ignore 'company-capf '("do" "end"))
               (company-ignore 'company-keywords '("do" "end"))
               (company-ignore 'company-dabbrev-code '("do" "end"))
               (company-ignore 'company-dabbrev '("do" "end"))
               (company-ignore 'company-yasnippet '("do" "end")))))

  (leaf seeing-is-believing :ensure t :require t
    :config
    (diminish 'seeing-is-believing nil)
    :req "gem install seeing_is_believing")

  (leaf ruby-mode
    :req "gemでsolargraphを入れる"
    :req "gem install solargraph"
    :req "solargraph download-core"
    :req "yard gems"
    :doc "yard config --gem-install-yriでgem install時に自動生成する設定が便利"
    :req "プロジェクトルートでsolargraph bundleを実行"
    :req "プロジェクトにマジックコメントのファイルを設置"
    :url "https://solargraph.org/guides/rails"
    :custom
    (ruby-insert-encoding-magic-comment . nil)
    (dabbrev-abbrev-skip-leading-regexp . ":")
    :hook ((ruby-mode-hook . lsp-deferred)
           (ruby-mode-hook . seeing-is-believing)
           (ruby-mode-hook
            . (lambda ()
                (setq flycheck-local-checkers
                      '((lsp . ((next-checkers . (ruby-rubocop)))))))))))

(leaf lsp
  :init
  (leaf lsp-mode :ensure t
    :diminish t
    :defun lsp-find-definition lsp-find-references
    :hook ((lsp-mode-hook . lsp-enable-which-key-integration)
           (lsp-mode-hook . lsp-ui-mode))
    :custom
    ;; LSPでパフォーマンスの高いplistsを使う
    ;; .profileでexport LSP_USE_PLISTS=trueする
    ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
    (lsp-use-plists . t)
    ;; BUG:
    ;; Emacs28.1ではlsp-modeがjson parse errorする
    ;; Emacs29が出るまで暫定対処する
    ;; https://github.com/emacs-lsp/lsp-mode/issues/2681#issuecomment-1214902146
    :defer-config
    (eval-and-compile (require 's))
    :advice
    ;; same definition as mentioned earlier
    (:around json-parse-string
             (lambda (orig string &rest rest)
               (apply orig (s-replace "\\u0000" "" string)
                      rest)))
    ;; minor changes:
    ;; saves excursion and uses search-forward instead of re-search-forward
    (:around json-parse-buffer
             (lambda (oldfn &rest args)
               (save-excursion
                 (while (search-forward "\\u0000" nil t)
                   (replace-match "" nil t)))
               (apply oldfn args))))

  (leaf lsp-ui :ensure t
    :custom
    (lsp-ui-doc-show-with-cursor . t)
    (lsp-ui-doc-header . t)
    (lsp-ui-doc-include-signature . t)
    (lsp-ui-doc-delay . 2)
    (lsp-ui-sideline-code-action . t)
    (lsp-ui-doc-position . 'at-point)
    (lsp-ui-doc-border . "gray10")
    ;; WSL2ではexport WEBKIT_FORCE_SANDBOX=0すればXwidgetが使える
    ;; ただし、HighDPI対応をどうすればいいのかわからない。
    ;; Emacsを--with-x-toolkit=gtk3 --with-xwidgetsでビルドする必要がある
    ;; (lsp-ui-doc-use-webkit . t)
    (lsp-ui-doc-delay . 0.7)
    :bind (:lsp-ui-mode-map (("C-c <tab>" . lsp-ui-doc-focus-frame)
                             ("C->" . lsp-ui-imenu))))

  (leaf lsp-treemacs :ensure t
    :global-minor-mode lsp-treemacs-sync-mode)

  (leaf lsp-ivy :ensure t))

(leaf html-css
  :init
  (leaf web-mode :ensure t
    :mode "\\.erb\\'"
    :custom
    (web-mode-enable-comment-interpolation . t)
    (web-mode-enable-auto-pairing . nil)
    (web-mode-enable-auto-quoting . t)
    (web-mode-enable-auto-expanding . t)
    (web-mode-enable-current-element-highlight . t)
    (web-mode-enable-current-column-highlight . t)
    (web-mode-comment-style . 2)
    (web-mode-auto-close-style . 2)
    ;; web-modeとwhitespace-modeのコンフリクトでfaceがおかしくなるのを解消する
    ;; https://github.com/fxbois/web-mode/issues/119a
    (web-mode-display-table . nil)
    :defer-config
    (require 'smartparens-html)
    (sp-local-pair 'web-mode "<%" "%>"
                   :post-handlers '(("|| " "SPC") (" || " "=")))
    (unbind-key "C-c C-f" web-mode-map))

  (leaf impatient-mode :ensure t
    :doc "HTMLのライブプレビューモード")

  (leaf reformatter :ensure t
    :doc "el-getでいれるhtmlbeautifierが依存")

  (leaf htmlbeautifier
    :el-get (htmlbeautifier
             :url "https://github.com/yonta/htmlbeautifier.el.git")
    :hook (web-mode-hook . htmlbeautifier-format-on-save-mode)
    :custom (htmlbeautifier-keep-blank-lines . 1))

  (leaf erblint :ensure t
    :custom
    (erblint-check-command . "erblint --lint-all"))

  (leaf flycheck-markuplint
    :el-get (flycheck-markuplint
             :url "https://github.com/yonta/flycheck-markuplint.git")
    :hook ((web-mode-hook . flycheck-markuplint-setup)
           (html-mode-hook . flycheck-markuplint-setup)
           (mhtml-mode-hook . flycheck-markuplint-setup)))

  (leaf company-bootstrap5
    :defun company-bootstrap5
    :el-get (company-bootstrap5
             :url "https://github.com/yonta/company-bootstrap5.git"))

  (leaf company-bootstrap-icons
    :el-get (company-bootstrap-icons
             :url "https://github.com/yonta/company-bootstrap-icons.git"))

  (leaf company-web :ensure t
    :after web-mode
    :config
    (add-to-list 'company-backends
                 '(company-web-html
                   :with company-bootstrap5
                   company-bootstrap-icons
                   company-dabbrev-code company-dabbrev)))

  (leaf css-mode
    :custom
    (css-indent-offset . 2)
    :defer-config
    (add-to-list 'company-backends
                 '(company-css
                   :with company-bootstrap5
                   company-dabbrev-code company-dabbrev)))

  (leaf scss-mode
    :config
    ;; scssで正しいcheckerが走らない暫定対処
    ;; https://github.com/flycheck/flycheck/issues/1912
    (flycheck-define-checker general-stylelint
      "A checker for CSS and related languages using Stylelint"
      :command ("stylelint"
                (eval flycheck-stylelint-args)
                (option-flag "--quiet" flycheck-stylelint-quiet)
                (config-file "--config" flycheck-general-stylelintrc))
      :standard-input t
      :error-parser flycheck-parse-stylelint
      :predicate flycheck-buffer-nonempty-p
      :modes (scss-mode))
    (flycheck-def-config-file-var flycheck-general-stylelintrc
        (general-stylelint) nil)
    (add-to-list 'flycheck-checkers 'general-stylelint)
    :custom
    (flycheck-disabled-checkers . '(scss-stylelint))))

(leaf javascript
  :init
  (leaf js
    :hook ((js-mode-hook . lsp-deferred)
           (js-mode-hook
            . (lambda ()
                (setq flycheck-local-checkers
                      '((lsp . ((next-checkers . (javascript-eslint)))))))))
    :custom
    (js-indent-level . 2))

  (leaf add-node-modules-path :ensure t
    :custom
    ;; npm v9より`npm bin'が削除されたため、暫定対処
    ;; https://github.com/codesuki/add-node-modules-path/issues/23
    (add-node-modules-path-command . "echo \"$(npm root)/.bin\"")
    :hook ((js-mode-hook . add-node-modules-path)
           (typescript-mode-hook . add-node-modules-path)
           (markdown-mode-hook . add-node-modules-path)
           (css-mode-hook . add-node-modules-path)
           (web-mode-hook . add-node-modules-path)))

  (leaf json-mode :ensure t))

(leaf typescript
  :init
  (leaf typescript-mode :ensure t
    :req "npmでtypescript-language-serverとtypescriptを入れておく"
    :req "npm install -g typescript-language-server typescript"
    :defvar flycheck-check-syntax-automatically
    :hook ((typescript-mode-hook . lsp-deferred)
           (typescript-mode-hook
            . (lambda ()
                (setq-local flycheck-check-syntax-automatically
                            '(save mode-enabled))
                (setq flycheck-local-checkers
                      '((lsp . ((next-checkers . (javascript-eslint)))))))))
    :custom
    (typescript-indent-level . 2))

  (leaf prettier-js :ensure t
    :diminish prettier-js-mode
    :hook ((js-mode-hook . prettier-js-mode)
           (typescript-mode-hook . prettier-js-mode)))

  (leaf ts-comint :ensure t
    :if (executable-find "ts-node")
    :custom
    (ts-comint-program-command . "ts-node")
    :bind (:typescript-mode-map
           :package typescript-mode
           ("C-c C-r" . ts-send-region)
           ("C-c C-p" . run-ts))))

(leaf docker
  :ensure (t
           dockerfile-mode
           docker-compose-mode
           counsel-tramp
           yaml-mode)
  :bind ("C-c C-x d" . docker))

(leaf jenkinsfile-mode
  :ensure groovy-mode
  :el-get (jenkinsfile-mode
           :url "https://github.com/spotify/dockerfile-mode.git")
  :mode "^Jenkinsfile\\'")

(leaf haxe-mode :ensure t
  :custom
  (tab-width . 4)
  (fill-column . 80))

(leaf proof-general :ensure t :disabled t)

(leaf gnuplot-mode :ensure t
  :doc ".gpl .plt、.gp .gnuplotはautoloadで登録済み"
  :mode ("\\.gpl\\'" "\\.plt\\'"))

(leaf graphviz-dot-mode :ensure t)

(leaf git-modes :ensure t)

(leaf wakatime-mode :ensure t
  :global-minor-mode global-wakatime-mode
  :diminish t)

;;; Face

(leaf whitespace
  :defvar whitespace-line-column whitespace-style
  :global-minor-mode global-whitespace-mode
  :diminish global-whitespace-mode
  :custom
  ;; 空白などの可視化
  ;; 対象はタブ文字、80文字超え部、行末の空白、全角スペース、空白のみ行
  (whitespace-style . '(face tabs lines-tail trailing spaces empty))
  ;; 保存前に自動でクリーンアップ、対象はwhitespace-styleでセットしたもの
  (whitespace-action . '(auto-cleanup))
  ;; spacesの対象は全角スペースのみ
  (whitespace-space-regexp . "\\(　+\\)")
  ;; 一部モードで1行の最大文字数を変更する
  :hook ((java-mode-hook . (lambda () (setq-local whitespace-line-column 100)))
         (ruby-mode-hook . (lambda () (setq-local whitespace-line-column 120)))
         (web-mode-hook . (lambda () (setq-local whitespace-line-column 120)))))

(leaf parens
  :init
  (eval-when-compile (require 'smartparens)) ; sp-with-modesマクロの読み込み
  (leaf smartparens :ensure t
    :defun sp-local-pair
    :global-minor-mode smartparens-global-mode
    :diminish smartparens-mode
    :config
    ;; 一部のモードでは'での補完を行わない
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'lisp-mode "'" nil :actions nil)
    (sp-local-pair 'lisp-mode "`" nil :actions nil)
    (sp-local-pair 'sml-mode "(*" "*)")
    (sp-local-pair 'sml-mode "'" nil :actions nil)
    (sp-local-pair 'sml-mode "`" nil :actions nil)
    (sp-local-pair 'inferior-sml-mode "(*" "*)")
    (sp-local-pair 'inferior-sml-mode "'" nil :actions nil)
    (sp-local-pair 'inferior-sml-mode "`" nil :actions nil)
    (sp-local-pair 'tuareg-mode "'" nil :actions nil)
    ;; /*の後をいい感じにする
    (sp-with-modes '(js-mode typescript-mode)
      (sp-local-pair "/*" "*/" :post-handlers '(("|| " "SPC")
                                                ("* [i]||\n[i]" "RET")))) ;bug?
    ;; ｛の後にEnterすると｝の前に改行をつける
    (sp-with-modes
        '(web-mode js-mode css-mode typescript-mode)
      (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

  (leaf rainbow-delimiters :ensure t
    :defvar rainbow-delimiters-max-face-count
    :defun my-rainbow-delimiters-using-stronger-colors
    :defer-config
    ;; 白背景地には括弧の色をより強くする
    ;; https://qiita.com/megane42/items/ee71f1ff8652dbf94cf7
    (defun my-rainbow-delimiters-using-stronger-colors ()
      "Set delimiter to more strong color for white background."
      (if (string> (background-color-at-point) "#808080")
          (cl-loop
           for index from 1 to rainbow-delimiters-max-face-count do
           (let ((face
                  (intern (format "rainbow-delimiters-depth-%d-face" index))))
             (cl-callf color-saturate-name (face-foreground face) 30)))))
    (my-rainbow-delimiters-using-stronger-colors)
    :hook (prog-mode-hook . rainbow-delimiters-mode))

  (leaf color :require t
    :after rainbow-delimiters
    :defun color-saturate-name)

  (leaf highlight-parentheses :ensure t
    :diminish highlight-parentheses-mode
    :hook (prog-mode-hook . highlight-parentheses-mode)))

(leaf highlight
  :init
  (leaf auto-highlight-symbol :ensure t
    :leaf-defer nil
    :defvar ahs-modes
    :global-minor-mode global-auto-highlight-symbol-mode
    :diminish auto-highlight-symbol-mode
    :custom
    (ahs-default-range . 'ahs-range-whole-buffer)
    (ahs-disabled-minor-modes . '(iedit-mode ivy-mode))
    :config
    (push 'sml-mode ahs-modes)
    :bind (:auto-highlight-symbol-mode-map
           ("M-<up>" . ahs-backward)
           ("M-<down>" . ahs-forward)))

  (leaf volatile-highlights :ensure t
    :diminish volatile-highlights-mode
    :config
    (volatile-highlights-mode t))

  (leaf hl-line+
    :el-get (hl-line+
             :url "https://github.com/emacsmirror/hl-line-plus.git")
    :defun (toggle-hl-line-when-idle hl-line-when-idle-interval)
    :config
    (toggle-hl-line-when-idle 1)
    (hl-line-when-idle-interval 4)))

(leaf git-gutter-fringe :ensure t :require t
  :global-minor-mode global-git-gutter-mode
  :diminish git-gutter-mode
  :config
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

(leaf hiwin :ensure t
  :doc "アクティブかどうかでバッファーのモードラインの色を変える")

;; GitHubの絵文字をよく使うようなら有効にする
(leaf emojify :ensure t :disabled t
  :hook (after-init-hook . global-emojify-mode)
  :custom (emojify-emoji-styles . (ascii github)))

(leaf tree-sitter
  :doc "GitHubがAtom用に開発したインクリメンタルパーサ"
  :doc "高速で正確なsyntax highlightingを提供する"
  :doc "Emacs29では同梱されるようになった"
  :if (version< emacs-version "29") ; Emacs28以下
  :ensure t tree-sitter-langs
  :diminish tree-sitter-mode
  :global-minor-mode global-tree-sitter-mode
  :custom (tsc-dyn-get-from . '(:compilation))
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode))

(leaf treesit
  :if (version<= "29" emacs-version) ; Emacs29以降
  :ensure treesit-auto
  :global-minor-mode global-treesit-auto-mode
  :custom
  (treesit-font-lock-level . 4)
  (treesit-auto-install . 'prompt))

(leaf all-the-icons
  :init
  (leaf all-the-icons :ensure t
    :req "初回に`M-x all-the-icons-install-fonts'を実行する")

  (leaf all-the-icons-dired :ensure t
    :diminish all-the-icons-dired-mode
    :hook (dired-mode-hook . all-the-icons-dired-mode))

  (leaf all-the-icons-ibuffer :ensure t
    :doc "sizeの-h化も一緒にされる"
    :after ibuffer
    :global-minor-mode all-the-icons-ibuffer-mode)

  (leaf all-the-icons-ivy-rich :ensure t
    :global-minor-mode all-the-icons-ivy-rich-mode))

(leaf centaur-tabs :ensure t
  :global-minor-mode centaur-tabs-mode
  :defun
  centaur-tabs-headline-match
  centaur-tabs-enable-buffer-reordering
  centaur-tabs-change-fonts
  centaur-tabs-group-by-projectile-project
  centaur-tabs-get-group-name
  :defvar centaur-tabs-icon-scale-factor
  :config
  (setq centaur-tabs-icon-scale-factor 0.7)
  (centaur-tabs-headline-match)
  (centaur-tabs-enable-buffer-reordering)
  (centaur-tabs-group-by-projectile-project)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode'
`emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
All buffer name start with \" *\" will group to \"*Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs")
      ((string-equal " *" (substring (buffer-name) 0 2)) "*Emacs")
      ((derived-mode-p 'prog-mode) "Editing")
      ((derived-mode-p 'dired-mode) "Dired")
      (t (centaur-tabs-get-group-name (current-buffer))))))
  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))
       ;; Buffer name not match below blacklist.
       (and (string-prefix-p "*" name)
            (not (string= "*scratch*" name)))
       )))
  :custom
  ;; (centaur-tabs-style . "rounded")
  (centaur-tabs-set-icons . t)
  (centaur-tabs-set-close-button . nil)
  (centaur-tabs-cycle-scope . 'tabs)
  ;; (centaur-tabs-label-fixed-length . 10)
  (centaur-tabs-show-count . t)
  (centaur-tabs-show-new-tab-button . nil)
  :custom-face
  (tab-line . '((t (:background "white"))))
  (centaur-tabs-default . '((t (:foreground "black" :background "gray90"))))
  (centaur-tabs-selected . '((t (:inherit region))))
  (centaur-tabs-selected-modified . '((t (:inherit region :foreground "red"))))
  (centaur-tabs-unselected . '((t (:inherit highlight))))
  (centaur-tabs-unselected-modified
   . '((t (:inherit highlight :foreground "red"))))
  :hook
  ;; centaur-tabsを無効とする対象をHookで指定する
  (package-menu-mode-hook . centaur-tabs-local-mode)
  :bind (("C-<tab>" . centaur-tabs-forward)
         ("C-<iso-lefttab>" . centaur-tabs-backward)
         ("C-c C-<tab>" . centaur-tabs-counsel-switch-group)))

;;; OTHER

(leaf popper :ensure t
  :global-minor-mode popper-mode popper-echo-mode
  :custom
  (popper-reference-buffers . '(help-mode
                                helpful-mode
                                "\\*quickrun\\*"
                                inf-ruby-mode
                                inferior-sml-mode
                                "\\*Google Translate\\*"
                                ;; 全completionを対象
                                completion-list-mode
                                compilation-mode
                                ("\\*Warnings\\*" . hide)
                                (" \\*auto-async-byte-compile\\*" . hide)
                                ("\\*Compile-Log\\*" . hide)
                                " \\*undo-tree\\*"
                                "\\*robe-doc\\*"
                                "^ \\*Rubocop.*\\*$"
                                "\\*xref\\*"
                                "\\*Backtrace\\*"
                                "\\*ripgrep-search\\*"
                                ;; lsp
                                "\\*Call Hierarchy\\*"
                                "\\*LSP Error List\\*"
                                ))
  (popper-group-function . 'popper-group-by-projectile)
  ;; popper-echoでk/^コマンドを有効化
  (popper-echo-dispatch-actions . t)
  (popper-echo-dispatch-keys . '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
  :bind* ("M-o" . popper-toggle))

(leaf projectile
  :init
  (leaf projectile :ensure t
    :global-minor-mode projectile-mode
    :diminish projectile-mode
    :bind (:projectile-mode-map
           ("C-c C-f" . projectile-find-file)
           ("C-c b" . projectile-switch-to-buffer)
           ("C-c C-x k" . projectile-kill-buffers))
    :custom
    (projectile-completion-system . 'ivy)
    (projectile-globally-ignored-directories
     . '(".yarn" ".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg"
         ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".pijul" ".tox" ".svn"
         ".stack-work" ".ccls-cache" ".cache" ".clangd")))

  (leaf projectile-ripgrep :ensure t ripgrep wgrep
    :doc "wgrepはripgrep検索結果をC-cC-pで編集可にする"
    :bind (("C-c f" . ripgrep-regexp)
           (:projectile-mode-map
            :package projectile
            ("C-c f" . projectile-ripgrep))))

  (leaf projectile-rails :ensure t
    :global-minor-mode projectile-rails-global-mode
    :diminish projectile-rails-mode))

(leaf google-translate
  :init
  (leaf google-translate :ensure t)

  (leaf google-translate-smooth-ui
    :defvar google-translate-translation-directions-alist
    :defer-config
    (setq google-translate-translation-directions-alist
          '(("en" . "ja") ("ja" . "en")))
    :bind ("C-c C-t" . google-translate-smooth-translate)))

(leaf twittering-mode :ensure t
  :req "aptでgnupgを入れておく"
  :req "alpaca.elが必要"
  :defun twittering-icon-mode
  :commands twit
  :custom
  ;; use master passworad compressed by GnuPG
  (twittering-use-master-password . t)
  (twittering-private-info-file . "~/.emacs.d/twittering-mode.gpg")
  (twittering-use-icon-storage . t)
  (twittering-icon-storage-file . "~/.emacs.d/icons.gz")
  `(twittering-status-format
    . ,(concat
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
  :defer-config
  (twittering-icon-mode t)              ; use icon
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

(leaf shell
  :init
  (leaf company-shell :ensure t
    :after sh-mode
    :config
    (add-to-list 'company-backends
                 '(company-shell
                   company-shell-env
                   :with company-dabbrev-code company-files)))

  (leaf ansi-color
    :doc "コマンドラインと同じ色付けを使う"
    :commands ansi-color-for-comint-mode-on
    :hook (shell-mode-hook . ansi-color-for-comint-mode-on))

  (leaf sh-script
    :mode (("Procfile" . sh-mode)
           ("dotenv" . sh-mode))
    :defer-config
    (unbind-key "C-c C-d" sh-mode-map)
    :hook (sh-mode-hook
           . (lambda () (setq-local flycheck-checker 'sh-posix-bash)))
    :bind (:sh-mode-map ("C-c C-p" . sh-cd-here)))

  (leaf shell
    :custom
    ;; Emacsを起動したshellを使用する（bashからの起動を前提）
    ;; TODO: バイトコンパイル時でなく起動時に評価するよう変更する
    `(explicit-shell-file-name . ,(getenv "SHELL"))
    ;; (explicit-shell-file-name . my-shell-file-name)
    (explicit-bash-args . '("--login" "-i"))
    ;; shell-modeでのファイル名補完
    (shell-file-name-chars . "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")))
;; :hook (shell-mode-hook . (lambda ()
;;                            ;; SHELL で ^M が付く場合は ^M を削除する
;;                            (set-process-coding-system
;;                             'undecided-dos 'sjis-unix)))

(leaf image-dired+ :ensure t
  :doc "非同期でimage-diredを動作させ、大量画像でフリーズしないようにするパッケージ"
  :req "ImageMagickをaptでいれておく"
  ;; BUG: ディレクトリを開く初回時にサムネイル作成に失敗する。
  ;;      diredバッファでimage-dired-create-thumbsを実行して手動でサムネイル
  ;;      を作ると、image-diredが問題なく動くようになる。
  ;;      --no-initを使って、image-dired+だけで動かすと問題は起こらない。
  ;;      何らかの自分のinitファイルが問題を引き起こしている。
  ;;      Error-log
  ;;      image-diredx--invoke-process: Wrong type argument: processp, [nil 23723 12045 294055 nil image-dired-thumb-queue-run nil nil 600000]
  :if (executable-find "convert")
  :commands image-dired
  :defer-config
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
         (:image-dired-image-mode-map
          :package image-dired
          ("f" . image-transform-reset-to-original)
          ("0" . image-mode-fit-frame))))

(leaf swiper
  :init
  (leaf ivy :ensure t
    :defvar ivy-height-alist ivy-initial-inputs-alist
    :custom
    (ivy-count-format . "(%d/%d) ")
    (ivy-extra-directories . '("./"))
    (ivy-initial-inputs-alist . nil) ;; 先頭の^をやめる
    (ivy-format-functions-alist . '((t . ivy-format-function-line)))
    ;; 開いていないファイルをswitch-bufferに含める
    (ivy-use-virtual-buffers . t))

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
    :custom
    ;; dotファイルとコンパイルファイルなどを無視する
    ;; .キーを押せばdotスタートファイルは表示される
    `(counsel-find-file-ignore-regexp
      . ,(concat "\\(\\`\\.\\)\\|"
                 (regexp-opt completion-ignored-extensions)))
    (counsel-mark-ring-sort-selections . nil)
    :custom
    (counsel-switch-buffer-preview-virtual-buffers . nil)
    :defer-config
    ;; counsel-yank-popの高さをデフォルト5から10に拡大する
    (setq ivy-height-alist
          (cons '(counsel-yank-pop . 10)
                (assq-delete-all 'counsel-yank-pop ivy-height-alist)))
    :bind* (("M-x" . counsel-M-x)
            ("M-r" . counsel-command-history)
            ("C-x f" . counsel-recentf)
            ("C-c C-x g" . counsel-git-grep)
            ("C-x b" . counsel-switch-buffer)
            ("C-M-y" . counsel-yank-pop)
            ("C-c C-SPC" . counsel-mark-ring))
    :bind (("C-x C-f" . counsel-find-file) ;; ibufferで上書きがある
           ("<f1> f" . counsel-describe-function)
           ("<f1> v" . counsel-describe-variable)
           ("C-c d" . counsel-describe-symbol)
           (:counsel-find-file-map
            ("<C-return>" . ivy-immediate-done) ;; C-M-jをよく忘れるので
            ("C-c <C-return>" . ivy-immediate-done)
            ("^" . counsel-up-directory))))

  (leaf ivy-rich :ensure t
    :global-minor-mode ivy-rich-mode
    :custom
    (ivy-rich-path-style . 'abbrev))

  (leaf s :ensure t)

  (leaf migemo :ensure t :require t
    :req "cmigemoをいれておく"
    :url "https://github.com/koron/cmigemo"
    :defun migemo-init migemo-get-pattern
    :defvar ivy-re-builders-alist
    :after swiper
    :if (executable-find "cmigemo")
    :custom
    (migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict")
    :defer-config
    (eval-and-compile (require 's))
    ;; swiperでもmigemoを使う
    ;; 参考: https://www.yewton.net/2020/05/21/migemo-ivy/
    (defun my-ivy-migemo-re-builder (str)
      (let* ((sep " \\|\\^\\|\\.\\|\\*")
             (splitted
              (--map (s-join "" it)
                     (--partition-by (s-matches-p " \\|\\^\\|\\.\\|\\*" it)
                                     (s-split "" str t)))))
        (s-join "" (--map (cond ((s-equals? it " ") ".*?")
                                ((s-matches? sep it) it)
                                (t (migemo-get-pattern it)))
                          splitted))))
    (migemo-init)
    (setq ivy-re-builders-alist '((t . ivy--regex-plus)
                                  (swiper . my-ivy-migemo-re-builder))))

  (leaf counsel-fd :ensure t
    :if (executable-find "fd")
    :bind ("C-c C-f" . counsel-fd-file-jump))

  (leaf helpful :ensure t
    :bind* ("<f1> k" . helpful-key)
    :bind ("C-c C-d" . helpful-at-point)
    :custom
    ((counsel-describe-function-function . 'helpful-callable)
     (counsel-describe-variable-function . 'helpful-variable)))

  (leaf ivy-prescient :ensure t
    :doc "MEMO: ivy-***-alistを書き換える中で最後に来ないと動かないことがある"
    :global-minor-mode ivy-prescient-mode))

(leaf smart-jump
  :req "ripgrepをpcre2サポートありでインストールしておく"
  :req "cargo install ripgrep --features 'pcre2'"
  :doc "またはデフォルトのripgrepを使う場合は、"
  :doc "custom値を設定してpcre2を使わないようにする"
  :doc ":custom (dumb-jump-rg-search-args . \"\")"
  :ensure t dumb-jump rg ivy-xref
  :defvar dumb-jump-find-rules
  :defun smart-jump-simple-find-references smart-jump-find-references-with-rg
  :custom
  ;; ripgrepを使う
  (smart-jump-find-references-fallback-function
   . #'smart-jump-find-references-with-rg)
  ;; xrefをivyで選択表示する
  (xref-show-definitions-function . #'ivy-xref-show-defs)
  (xref-show-xrefs-function . #'ivy-xref-show-xrefs)
  ;; legacyなdumb-jump-goなどをivyにする
  (dumb-jump-selector . 'ivy)
  ;; xrefをdumb-jumpで行うhook
  ;; :hook (xref-backend-functions . dumb-jump-xref-activate)
  :defer-config
  ;; dump-jump対応言語はすべて登録する
  (require 'dumb-jump)
  (let* ((languages-dup (--map (plist-get it :language) dumb-jump-find-rules))
         (languages (delete-dups languages-dup))
         (modes-str (--map (concat it "-mode") languages))
         (modes (mapcar 'intern modes-str)))
    (smart-jump-register :modes modes
                         :jump-fn #'dumb-jump-go
                         :pop-fn #'dumb-jump-back
                         :refs-fn #'smart-jump-simple-find-references
                         :heuristic 'point
                         :order 100))
  (smart-jump-register :modes '(ruby-mode typescript-mode js-mode)
                       :jump-fn #'lsp-find-definition
                       :refs-fn #'lsp-find-references
                       :heuristic 'point
                       :async 300       ; サーバとの通信のため300msまで待つ
                       :order 1)
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("M-/" . smart-jump-references)))

(leaf expand-region :ensure t
  :bind ("C-`" . er/expand-region))

(leaf which-key :ensure t
  :global-minor-mode which-key-mode
  :diminish which-key-mode
  :custom
  (which-key-side-window-max-height . 0.4)
  :config
  (which-key-setup-side-window-bottom))

(leaf sudo-edit :ensure t)

(leaf visual-regexp :ensure t
  :bind ("M-&" . vr/query-replace))

(leaf async :ensure t
  :hook (emacs-lisp-mode-hook . async-bytecomp-package-mode))

(leaf undo-tree :ensure t
  :bind ("C-c C-/" . undo-tree-visualize))

(leaf persp-mode
  :init
  (leaf persp-mode :ensure t
    :global-minor-mode persp-mode
    :defun get-current-persp persp-contain-buffer-p
    :defvar ivy-sort-functions-alist
    :bind* (("M-<right>" . persp-prev)
            ("M-<left>" . persp-next))
    :custom
    (persp-save-dir . "~/.emacs.d/.persp-confs/")
    (persp-auto-save-persps-to-their-file-before-kill . t)
    (persp-auto-save-num-of-backups . 10)
    (persp-kill-foreign-buffer-behaviour . nil)
    `(persp-keymap-prefix . ,(kbd "C-x x"))
    (persp-add-buffer-on-after-change-major-mode . t)
    :config
    ;; counsel-switch-bufferで現ワークスペースのbufferのみを選択肢とする
    ;; 参考：https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-ivy-el
    (eval-after-load 'ivy
      '(progn
         (add-hook 'ivy-ignore-buffers
                   #'(lambda (b)
                       (when persp-mode
                         (let ((persp (get-current-persp)))
                           (if persp
                               (not (persp-contain-buffer-p b persp))
                             nil)))))
         (setq ivy-sort-functions-alist
               (append ivy-sort-functions-alist
                       '((persp-kill-buffer   . nil)
                         (persp-remove-buffer . nil)
                         (persp-add-buffer    . nil)
                         (persp-switch        . nil)
                         (persp-window-switch . nil)
                         (persp-frame-switch  . nil))))))))

(leaf theme
  :init
  (leaf rebecca-theme :ensure t :disabled t)

  (leaf solo-jazz-theme :disabled t
    :ensure t :require t
    :config
    (load-theme 'solo-jazz t))

  (leaf humanoid-themes :ensure t :disabled t)

  (leaf github-modern-theme :ensure t :disabled t))

(leaf mozc
  :init
  (leaf mozc :ensure t
    :req "予め${HOME}/bin/mozc_emacs_helperを用意するか、"
    :req "aptでemacs-mozc-binを入れておく。"
    :url "https://w.atwiki.jp/ntemacs/pages/61.html"
    :url "https://github.com/smzht/mozc_emacs_helper"
    :defun mozc-session-sendkey
    :if (executable-find "mozc_emacs_helper")
    ;; mozcモードで一部キーバインドが外れるので再設定
    :bind* ("C-\\" . mozc-mode)
    :bind (:mozc-mode-map
           ("C-x C-s" . save-buffer)
           ("C-x h" . mark-hole-buffer))
    :custom
    (default-input-method . "japanese-mozc")
    (mozc-leim-title . "[も]")
    ;; WindowsのGoogle日本語入力を使う
    :advice (:after mozc-session-execute-command
                    (lambda (&rest args)
                      (when (eq (nth 0 args) 'CreateSession)
                        (mozc-session-sendkey '(Hankaku/Zenkaku)))))
    :defer-config
    (set-language-environment "Japanese")
    (prefer-coding-system 'utf-8))

  (leaf mozc-popup :ensure t :require t
    :after mozc
    :custom
    (mozc-candidate-style . 'popup)))

(leaf keyfreq :ensure t
  :global-minor-mode keyfreq-mode keyfreq-autosave-mode)

(leaf editorconfig :ensure t
  :doc "projectの.editorconfigファイルを読み込む"
  :diminish t
  :global-minor-mode editorconfig-mode)

(leaf imenu
  :init
  (leaf imenu-list :ensure t
    :bind ("C->" . imenu-list-smart-toggle)
    :custom
    (imenu-list-focus-after-activation . t))

  (leaf imenu-anywhere :ensure t
    :bind ("C-." . ivy-imenu-anywhere)))

(leaf buffer-move :ensure t
  :bind* (("C-S-h" . buf-move-left)
          ("C-S-j" . buf-move-down)
          ("C-S-k" . buf-move-up)
          ("C-S-l" . buf-move-right)))

(leaf flyspell
  :init
  (leaf flyspell
    :req "aptでaspell-enをいれておく"
    :diminish flyspell-mode
    ;; :hook (text-mode-hook . flyspell-mode)
    :custom
    (ispell-local-dictionary . "en_US")
    :defer-config
    ;; 日本語まじりをチェック対象外にする
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

  (leaf flyspell-popup :ensure t
    :hook (flyspell-mode-hook . flyspell-popup-auto-correct-mode)))

(leaf aggressive-indent :ensure t
  :diminish aggressive-indent-mode
  :global-minor-mode global-aggressive-indent-mode
  :defvar aggressive-indent-excluded-modes
  ;; まずい動きをするときはに除外モードを追加する
  :config
  ;; defcustomだがデフォルト値に追加の形で書く
  (add-to-list 'aggressive-indent-excluded-modes 'web-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'dockerfile-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'js-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'typescript-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'compilation-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'inferior-sml-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'shell-mode))

(leaf git-timemachine :ensure t
  :bind ("C-c C-x t" . git-timemachine))

(leaf vc-msg :ensure t
  :bind ("C-c C-x b" . vc-msg-show))

(leaf ace-window :ensure t
  :custom
  ;; 参考: https://github.com/abo-abo/ace-window/wiki/display-buffer
  (aw-keys . '(?h ?j ?k ?l ?a ?s ?d ?f ?g))
  (display-buffer-base-action
   . '((display-buffer-reuse-window ace-display-buffer))))

(leaf goto-line-preview :ensure t
  :bind* ("M-g M-g" . goto-line-preview))

(leaf demap :ensure t
  :bind* ("M-m" . demap-toggle))

;;; Emacs default (not package.el)

(leaf dired
  :init
  (leaf dired
    :defun my-reload-current-dired-buffer
    :custom
    ;; dired-modeがlsコマンドに渡すオプションを設定する
    ;; --time-style=long-iso: 2022-01-01 12:00 形式で日時を表示する
    ;; l: 長い表示、dired-modeに必須のオプション
    ;; g: ユーザ名を非表示
    ;; G: グループ名を非表示
    ;; h: kbyte・Mbyteの使用
    ;; F: ディレクトリに「/」を表示
    ;; A: 「.」と「..」を非表示でドットファイルを表示
    ;; (setq dired-listing-switches "-gGhFA")
    (dired-listing-switches . "--time-style=long-iso -lgGhF")
    :defer-config
    ;; C-.でドットファイルの表示と非表示を切り替える
    (defun my-reload-current-dired-buffer ()
      "Reload current `dired-mode' buffer."
      (let* ((dir (dired-current-directory)))
        (progn (kill-buffer (current-buffer))
               (dired dir))))
    (defun my-toggle-dired-listing-switches ()
      "Toggle `dired-mode' option to show or hide dot files.

Rewrite `dired-listing-switches' variable between with and without 'A'"
      (interactive)
      (progn
        (if (string-match "[Aa]" dired-listing-switches)
            (setq dired-listing-switches "--time-style=long-iso -lgGhF")
          (setq dired-listing-switches "--time-style=long-iso -lgGhFA"))
        (my-reload-current-dired-buffer)))
    :hook
    (dired-mode-hook
     . (lambda ()
         (setq-local whitespace-style (delete 'lines-tail whitespace-style))
         (setq-local truncate-partial-width-windows t)))
    :bind (:dired-mode-map
           ("C-." . my-toggle-dired-listing-switches)
           ("r" . wdired-change-to-wdired-mode)))

  (leaf dired-collapse :ensure t
    :hook (dired-mode-hook . dired-collapse-mode))

  (leaf dired-sort-map :require t
    :after dired)

  (leaf dired-single :ensure t
    :bind (:dired-mode-map
           :package dired
           ("C-m" . dired-single-buffer)
           ("^" . dired-single-up-directory)
           ("C-." . my-toggle-dired-listing-switches)
           ("r" . wdired-change-to-wdired-mode))))

(leaf help-mode
  ;; Alt+左右でヘルプの進む・戻るを行う、デフォルトはl/r
  :bind (:help-mode-map
         ("M-<left>" . help-go-back)
         ("M-<right>". help-go-forward)))

(leaf ibuffer
  :defun ibuffer-current-buffer
  :defer-config
  ;; ibuffer選択肢を考慮したibuffer-find-file関数を、counselで実現する
  (defun my-counsel-ibuffer-find-file ()
    "Like `counsel-find-file', starting with directory of ibuffer candidate."
    (interactive)
    (let ((default-directory
            (let ((buf (ibuffer-current-buffer)))
              (if (buffer-live-p buf)
                  (with-current-buffer buf
                    default-directory)
                default-directory))))
      (counsel-find-file default-directory)))
  :bind* ("C-x C-b" . ibuffer)
  :bind (:ibuffer-mode-map ("C-x C-f" . my-counsel-ibuffer-find-file)))

(leaf winner
  :global-minor-mode winner-mode
  :bind (("C-M-<left>" . winner-undo)
         ("C-M-<right>" . winner-redo)))

(leaf line-number
  :global-minor-mode column-number-mode
  :init
  (line-number-mode -1)

  (leaf display-line-numbers
    :if (version<= "26" emacs-version) ; Emacs26以降
    :global-minor-mode global-display-line-numbers-mode)

  (leaf linum
    :if (version< emacs-version "26") ; Emacs25以下
    :defvar linum-format
    :global-minor-mode global-linum-mode
    :init
    (setq linum-format "%4d "))

  ;; 列番号を1オリジンで表示する
  :custom (mode-line-position-column-format . '(" %C")))

(leaf autoinsert
  :global-minor-mode auto-insert-mode
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
  (defun my-replace-template ()
    "Add template string to file."
    (mapc (lambda (template-replacement)
            (goto-char (point-min))
            (while (search-forward (car template-replacement) nil t)
              (replace-match (funcall (cdr template-replacement)))))
          template-replacements-alists)
    (goto-char (point-min))
    (message "done."))
  :config
  (setq auto-insert-directory "~/.emacs.d/autoinsert/")
  (setq auto-insert-alist
        (nconc
         '(("Test\\.\\(cpp\\|cc\\|cxx\\)$" .
            ["templateTest.cpp" my-replace-template])
           ("\\.\\(cpp\\|cc\\|cxx\\)$" . ["template.cpp" my-replace-template])
           ("\\.\\(hpp\\|hh\\|hxx\\)$" . ["template.hpp" my-replace-template])
           ("\\.c$" . ["template.c" my-replace-template])
           ("\\.ino$" . ["template.ino" my-replace-template]))
         auto-insert-alist)))

(leaf recentf
  :init
  (leaf recentf
    :doc "最近使ったファイルを.recentfファイルに保存する"
    :doc "counsel-recentfで呼び出せる"
    :global-minor-mode recentf-mode
    :custom
    (recentf-max-saved-items . 1000)
    (recentf-auto-cleanup . 'never))

  (leaf recentf-ext :ensure t :require t
    :after recentf))

(leaf subword
  :global-minor-mode global-subword-mode
  :diminish subword-mode)

(leaf windmove
  :config
  ;; Shift + カーソル で分割ウィンドウ間を移動
  (windmove-default-keybindings)
  :custom
  ;; 画面外への移動はサイクルする
  (windmove-wrap-around . t)
  ;; C-x oの代わりのバッファ移動
  :bind* (("C-c l" . windmove-right)
          ("C-c h" . windmove-left)
          ("C-c j" . windmove-down)
          ("C-c k" . windmove-up)
          ;; カーソルのみで分割ウィンドウ間を移動
          ;; ("<left>" . windmove-right)
          ;; ("<right>" . windmove-left)
          ;; ("<down>" . windmove-down)
          ;; ("<up>" . windmove-up)
          ))

(leaf indent
  :init
  ;; Cソースコードの変数だが、indentにまとめておく
  (setq-default tab-width 2)
  (setq-default indent-tabs-mode nil)
  :custom
  (standard-indent . 2))

(leaf newcomment
  :bind* ("C-;" . comment-line))

(leaf saveplace
  :global-minor-mode save-place-mode)

(leaf autorevert
  :global-minor-mode global-auto-revert-mode
  :custom
  (auto-revert-interval . 5)
  (auto-revert-check-vc-info . t))

(leaf sticky-buffer
  :bind*
  ("C-c C-s" . sticky-buffer-mode))

(leaf files
  :custom
  (confirm-kill-emacs . 'y-or-n-p)
  ;; wl-copyのキルを確認せずに終了する
  (confirm-kill-processes . nil))

(leaf window
  :defun split-windown-right split-windown-below
  :preface
  (defun my-split-window-right-and-switch ()
    "Split the window right, then switch to the new window."
    (interactive)
    (split-window-right)
    (other-window 1))
  (defun my-split-window-below-and-switch ()
    "Split the window right, then switch to the new window."
    (interactive)
    (split-window-below)
    (other-window 1))
  :bind* (("C-c 2" . my-split-window-below-and-switch)
          ("C-c 3" . my-split-window-right-and-switch)))

(leaf browse-url
  :doc "WSLでのブラウザ設定"
  :req "aptでubuntu-wslをいれておく"
  :if (getenv "WSLENV") (executable-find "wslview")
  :custom
  (browse-url-browser-function . #'browse-url-generic)
  (browse-url-generic-program . "wslview"))

(leaf clipboard
  :doc "emacs29でクリップボードが文字化けする問題を対処"
  :doc "credit: yorickvP on Github"
  :req "wl-clipboardをインストールしておく"
  :req "sudo apt install wl-clipboard"
  :url "https://zenn.dev/ignorant/scraps/4456a9fb017eb3"
  :url "https://www.emacswiki.org/emacs/CopyAndPaste#h5o-4"
  :if (version<= "29" emacs-version) ; Emacs29以降
  :if (and (executable-find "wl-copy") (executable-find "wl-paste"))
  :defvar wl-copy-process
  :init
  (setq wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))

(leaf pixel-scroll
  :doc "スクロールをなめらかにするグローバルマイナーモード"
  :if (version<= "29" emacs-version) ; Emacs29以降
  :config
  (pixel-scroll-precision-mode))

;;; init_package.el ends here
