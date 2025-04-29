;;; init_package --- settings about packages -*- lexical-binding: t; -*-

;;; Commentary:
;; This is settings about package.
;; It is written using `leaf.el`.
;; See also ../themes/original-theme.el for settings of faces.

;;; Code:

;; byte compileの警告をしない
;; cl: cl-libに移行すれば消せるが、依存先すべての対応が不可能
(with-no-warnings (require 'cl))
;; autload: Emacs29でautoloadも警告になる、議論中っぽい？
(with-no-warnings (require 'autoload))

(require 'package)
;; パッケージアーカイブの順番は関係ない。
;; 優先度はバージョン番号が大きい方が優先されるため、
;; melpaが常にmarmaladeよりも優先される。
;; melpaよりもmelpa-stableを優先するなどの別途優先度をつけるには、
;; `package-archive-priorities'を使って設定する。
(add-to-list 'package-archives ; MELPAを追加
             '("melpa" . "https://melpa.org/packages/"))
(eval-and-compile (package-initialize))

(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

(eval-and-compile
  (leaf leaf-keywords
    ;; :diminishを有効にし、モードラインをスッキリさせる
    :ensure t diminish smartrep
    :config
    (leaf diminish :ensure t)
    (leaf-keywords-init)))

(leaf leaf
  :bind
  ;; MEMO: lisp-shared-mode-mapだと既存でC-cC-eに割り当てられた
  ;;       elisp-eval-region-or-bufferを上書きできない
  (:emacs-lisp-mode-map :package elisp-mode ("C-c C-e" . leaf-expand))
  (:lisp-interaction-mode-map :package elisp-mode ("C-c C-e" . leaf-expand)))

(leaf bind-key :ensure t :require t)

(leaf util :defun call-with-region-or-line) ; dummy, init_util.el

(leaf initchart
  :vc (:url "https://github.com/yuttie/initchart.git"))

;;; Vertico

;; FIXME: 初回起動時にpackageがない状況ではrequireがエラーする。
;;        マクロを使うため、強制インストールする。
(when (not (package-installed-p 'consult))
  (package-install 'consult))
(when (not (package-installed-p 'orderless))
  (package-install 'orderless))
(leaf vertico
  :leaf-path nil
  :preface
  (leaf vertico
    :doc "コンプリージョンをまとめてモダンにする"
    :ensure t
    :global-minor-mode t
    :custom
    ;; verticoの最上部と最下部のサイクルをオンにする
    ;; consult-lineにおいて、開始位置よりも前行の検索結果は最下部となる。
    ;; これをswiperの直感どおりに検索するためにこれをオンにする。
    ;;
    ;; ex. 3行目で検索開始
    ;;   3 ghi <= start
    ;;   4 jkl
    ;;   1 abc
    ;;   2 def
    ;;
    ;; 他解法に(consult-line-start-from-top . t)がある
    ;; これは表示結果がつねに行番号順に並ぶ。
    ;; 検索結果一覧の見やすさは優れる。
    ;; しかし、検索開始時にカーソルが先頭行に固定される。
    ;; このときどの行で検索開始してもプレビューが先頭行になる。
    ;; これが見づらいため採用しない。
    ;;
    ;; ex. 3行目で検索開始
    ;;   1 abc <= start
    ;;   2 def
    ;;   3 ghi
    ;;   4 jkl
    ;;
    ;; なお、consultレポジトリにて何度か議論されPRもレジェクトされている。
    ;; consult的には利便性よりも簡素さを優先する、
    ;; またパッチされるとすればverticoへのほうが適切、のようだ。
    ;; https://github.com/minad/consult/issues/795#issuecomment-1528030324
    (vertico-cycle . t)
    (vertico-count . 15))

  (leaf vertico-directory
    :doc "find-fileにおけるBackspaceをディレクトリ単位で削除にする"
    :bind
    (:vertico-map
     :package vertico
     ("<backspace>" . vertico-directory-delete-char)))

  ;; なぜかconsult-narrowでエラーがでる
  ;; なぜか:defunとrequireの2つで消せる
  ;;   the function 'consult-narrow' might not be defined at runtime.
  (eval-when-compile (require 'consult))
  (leaf consult
    :doc "便利コマンドを提供する"
    :ensure t
    :defvar consult-ripgrep-args consult--narrow-keys
    :defun consult-narrow
    :config
    ;; 隠しファイル込みでのconsult-ripgrep
    (defun consult-ripgrep-including-hidden (&optional DIR INITIAL)
      "Search with rg for files including hidden ones in DIR with INITIAL input"
      (interactive "P")
      (let ((consult-ripgrep-args (concat consult-ripgrep-args " --hidden")))
        (consult-ripgrep DIR INITIAL)))
    ;; consult-lineにおいてC-sC-sで最後の検索を再検索
    ;; isearchやswiperでの手癖に対応する
    ;;   https://github.com/minad/consult/wiki#add-command-local-keybinding
    (defvar my/consult-line-map
      (let ((map (make-sparse-keymap)))
        (bind-key "C-s" #'previous-history-element map)
        map))
    (eval-when-compile (require 'consult))
    (consult-customize consult-line :keymap my/consult-line-map)
    ;; C-<left>/C-<right>でconsult対象グループを変更する
    ;; 例えばswitch-bufferではファイル、プロジェクト、hiddenバッファと切り替わる
    ;; https://github.com/minad/consult/wiki#cycle-through-narrowing-keys
    (defun consult-narrow-cycle-backward ()
      "Cycle backward through the narrowing keys."
      (interactive)
      (when consult--narrow-keys
        (consult-narrow
         (if consult--narrow
             (let ((idx (seq-position
                         consult--narrow-keys
                         (assq consult--narrow consult--narrow-keys))))
               (unless (eq idx 0)
                 (car (nth (1- idx) consult--narrow-keys))))
           (caar (last consult--narrow-keys))))))
    (defun consult-narrow-cycle-forward ()
      "Cycle forward through the narrowing keys."
      (interactive)
      (when consult--narrow-keys
        (consult-narrow
         (if consult--narrow
             (let ((idx (seq-position
                         consult--narrow-keys
                         (assq consult--narrow consult--narrow-keys))))
               (unless (eq idx (1- (length consult--narrow-keys)))
                 (car (nth (1+ idx) consult--narrow-keys))))
           (caar consult--narrow-keys)))))
    ;; カーソル位置のワードを検索する
    ;; https://github.com/minad/consult/wiki#start-consult-line-search-with-symbol-at-point
    (defun consult-line-symbol-at-point ()
      "Search for a matching line with a symbol found near point."
      (interactive)
      (consult-line (thing-at-point 'symbol)))
    :bind
    (:consult-narrow-map
     ("M-<left>" . consult-narrow-cycle-backward)
     ("M-<right>" . consult-narrow-cycle-forward))
    ("C-s"   . consult-line)
    ("M-s s" . consult-line)
    ("C-S-s" . consult-line-symbol-at-point)
    ("M-s w" . consult-line-symbol-at-point)
    ("C-M-s" . consult-line-multi)
    ("M-s S" . consult-line-multi)
    ("C-c f" . consult-ripgrep)
    ("M-s C" . consult-ripgrep)
    ("M-s c" . consult-ripgrep-including-hidden)
    ("C-c g" . consult-git-grep)
    ("M-s g" . consult-git-grep)
    ("M-s f" . consult-fd)
    ("C-x b" . consult-buffer)
    ("C-x f" . consult-recent-file)
    ("M-g M-g" . consult-goto-line)
    ("M-g SPC" . consult-mark)
    ("C-c C-SPC" . consult-mark)
    ("C-M-y" . consult-yank-pop)
    ("C-." . consult-imenu)
    ("C-c C-." . consult-outline)
    ("M-r" . consult-complex-command))

  (leaf migemo
    :req "cmigemoをいれておく"
    :url "https://github.com/koron/cmigemo"
    :ensure t
    :defun migemo-init
    :if (executable-find "cmigemo")
    ;; TODO: 起動直後にfind-fileするとmigemoが読み込まれていないため
    ;;       fileで指定しているorderless+migemoがエラーする
    ;;       switch-bufferやconsult-line後ならmigemoが読み込まれるため大丈夫
    ;;       :after verticoで修正可能だが、
    ;;       migemoとその関連で総起動時間が100msほど起動時間が変わるので悩みどころ
    :after consult
    :require t
    :custom
    (migemo-user-dictionary . nil)
    (migemo-regex-dictionary . nil)
    (migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict")
    :config (migemo-init))

  (leaf orderless
    :doc "保管候補を順番関係なし、空白区切りで複数検索可能にする"
    :doc "migemo化の参考"
    :doc "  https://nyoho.jp/diary/?date=20210615"
    :doc "orderlessのドキュメント"
    :doc "  https://github.com/oantolin/orderless#defining-custom-orderless-styles"
    :defun (migemo-get-pattern . migemo)
    :ensure t
    :config
    (defun orderless-migemo (component)
      (let ((pattern (migemo-get-pattern component)))
        (condition-case nil
            (progn (string-match-p pattern "") pattern)
          (invalid-regexp nil))))
    (eval-when-compile (require 'orderless))
    (orderless-define-completion-style orderless+migemo
      (orderless-matching-styles
       '(orderless-literal
         orderless-regexp
         orderless-initialism
         orderless-migemo)))
    :custom
    (completion-styles . '(orderless))
    (orderless-matching-styles
     . '(orderless-literal
         orderless-regexp
         orderless-initialism))
    ;; カテゴリによってcompletion-stylesを変更する
    ;; 利用できるcategoryはEmacs28移行で定義されている
    ;; consult.el内を:categoryタグで検索するとよい
    (completion-category-overrides
     . '((file (styles orderless+migemo partial-completion))
         (buffer (styles orderless+migemo))
         (unicode-name (styles orderless+migemo))
         (kill-ring (styles orderless+migemo))
         ;; consult with migemo
         (consult-location (styles orderless+migemo)) ; consult-line
         (consult-multi (styles orderless+migemo))    ; consult-buffer
         )))

  (leaf marginalia
    :doc "候補リストを彩る"
    :ensure t
    :defvar marginalia-command-categories
    :global-minor-mode t
    :config
    ;; projectileにカテゴリを与えて彩る
    (setq marginalia-command-categories
          (append '((projectile-switch-to-buffer . buffer)
                    (projectile-find-file . project-file)
                    (projectile-find-dir . project-file)
                    (projectile-switch-project . file))
                  marginalia-command-categories)))

  (leaf nerd-icons-completion
    :ensure t
    :doc "コンプリージョンリストにアイコンをつける"
    :global-minor-mode t
    :hook (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

  (leaf embark
    :ensure t
    :defvar embark-indicators
    :defer-config
    (autoload 'which-key--hide-popup-ignore-command "which-key")
    (autoload 'which-key--show-keymap "which-key")
    :custom (embark-help-key . "?")
    :bind* ("M-C-x" . embark-bindings)
    :bind ("<mouse-3>" . embark-act))

  (leaf embark-consult
    :ensure t
    :hook (embark-collect-mode . consult-preview-at-point-mode))

  (leaf embark-which-key
    :doc "embark wikiより"
    :doc "embarkのコンプリージョンリストをwhich-keyでだす"
    :doc "https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt"
    :leaf-path nil
    :defun
    embark-which-key-indicator
    (embark--truncate-target . embark)
    (which-key--hide-popup-ignore-command . which-key)
    (which-key--show-keymap . which-key)
    :after embark which-key
    :init
    (defun embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))
    (defun embark-hide-which-key-indicator (fn &rest args)
      "Hide the which-key indicator when using the completing-read."
      (which-key--hide-popup-ignore-command)
      (let ((embark-indicators
             (remq #'embark-which-key-indicator embark-indicators)))
        (apply fn args)))
    :advice
    (:around embark-completing-read-prompter embark-hide-which-key-indicator)
    :custom
    (embark-indicators
     . '(embark-which-key-indicator
         embark-highlight-indicator
         embark-isearch-highlight-indicator))))

;;; Completion

(leaf corfu
  :leaf-path nil
  :preface
  (leaf hotfuzz
    :ensure t
    :req "GitHubリポジトリをクローンする"
    :req "https://github.com/axelf4/hotfuzz"
    :req "READMEに従いhotfuzz-module.soをビルドする"
    :req "hotfuzz-module.soを.config/emacs/lispに配置する")

  ;; BUG?: consult-lineで2キーを同時押しするとバグが起きる
  ;; 具体的には、consult-lineで検索時に下記エラーがでて、
  ;; プレビューと検索結果へ移動がうまく動かなくなる。
  ;; Error in post-command-hook (consult--preview-post-command-hook): (quit)
  ;; おそらくcompletion-stylesをconsultとcorfuとで設定しているせい。
  ;; 対処のために、vertico系 -> corfuの設定順にする
  (leaf corfu
    :ensure t
    :init
    (setq completion-ignore-case t)
    ;; インデント済みのときTABキーで補完開始
    ;; C-M-iが身についているからいらないかも
    (setq tab-always-indent 'complete)
    (defun my/corfu-mode ()
      "Turn on corfu mode."
      (corfu-mode)
      (corfu-popupinfo-mode)
      (corfu-history-mode))
    :custom
    (corfu-cycle . t)
    (corfu-auto . t)
    (corfu-auto-delay . 0.2)
    ;; corfu中に選択候補をカーソル先に表示しない
    (corfu-preview-current . nil)
    (corfu-auto-prefix . 2)
    (corfu-popupinfo-delay . '(0.3 . 0.3))
    (corfu-popupinfo-max-height . 30)
    :hook
    ;; corfuではhotfuzzでフィルター/ソートする
    (corfu-mode-hook
     . (lambda () (setq-local completion-styles '(hotfuzz))))
    ;; shellではすぐにcorfuしない
    ;; https://github.com/minad/corfu?tab=readme-ov-file#completing-in-the-eshell-or-shell
    ((shell-mode-hook eshell-mode-hook)
     . (lambda () (setq-local corfu-auto nil) (my/corfu-mode)))
    (prog-mode-hook . my/corfu-mode)
    :bind (:corfu-map
           ("C-f" . corfu-insert)
           ("C-c C-d" . corfu-info-documentation)
           ("<tab>" . corfu-next)
           ("<backtab>" . corfu-previous)
           ("RET" . nil)
           ;; 手癖のC-M-i連打で何も起こらないようにする
           ("C-M-i" . corfu-next)
           ("C-s" . corfu-insert-separator)))

  (leaf nerd-icons-corfu
    :ensure t
    :doc "VLゴシックと組み合わせると下記不具合がでる"
    :doc "cape-emojiのプレビューが壊れて____表示になる"
    :doc "corfuの補完候補高さの計算が壊れて、補完候補が多いときに一番下が見切れる"
    :custom
    (corfu-margin-formatters . '(nerd-icons-corfu-formatter)))

  (leaf cape
    :ensure t
    :doc "バックエンド合成やcompanyバックエンドの変換を提供する"
    :init
    ;; デフォルトのelisp-completion-at-pointは関数・変数を区別する
    ;; そのため:hook (corfu-mode-hookのような補完ができない
    ;; そこでEmacs Lispの補完にcape-elisp-symbolを使う
    (defun my/elisp-mode-init ()
      "Set completion function to cape"
      (setq-local completion-at-point-functions
                  (list (cape-capf-inside-code
                         (cape-capf-super #'cape-elisp-symbol
                                          #'cape-dabbrev))
                        (cape-capf-inside-string #'cape-file))))
    :hook (emacs-lisp-mode-hook . my/elisp-mode-init)
    :init
    ;; リスト先頭のほうが優先
    ;; ここでいうと下ほど優先
    ;; (add-to-list 'completion-at-point-functions #'cape-file)
    ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    ;; (add-to-list 'completion-at-point-functions #'cape-history)
    ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
    ;; (add-to-list 'completion-at-point-functions #'cape-tex)
    ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
    ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
    ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
    ;; (add-to-list 'completion-at-point-functions #'cape-dict)
    ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
    ;; (add-to-list 'completion-at-point-functions #'cape-line)
    ;; (add-to-list 'completion-at-point-functions #'cape-emoji)
    :bind
    ("C-c i t" . complete-tag)
    ("C-c i d" . cape-dabbrev)
    ("C-c i h" . cape-history)
    ("C-c i f" . cape-file)
    ("C-c i k" . cape-keyword)
    ("C-c i s" . cape-elisp-symbol)
    ("C-c i e" . cape-elisp-block)
    ("C-c i a" . cape-abbrev)
    ("C-c i l" . cape-line)
    ("C-c i w" . cape-dict)
    ("C-c i :" . cape-emoji)
    ("C-c i x" . cape-tex)
    ("C-c i g" . cape-sgml)
    ("C-c i r" . cape-rfc1345))

  (leaf company
    :ensure t
    :doc "capeで既存のcompany補完も利用できるためいれておく"
    :doc "特に、company-dabbrev-codeが便利"
    :custom
    (company-dabbrev-ignore-case . t)
    (company-dabbrev-code-ignore-case . t)
    (company-etags-ignore-case . t))

  (leaf tempel
    :doc "モダンなsnippet補完"
    :ensure t
    :bind ("C-M-o" . tempel-insert))

  (leaf tempel-collection
    :ensure t
    :after tempel)

  (leaf company-tabnine
    :disabled t
    :ensure t
    :doc "company-tabnine-install-binaryを実行する"
    :require t
    :custom
    (company-tabnine-binaries-folder . "~/.config/tabnine")))

(leaf tabnine
  :req "M-x tabnine-install-binary"
  :req "M-x tabnine-login"
  :doc "現在のtabnine-chatは自動でバッファモードやバッファ内容すべてを送る。"
  :doc "そのためすぐにチャット量の限界を超えたり、"
  :doc "変な英語プロンプトが足され回答が英語になる。"
  :doc "あまり使わず、ellamaやChat-GPTを使うほうがよい。"
  :ensure t
  :commands
  tabnine-start-process
  tabnine-kill-process
  ;; うまくオートロードされてないコマンドたち
  tabnine-chat
  tabnine-chat-explain-code
  tabnine-chat-generate-test-for-code
  tabnine-chat-document-code
  tabnine-chat-fix-code
  :diminish "⌬"
  :custom
  (tabnine-chat-prompt-alist
   . '((explain-code . "このコードを日本語で説明して")
       (generate-test-for-code . "このコードのテストコードを書いて、日本語で説明して")
       (document-code . "このコードのドキュメントを、日本語で生成して")
       (fix-code . "このコードの間違いを指摘して日本語で出力して、また、修正したコードも一緒に出力して")))
  (tabnine-minimum-prefix-length . 0)
  (tabnine-idle-delay . 0.6)
  (tabnine-binaries-folder . "~/.config/tabnine")
  :config
  (tabnine-start-process)
  :hook
  ;; (prog-mode-hook . tabnine-mode)
  ;; (kill-emacs-hook . tabnine-kill-process)
  ;; tabnine-modeが起動していないときにEmacsを終了すると、
  ;; tabnine-kill-processが呼ばれ、そこから:config設定で
  ;; tabnine-start-processが呼び出されてしまうのを対処
  (kill-emacs-hook
   . (lambda () (when (boundp 'tabnine--process) (tabnine-kill-process))))
  :bind
  (:tabnine-completion-map
   :package tabnine-core
   ("<tab>" . nil) ;; tabnine-accept-completion
   ("TAB" . nil)   ;; tabnine-accept-completion
   ("M-f" . nil)   ;; tabnine-accept-completion-by-word
   ("C-<return>" . tabnine-accept-completion))
  (:tabnine-chat-mode-map
   :package tabnine-chat
   ("C-<return>" . tabnine-chat-send)))

(leaf tabby
  :disabled t
  :url "https://tabby.tabbyml.com/"
  :doc "tabbyで補完を行う"
  :doc "---"
  :req "node v18以上が必要"
  :req "tabbyバイナリが必要"
  :doc "Ubuntu 22.04 LTSだとv13までしか動かない"
  :url "https://github.com/TabbyML/tabby/releases"
  :doc "---"
  :doc "バックグラウンドでtabbyサーバーを動かす"
  :doc "Chat無しも可能"
  :doc "tabby serve --model DeepseekCoder-1.3B --chat-model Qwen2-1.5B-Instruct --device cuda"
  :doc "tabby serve --model DeepseekCoder-1.3B --device cuda"
  :doc "tabby serve --device cuda"
  :doc "---"
  :doc "他に色々あるが、RTXのメモリが足りずに動かせないものがほとんど"
  :url "https://tabby.tabbyml.com/docs/models/"
  :doc "---"
  :doc "初回はlocahost:8080にログインしてadminアカウントを作成する"
  :doc "初回は~/.tabby-client/agent/config.tomlのserver項目を設定する"
  :doc "tokenはログインした先に書いてある"
  :doc "---"
  :doc "Ollamaを使う場合は、~/.tabby/config.tomlに設定する"
  :url "https://tabby.tabbyml.com/docs/references/models-http-api/ollama/"
  :doc "model_nameはdeepseek-coder:1.3bなど"
  :doc "api_endpointはhttp://localhost:11434"
  :doc "prompt_templateは設定しなくてよい"
  :doc "---"
  :doc "利用時は手動でtabby-modeをオンにする"
  :if (executable-find "tabby")
  :vc (:url "https://github.com/alan-w-255/tabby.el.git")
  ;; 消極的な補完、手動でC-c <tab>で補完候補を出す
  ;; :custom (tabby-idle-delay . 5)
  ;; :bind (:tabby-mode-map
  ;;        ("C-c <tab>" . tabby-complete)
  ;;        ("C-<return>" . tabby-accept-completion))
  ;; 積極的な補完、どんどんゴースト補完を提案する
  :custom (tabby-idle-delay . 0.6)
  :bind (:tabby-mode-map
         ("C-<return>" . tabby-accept-completion)))

;;; Flycheck

(defvar-local my/flycheck-next-local-cache nil)
(leaf flycheck
  :leaf-path nil
  :preface
  (leaf flycheck :ensure t
    :req "pipでflake8とmypyをいれておく"
    :defvar (flycheck-checker
             flycheck-checkers
             flycheck-gcc-language-standard
             flycheck-clang-language-standard)
    :global-minor-mode global-flycheck-mode
    :custom
    (flycheck-python-flake8-executable . "flake8")
    (flycheck-checker-error-threshold . 250)
    (flycheck-mode-line-prefix . "f")
    :bind (:flycheck-mode-map
           ("M-p" . flycheck-previous-error)
           ("M-n" . flycheck-next-error)))

  (leaf flycheck-posframe
    :ensure t
    :after flycheck
    :hook (flycheck-mode-hook . flycheck-posframe-mode)
    :custom (flycheck-posframe-position . 'window-bottom-right-corner))

  (leaf flycheck-color-mode-line :ensure t
    :after flycheck
    :hook (flycheck-mode-hook . flycheck-color-mode-line-mode))

  (leaf my/flycheck-next
    :doc "メジャーモードによってflycheckのnext-checkerを設定する"
    :url "https://github.com/flycheck/flycheck/issues/1762"
    :leaf-autoload nil
    :leaf-path nil
    :defun my/flycheck-next-checker-get
    :preface
    (defun my/flycheck-next-checker-get (fn checker property)
      (or (alist-get property (alist-get checker my/flycheck-next-local-cache))
          (funcall fn checker property)))
    :advice (:around flycheck-checker-get my/flycheck-next-checker-get))

  (leaf flycheck-ocaml :ensure t)

  (leaf flycheck-docker-build-checks
    :defun flycheck-docker-build-checks-setup
    :vc (:url "https://github.com/yonta/flycheck-docker-build-checks.git")
    :init (flycheck-docker-build-checks-setup)
    :hook
    ((dockerfile-ts-mode-hook dockerfile-mode-hook)
     . (lambda ()
         (setq my/flycheck-next-local-cache
               '((docker-build-checks
                  . ((next-checkers . (dockerfile-hadolint))))))))))

;;; LSP

(leaf lsp-bridge
  :disabled t
  :req "pip install epc orjson sexpdata six setuptools paramiko rapidfuzz"
  :defun lsp-bridge-show-documentation
  :ensure markdown-mode yasnippet
  :diminish t
  :vc (:url "https://github.com/manateelazycat/lsp-bridge.git")
  :defun global-lsp-bridge-mode
  :init
  ;; lsp-bridgeではacmを使うため、prog-mode全体でのcorfuをオフ
  (remove-hook 'prog-mode-hook 'my/corfu-mode)
  (global-lsp-bridge-mode)
  (defun my/helpful-at-point ()
    "Show documentation lsp-bridge or lisp help."
    (interactive)
    (if (derived-mode-p 'emacs-lisp-mode)
        (helpful-at-point)
      (lsp-bridge-show-documentation)))
  :custom
  (lsp-bridge-find-def-fallback-function . #'smart-jump-go)
  (lsp-bridge-find-ref-fallback-function . #'smart-jump-references)
  (lsp-bridge-enable-with-tramp . nil)
  (acm-doc-frame-max-lines . 30)
  (acm-candidate-match-function . 'orderless-flex)
  (acm-backend-search-file-words-enable-fuzzy-match . t)
  :hook
  ;; LSPを使わない言語ではcorfuを使う
  ((sml-mode-hook web-mode-hook css-base-mode-hook) . my/corfu-mode)
  :bind
  ("M-." . lsp-bridge-find-def)
  ("M-," . lsp-bridge-find-def-return)
  ("M-/" . lsp-bridge-find-references)
  ;; lsp-bridgeではcorfuがオンになっておらずcape-emojiが使いづらい
  (:lsp-bridge-mode-map
   ("C-c i :" . isearch-emoji-by-name)
   ("C-M-i" . nil)
   ("C-c C-d" . my/helpful-at-point))
  (:acm-mode-map
   :package acm
   ("C-f" . acm-complete)
   ("<tab>" . acm-insert-common)
   ("<backtab>" . acm-select-prev)
   ("C-s" . acm-filter)
   ("C-c C-d" . acm-doc-toggle)
   ("M-<up>" . acm-doc-scroll-up)
   ("M-<down>" . acm-doc-scroll-down)))

(leaf eglot
  :leaf-path nil
  :preface

  (leaf eglot
    :req "gem install solargraph -v 0.49.0"
    :req "npm install -g typescript-language-server"
    :doc "html, css, json, eslint"
    :req "npm install -g vscode-langservers-extracted"
    :req "npm install -g bash-language-server"
    :defun eglot-completion-at-point
    :defvar eglot-server-programs
    ;; debug出力なしでスピードアップ
    :custom (eglot-events-buffer-size . 0)
    :config
    (defun my/eglot-completion-at-point-with-cape ()
      "Completion function by `eglot-completion-at-point` with cape"
      (cape-wrap-super #'eglot-completion-at-point
                       #'cape-file
                       #'cape-dabbrev))
    ;; solargraphの出力がされていない不具合に対処
    ;; これがないと、例えばrubocopの結果がflycheckに出力されない
    ;; https://github.com/castwide/solargraph/issues/709
    ;;
    ;; eglotデフォルトではautoportを使っているが、stdioに変更
    ;; これによりeglot-boosterが動くようになる
    (add-to-list 'eglot-server-programs
                 '((ruby-mode ruby-ts-mode)
                   . ("solargraph" "stdio" :initializationOptions
                      (:diagnostics t))))
    :hook
    ;; Eglotがlocal変数でcompletion-at-point-functionsを上書きする
    ;; capeと組み合わせを手動で設定する
    (eglot-managed-mode-hook
     . (lambda ()
         (setq-local completion-at-point-functions
                     '(my/eglot-completion-at-point-with-cape)))))

  (leaf flycheck-eglot
    :ensure t
    :after eglot
    :global-minor-mode global-flycheck-eglot-mode)

  (leaf eglot-booster
    :req "cargoやgithubからemacs-lsp-boosterを入れておく"
    :req "git clone --depth 1 https://github.com/blahgeek/emacs-lsp-booster.git"
    :req "cargo build --release"
    :req "cp target/release/emacs-lsp-booster ~/bin/"
    :defun eglot-booster-mode
    :vc (:url "https://github.com/jdtsmith/eglot-booster.git")
    :if (executable-find "emacs-lsp-booster")
    :global-minor-mode t
    :after eglot)

  (leaf consult-eglot
    :ensure t
    :bind (:eglot-mode-map ("M-s s" . consult-eglot-symbols))))

;;; MODE

(leaf emacs-lisp
  :leaf-path nil
  :preface
  (leaf elisp-mode
    :defer-config
    (defun my-eval-region-or-line ()
      "Eval active region or current line."
      (interactive) (call-with-region-or-line #'eval-region))
    :bind ((:lisp-mode-shared-map ("C-c C-r" . my-eval-region-or-line))))

  (leaf eldoc
    :diminish eldoc-mode
    :custom (eldoc-idle-delay . 0.3))

  (leaf auto-async-byte-compile
    :ensure t
    :doc "save時に非同期で自動コンパイルする"
    :hook (emacs-lisp-mode-hook . enable-auto-async-byte-compile-mode))

  (leaf auto-compile
    :vc (:url "https://github.com/emacscollective/auto-compile.git")
    :doc "ロードの設定はearly-init.elにある"
    :doc "ディレクトリ名にバージョン番号を入れないようpackage-vcを使う"
    :doc "load時にelcが古ければ自動コンパイルする")

  (leaf lispxmp :ensure t
    :bind (:lisp-mode-shared-map
           :package elisp-mode
           ("C-M-;" . lispxmp)))

  (leaf macrostep
    :ensure t
    :bind (:lisp-mode-shared-map
           :package elisp-mode
           ("C-c e" . macrostep-expand))))

(leaf cc-mode
  :leaf-path nil
  :preface
  (leaf c-mode
    :preface
    :custom
    ;; 基本的にK&Rスタイルを使う
    (c-default-style . '((java-mode . "java")
                         (awk-mode . "awk")
                         (other . "k&r"))))

  (leaf c++-mode
    :preface
    (defun my-c++-mode-hook ()
      "Setting for c++-mode."
      (setq-local flycheck-gcc-language-standard "c++11")
      (setq-local flycheck-clang-language-standard "c++11"))
    :hook (c++-mode-hook . my-c++-mode-hook)))

(leaf tuareg :disabled t
  :ensure t
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

(leaf markdown
  :leaf-path nil
  :preface
  (leaf edit-indirect
    :doc "markdownでコードブロックの編集のために必要"
    :ensure t)

  (leaf markdown-mode
    :req "apt install markdown"
    :doc "markdown用メジャーモード。GitHub flavordのgfm-modeも同梱される。"
    :ensure t
    :if (executable-find "markdown")
    :defvar markdown-mode-map
    :mode ("README\\.md\\'" . gfm-mode)
    :custom
    (markdown-command . "markdown")
    ;; style sheetは生成HTMLと同フォルダにあるstyle.cssにする
    (markdown-css-paths . '("style.css"))
    :bind
    (:markdown-mode-map
     ;; markdown-outline-next-same-level
     ("C-c C-f" . nil)
     ;; originalはC-c'にマッピングされているcode block編集
     ("C-c `" . markdown-edit-code-block))
    (:gfm-mode-map ("C-c `" . markdown-edit-code-block)))

  (leaf grip-mode
    :req "cargo install mdopen"
    :doc "GitHub flavoredなスタイルシートによるMarkdownプレビューを行う"
    :ensure t
    :if (executable-find "mdopen")
    :custom (grip-use-mdopen . t)
    ;; gfm-modeのときは自動でgrip-mode
    ;; :hook (gfm-mode-hook . grip-mode)
    ))

(leaf csv-mode :ensure t)

(leaf sml-mode
  :leaf-path nil
  :preface
  (leaf sml-mode
    :vc ( :url "https://github.com/yonta/sml-mode.git"
          :branch "add-smlsharp")
    :mode ("\\.smi\\'" "\\.ppg\\'")
    :interpreter "smlsharp"
    :defun (sml-prog-proc-proc
            sml-prog-proc-send-string
            my-sml-prog-proc-send-region-by-string)
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
    :vc ( :url "https://github.com/yonta/company-mlton.git"
          :branch "add-smlsharp")
    :defun
    company-mlton-basis
    company-mlton-keyword
    :custom
    (company-mlton-modes . '(sml-mode inferior-sml-mode))
    ;; MLtonのbasisを除き、SMLのbasisを使う
    (company-mlton-basis-file
     . "~/.config/emacs/elpa/company-mlton/sml-basis-lib.basis")
    :config
    (defun my/company-mlton-init ()
      "Set company backends for completion"
      (setq-local completion-at-point-functions
                  (list
                   (cape-capf-super
                    ;; company-mlton系だけcase sensitiveになる
                    (cape-company-to-capf #'company-mlton-basis)
                    (cape-company-to-capf #'company-mlton-keyword)
                    (cape-company-to-capf #'company-dabbrev-code))
                   #'cape-dabbrev
                   #'cape-file)))
    :hook
    (sml-mode-hook . company-mlton-basis-autodetect)
    (sml-mode-hook . my/company-mlton-init))

  (leaf flycheck-smlsharp
    :vc (:url "https://github.com/yonta/flycheck-smlsharp.git")
    :after sml-mode
    :require t)

  (leaf flycheck-mlton
    :vc (:url "https://gist.github.com/80c938a54f4d14a1b75146e9c0b76fc2.git")
    :hook (sml-mode-hook . (lambda () (require 'flycheck-mlton))))

  (leaf sml-eldoc :disabled t
    :vc (:url "https://raw.githubusercontent.com/xuchunyang/emacs.d/master/lisp/sml-eldoc.el")
    :hook (sml-mode-hook . sml-eldoc-turn-on)))

(leaf python
  :leaf-path nil
  :preface
  (leaf python
    :leaf-path nil
    :defun python-shell-send-region
    :req "ruff, pyright, (mypy)"
    :req "*: pip install ruff pyright"
    :defer-config
    (defun my-python-shell-send-region-or-line ()
      "Call REPL with active region or current line."
      (interactive) (call-with-region-or-line #'python-shell-send-region))
    :hook
    (python-base-mode-hook . eglot-ensure)
    ;; eglot-checkの後にpython-ruffによるチェックを追加
    ;; なお、python-ruffのnextにpython-mypyがセットされている
    (python-base-mode-hook
     . (lambda ()
         (when (derived-mode-p 'python-base-mode)
           (setq my/flycheck-next-local-cache
                 '((eglot-check . ((next-checkers . (python-ruff)))))))))
    :bind (:python-ts-mode-map
           ("C-c C-r" . my-python-shell-send-region-or-line)
           ("<backtab>" . python-indent-shift-left))
    :custom
    (python-shell-interpreter . "python3")
    (python-indent-offset . 4))

  (leaf ruff-format
    :doc "Rust制ruffでフォーマットする"
    :req "reformatter.el"
    :req "ruff: pip install ruff"
    :ensure t
    :diminish ruff-format-on-save-mode
    :hook (python-base-mode-hook . ruff-format-on-save-mode))

  (leaf pip-requirements
    :ensure t
    :doc "requirements.txt用メジャーモード")

  (leaf highlight-indentation
    :ensure t
    :diminish highlight-indentation-mode
    ;; インデントに意味のあるPythonでとりあえず使う
    :hook (python-base-mode-hook . highlight-indentation-mode))

  (leaf pyvenv
    :doc "pyvenv-activate pyvenv-deactivateで便利にvenv管理できる"
    :req "venv: apt install python3-venv"
    :ensure t
    :hook python-base-mode-hook)

  (leaf pyvenv-auto
    :doc "vnev .venvディレクトリがあると自動でactivateする"
    :doc "別な複数のvenv環境のpythonファイルを開くとうまく行かない"
    :doc "そのときは手動でvenv-activate venv-deactivateする"
    :ensure t
    :hook (python-base-mode-hook . pyvenv-auto-run)))

(leaf ruby
  :leaf-path nil
  :preface
  (leaf ruby-lsp
    :disabled t
    :req "gem install ruby-lsp ruby-lsp-rails ruby-lsp-rspec ruby-lsp-rubyfmt"
    :after eglot
    :config
    (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

  (leaf inf-ruby
    :ensure t
    :hook (ruby-base-mode-hook . inf-ruby-minor-mode)
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

  (leaf rubocop
    :req "gemでrubocopを入れておく"
    :req "gem install rubocop"
    :ensure t
    :if (executable-find "rubocop"))

  (leaf rufo
    :disabled t
    :req "gemでrufoを入れておく"
    :req "gem install rufo"
    :doc "TODO: rufoやめてrubocop -aに移行したい"
    :ensure t
    :if (executable-find "rufo")
    :diminish rufo-minor-mode
    :hook (ruby-base-mode-hook . rufo-minor-mode))

  (leaf rubocopfmt
    ;; :disabled t
    :ensure t
    :if (executable-find "rubocop")
    :diminish rubocopfmt-mode
    :hook (ruby-base-mode-hook . rubocopfmt-mode))

  (leaf rspec-mode :ensure t :diminish t)

  (leaf yard-mode
    :ensure t
    :diminish t
    :hook (ruby-base-mode-hook . yard-mode))

  (leaf ruby-tools
    :doc "Rubyでダブルクオート・シングルクオート・シンボルを自動切り替え"
    :ensure t
    :diminish t
    :hook (ruby-base-mode-hook . ruby-tools-mode))

  (leaf seeing-is-believing
    :req "gem install seeing_is_believing"
    :doc "実行結果をコメントで表示・クリアする"
    :ensure t
    :config
    (diminish 'seeing-is-believing nil)
    :hook (ruby-base-mode-hook . seeing-is-believing))

  (leaf ruby-mode
    :req "gemでsolargraphを入れる"
    :req "gem install --version 0.49.0 solargraph"
    :req "gem install solargraph-rails solargraph-rails-patch-for-rails71 solargraph-rspec"
    :req "yard gems"
    :doc "yard config --gem-install-yriでgem install時に自動生成する設定が便利"
    :req "プロジェクトルートでsolargraph bundleを実行"
    :custom
    (ruby-insert-encoding-magic-comment . nil)
    :hook
    (ruby-base-mode-hook . eglot-ensure)
    (ruby-base-mode-hook
     . (lambda ()
         ;; ruby symbol
         (setq-local dabbrev-abbrev-skip-leading-regexp ":"))))

  (leaf rbs-mode :ensure t))

(leaf html-css
  :leaf-path nil
  :preface
  (leaf company-bootstrap5
    :defun company-bootstrap5
    :vc (:url "https://github.com/yonta/company-bootstrap5.git"))

  (leaf company-bootstrap-icons
    :vc (:url "https://github.com/yonta/company-bootstrap-icons.git"))

  (leaf company-web :ensure t)

  (leaf web-mode :ensure t
    :defun
    (company-web-html . company-web)
    (company-bootstrap5 . company-bootstrap5)
    (company-bootstrap-icons . company-bootstrap-icons)
    :mode "\\.erb\\'"
    :custom
    (web-mode-enable-comment-interpolation . t)
    (web-mode-enable-auto-pairing . t)
    (web-mode-enable-auto-quoting . t)
    (web-mode-enable-auto-expanding . t)
    (web-mode-enable-current-element-highlight . t)
    (web-mode-enable-current-column-highlight . t)
    (web-mode-comment-style . 2)
    (web-mode-auto-close-style . 2)
    ;; web-modeとwhitespace-modeのコンフリクトでfaceがおかしくなるのを解消する
    ;; https://github.com/fxbois/web-mode/issues/119a
    (web-mode-display-table . nil)
    :config
    (defun my/web-mode-init ()
      "Set company backends for completion"
      (setq-local completion-at-point-functions
                  (list
                   (cape-capf-super
                    (cape-company-to-capf #'company-web-html)
                    (cape-company-to-capf #'company-bootstrap5)
                    (cape-company-to-capf #'company-bootstrap-icons)
                    (cape-company-to-capf #'company-dabbrev-code))
                   #'cape-dabbrev
                   #'cape-file)))
    :hook (web-mode-hook . my/web-mode-init)
    :bind (:web-mode-map ("C-c C-f" . nil)))

  (leaf impatient-mode :ensure t
    :doc "HTMLのライブプレビューモード")

  (leaf reformatter
    :doc "htmlbeautifierに必要"
    :ensure t)

  (leaf htmlbeautifier
    :req "reformatter"
    :req "gem install htmlbeautifier"
    :if (executable-find "htmlbeautifier")
    :vc (:url "https://github.com/yonta/htmlbeautifier.el.git")
    :hook (web-mode-hook . htmlbeautifier-format-on-save-mode)
    :custom (htmlbeautifier-keep-blank-lines . 1))

  (leaf erblint
    :ensure t
    :defun erblint-command-prefix
    :custom
    ;; ファイル名、ライン、カラムを認識できるフォーマットにする
    (erblint-check-command . "erblint --format compact")
    :defer-config
    ;; pathが"\~/git/sakazuki"のようにバックスラッシュされるのを防ぐため
    ;; 修正した関数を定義する
    (defun erblint-build-command (command path)
      "Build the full command to be run based on COMMAND and PATH.
The command will be prefixed with `bundle exec` if Erblint is bundled."
      (mapconcat 'identity
                 (list (erblint-command-prefix) command path)
                 " ")))

  (leaf flycheck-markuplint
    :vc (:url "https://github.com/yonta/flycheck-markuplint.git")
    :hook ((web-mode-hook . flycheck-markuplint-setup)
           (html-mode-hook . flycheck-markuplint-setup)
           (mhtml-mode-hook . flycheck-markuplint-setup)))

  (leaf css-mode
    :custom
    (css-indent-offset . 2)
    :config
    (defun my/css-mode-init ()
      "Set company backends for completion"
      (setq-local completion-at-point-functions
                  (list
                   (cape-capf-super
                    (cape-company-to-capf #'company-css)
                    (cape-company-to-capf #'company-bootstrap5)
                    (cape-company-to-capf #'company-dabbrev-code))
                   #'cape-dabbrev
                   #'cape-file)))
    :hook (css-base-mode-hook . my/css-mode-init)))

(leaf javascript
  :leaf-path nil
  :preface
  (leaf js
    :mode ("\\.mjs\\'" . js-ts-mode)
    :custom (js-indent-level . 2)
    :hook
    (js-base-mode-hook . eglot-ensure)
    ;; eglot-checkの後にjavascript-eslintによるチェックを追加
    (js-base-mode-hook
     . (lambda ()
         (when (derived-mode-p 'js-base-mode)
           (setq my/flycheck-next-local-cache
                 '((eglot-check . ((next-checkers . (javascript-eslint))))))))))

  (leaf add-node-modules-path
    :ensure t
    :config
    ;; npm v9より`npm bin'が削除されたため、暫定対処
    ;; https://github.com/codesuki/add-node-modules-path/issues/23
    ;;
    ;; WHY
    ;; cusotmで設定するとadd-node-modules-pathが起動時に読み込まれてしまう
    ;; setqだと大丈夫
    (setq add-node-modules-path-command '("echo \"$(npm root)/.bin\""))
    :hook ((js-base-mode-hook . add-node-modules-path)
           (typescript-ts-base-mode-hook . add-node-modules-path)
           (markdown-mode-hook . add-node-modules-path)
           (css-base-mode-hook . add-node-modules-path)
           (html-mode-hook . add-node-modules-path)
           (web-mode-hook . add-node-modules-path))))

(leaf typescript
  :leaf-path nil
  :preface
  (leaf typescript-mode
    :ensure t
    :req "npmでtypescript-language-serverとtypescriptを入れておく"
    :req "npm install -g typescript-language-server typescript"
    :defvar flycheck-check-syntax-automatically
    :hook
    (typescript-ts-base-mode-hook . eglot-ensure)
    (typescript-ts-base-mode-hook
     . (lambda ()
         (setq-local flycheck-check-syntax-automatically
                     '(save mode-enabled))))
    ;; eglot-checkの後にjavascript-eslintによるチェックを追加
    (typescript-ts-base-mode-hook
     . (lambda ()
         (when (derived-mode-p 'typescript-ts-base-mode)
           (setq my/flycheck-next-local-cache
                 '((eglot-check . ((next-checkers . (javascript-eslint)))))))))
    :custom
    (typescript-indent-level . 2))

  (leaf prettier-js :ensure t
    :diminish prettier-js-mode
    ;; prettierのエラー内容をbufferに表示しない
    :custom (prettier-js-show-errors . 'echo)
    :hook (html-mode-hook
           css-base-mode-hook
           scss-mode-hook
           js-base-mode-hook
           json-ts-mode-hook
           typescript-ts-base-mode-hook
           markdown-mode-hook
           yaml-ts-mode-hook
           . prettier-js-mode))

  (leaf ts-comint :ensure t
    :if (executable-find "ts-node")
    :custom
    (ts-comint-program-command . "ts-node")
    :bind (:typescript-ts-base-mode-map
           :package typescript-ts-mode
           ("C-c C-r" . ts-send-region)
           ("C-c C-p" . run-ts))))

(leaf yaml-ts-mode :mode "\\.\\(e?ya?\\|ra\\)ml\\'")

(leaf docker
  :doc "TODO: yaml-modeの削除"
  :doc "      docker-compose-modeが依存している"
  :ensure (t
           dockerfile-mode
           docker-compose-mode
           yaml-mode)
  :mode ("compose\\.ya?ml\\'" . docker-compose-mode)
  :bind ("C-c C-x d" . docker))

(leaf jenkinsfile-mode :ensure t)

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

(defvar my/wakatime-cli-path
  (concat (getenv "XDG_CONFIG_HOME") "/wakatime/wakatime-cli-linux-amd64"))
(leaf wakatime-mode
  :ensure t
  :if (executable-find my/wakatime-cli-path)
  :global-minor-mode global-wakatime-mode
  :diminish t
  :custom (wakatime-cli-path . my/wakatime-cli-path))

(leaf rust-lang
  :leaf-path nil
  :preface
  (leaf rust-mode
    :ensure t
    :doc "flycheckが/dev/XXXXに書き込もうとしてパーミッションエラーすることがある"
    :doc "現在調査中で、以下URLにあるようにflycheckを変更する必要がある"
    :url "https://github.com/flycheck/flycheck/issues/2043#issuecomment-2377422002"
    :custom
    ;; Tree Sitter統合
    ;; 動いていない気がする
    (rust-mode-treesitter-derive . t))

  (leaf rustic
    :ensure t
    :custom
    (rustic-format-on-save . t)
    ;; (rustic-cargo-use-last-stored-arguments . t)
    (rustic-lsp-client . 'eglot)
    :config
    (push 'rustic-clippy flycheck-checkers)
    :hook
    (rustic-mode-hook
     . (lambda ()
         (when (derived-mode-p 'rustic-mode)
           (setq my/flycheck-next-local-cache
                 '((eglot-check . ((next-checkers . (rustic-clippy)))))))))
    :bind (:rustic-mode-map
           ("C-c C-c <return>" . rustic-cargo-comint-run))))

(leaf rust :disabled t
  :leaf-path nil
  :preface
  (leaf rust-mode
    :ensure t
    :doc "flycheckが/dev/XXXXに書き込もうとしてパーミッションエラーすることがある"
    :doc "現在調査中で、以下URLにあるようにflycheckを変更する必要がある"
    :url "https://github.com/flycheck/flycheck/issues/2043#issuecomment-2377422002"
    :mode ("\\.rs\\'")
    :after eglot
    :custom
    (rust-format-on-save . t)
    (rust-mode-treesitter-derive . t)
    :config
    (add-to-list 'eglot-server-programs
                 '((rust-ts-mode rust-mode) .
                   ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
    :hook (rust-ts-mode-hook . eglot-ensure))

  (leaf flycheck-rust :disabled t
    :ensure t
    :doc "flycheckでrust-cargが101エラーを返すときに使う"
    :doc "必要な変数設定をしてくれるらしい"
    :url "https://github.com/flycheck/flycheck/issues/2043#issuecomment-2378864669"
    :after rust-mode
    :hook (flycheck-mode-hook . flycheck-rust-setup))

  (leaf cargo
    :ensure t
    :hook (rust-ts-mode-hook . cargo-minor-mode)))

;;; Face

(leaf fontaine
  :doc "Font設定をまとめて行う"
  :req "IBM Plex Sans JPを.local/share/fontsにインストールする"
  :url "https://fonts.google.com/specimen/IBM+Plex+Sans+JP"
  :req "apt install fonts-noto-cjk-extra"
  :ensure t
  :require t
  :global-minor-mode t
  :config
  ;; 前回の設定を復元する
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  :custom
  (fontaine-presets
   . '(
       (small
        :default-height 190)
       (regular) ; like this it uses all the fallback values and is named `regular'
       (medium
        :default-weight semilight
        :default-height 250
        :bold-weight extrabold)
       (large
        :inherit medium
        :default-height 270)
       (presentation
        :default-height 300)
       (t
        ;; I keep all properties for didactic purposes, but most can be
        ;; omitted.  See the fontaine manual for the technicalities:
        ;; <https://protesilaos.com/emacs/fontaine>.
        :default-family "VL Gothic"
        :default-weight regular
        :default-height 230

        :fixed-pitch-family "VL Gothic"
        :fixed-pitch-weight nil ; falls back to :default-weight
        :fixed-pitch-height 1.0

        :fixed-pitch-serif-family "Noto Serif CJK JP"
        ;; :fixed-pitch-serif-family "IBM Plex Sans JP"
        :fixed-pitch-serif-weight nil ; falls back to :default-weight
        :fixed-pitch-serif-height 1.0

        ;; :variable-pitch-family "VL PGothic"
        ;; :variable-pitch-family "Noto Sans CJK JP"
        :variable-pitch-family "IBM Plex Sans JP"
        :variable-pitch-weight nil
        :variable-pitch-height 1.0

        :mode-line-active-family nil ; falls back to :default-family
        :mode-line-active-weight nil ; falls back to :default-weight
        :mode-line-active-height 1.0

        :mode-line-inactive-family nil ; falls back to :default-family
        :mode-line-inactive-weight nil ; falls back to :default-weight
        :mode-line-inactive-height 1.0

        :header-line-family nil ; falls back to :default-family
        :header-line-weight nil ; falls back to :default-weight
        :header-line-height 0.9

        :line-number-family nil ; falls back to :default-family
        :line-number-weight nil ; falls back to :default-weight
        :line-number-height 0.95

        :tab-bar-family nil ; falls back to :default-family
        :tab-bar-weight nil ; falls back to :default-weight
        :tab-bar-height 0.9

        :tab-line-family nil ; falls back to :default-family
        :tab-line-weight nil ; falls back to :default-weight
        :tab-line-height 0.9

        :bold-family nil ; use whatever the underlying face has
        :bold-weight bold

        :italic-family nil
        :italic-slant italic

        :line-spacing nil
        )))
  ;; :hook
  ;; テーマ切り替え時にフォントを維持する
  ;; (enable-theme-functions . fontaine-apply-current-preset)
  )

(leaf color
  :leaf-path nil
  :preface
  (leaf rainbow-mode
    :doc "#ff0000などに色をつける"
    :ensure t
    :diminish t
    :custom
    (rainbow-r-colors . t)                ; R color listを使う
    (rainbow-html-colors . t)             ; html color listを使う
    :hook (c++-mode-hook arduino-mode-hook))

  (leaf colorful-mode
    :doc "#ff0000などの前に色見本をつける"
    :ensure t
    :custom
    (colorful-use-prefix . t)))

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
  :hook
  ((java-mode-hook . (lambda () (setq-local whitespace-line-column 100)))
   (ruby-base-mode-hook
    . (lambda () (setq-local whitespace-line-column 120)))
   (web-mode-hook . (lambda () (setq-local whitespace-line-column 120)))
   (rust-mode-hook . (lambda () (setq-local whitespace-line-column 100)))
   ))

;; MEMO: 初回起動時にpackageがない状況ではrequireエラーする。
;;       マクロが実際に必要なので、強制インストールする。
;; (when (not (package-installed-p 'smartparens))
;;   (package-install 'smartparens))
(leaf parens
  :leaf-path nil
  :preface
  ;; sp-with-modesマクロの読み込み
  ;; (eval-when-compile (require 'smartparens))
  (leaf smartparens :disabled t
    :ensure t
    :defun sp-local-pair
    :global-minor-mode smartparens-global-mode
    :diminish smartparens-mode
    :config
    (require 'smartparens-config)
    ;; SML
    (sp-with-modes '(sml-mode inferior-sml-mode)
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "`" nil :actions nil)
      (sp-local-pair "sig" "end")
      (sp-local-pair "struct" "end")
      (sp-local-pair "(*" "*)" :post-handlers '(("|| " "SPC")
                                                ("* [i]||\n[i]" "RET"))))
    ;; <%に%>を対応させる
    (sp-with-modes '(web-mode)
      (sp-local-pair
       "<%" "%>" :post-handlers '(("|| " "SPC") (" || " "=") (" || " "#"))))
    ;; /*の後をいい感じにする
    (sp-with-modes '(js-base-mode typescript-ts-base-mode)
      (sp-local-pair "/*" "*/" :post-handlers '(("|| " "SPC")
                                                ("* [i]||\n[i]" "RET"))))
    ;; ｛の後にEnterすると｝の前に改行をつける
    (sp-with-modes
        '(web-mode js-base-mode css-base-mode typescript-ts-base-mode)
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
    ;; $で{}を補完する
    (sp-with-modes '(sh-mode bash-ts-mode)
      (sp-local-pair "$" "" :post-handlers '(:add "{|}")))
    )

  (leaf electric-pair-mode :global-minor-mode t)

  (leaf rainbow-delimiters
    :doc "デフォルトの色合いがだいぶ淡い"
    :doc "変更はテーマにて自分で設定する"
    :ensure t
    :hook (prog-mode-hook . rainbow-delimiters-mode))

  (leaf highlight-parentheses
    :doc "カーソル位置をくくるカッコをハイライトする"
    :ensure t
    :diminish highlight-parentheses-mode
    :hook (prog-mode-hook . highlight-parentheses-mode)))

(leaf highlight
  :leaf-path nil
  :preface
  (leaf auto-highlight-symbol :ensure t
    :leaf-defer nil
    :defvar ahs-modes
    :global-minor-mode global-auto-highlight-symbol-mode
    :diminish auto-highlight-symbol-mode
    :custom
    (ahs-default-range . 'ahs-range-whole-buffer)
    (ahs-disabled-minor-modes . '(iedit-mode))
    :config
    (push 'sml-mode ahs-modes))

  (leaf goggles
    :doc "変更箇所を強調する"
    :ensure t
    :diminish t
    :hook prog-mode-hook text-mode-hook
    :custom
    ;; 色を薄くする回数、1で即消える
    (goggles-pulse-iterations . 10)
    ;; 色を薄くする1回ごとの秒数
    (goggles-pulse-delay . 0.2))

  (leaf hl-line+
    :vc (:url "https://github.com/emacsmirror/hl-line-plus.git")
    :defun (toggle-hl-line-when-idle hl-line-when-idle-interval)
    :config
    (toggle-hl-line-when-idle 1)
    (hl-line-when-idle-interval 4)))

(leaf git-gutter
  :disabled t
  :ensure t
  :global-minor-mode global-git-gutter-mode
  :diminish git-gutter-mode
  :custom
  (git-gutter:modified-sign . " ")
  (git-gutter:added-sign . " ")
  (git-gutter:deleted-sign . " ")
  :custom-face
  (git-gutter:modified . '((t (:background "#fad987"))))
  (git-gutter:added . '((t (:background "#95fa87"))))
  (git-gutter:deleted . '((t (:background "#fa8d87")))))

(leaf diff-hl
  :ensure t
  :global-minor-mode global-diff-hl-mode diff-hl-margin-mode
  :custom
  (diff-hl-margin-symbols-alist . '((insert . " ")
                                    (delete . " ")
                                    (change . " ")
                                    (unknown . "?")
                                    (ignored . "i")))
  :custom-face
  (diff-hl-change . '((t (:background "#fad987"))))
  (diff-hl-insert . '((t (:background "#95fa87"))))
  (diff-hl-delete . '((t (:background "#fa8d87"))))
  :hook (dired-mode-hook . diff-hl-dired-mode))

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
  :emacs< 29
  :ensure t tree-sitter-langs
  :diminish tree-sitter-mode
  :global-minor-mode global-tree-sitter-mode
  :custom (tsc-dyn-get-from . '(:compilation))
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode))

(leaf treesit
  :emacs>= 29
  :ensure treesit-auto
  :global-minor-mode (global-treesit-auto-mode . treesit-auto)
  :custom
  (treesit-font-lock-level . 4)
  (treesit-auto-install . 'prompt))

(leaf nerd-icons
  :leaf-path nil
  :preface
  (leaf nerd-icons
    :ensure t
    :req "初回に`M-x nerd-icons-install-fonts`を実行する")

  (leaf nerd-icons-dired
    :ensure t
    :diminish t
    :hook (dired-mode-hook . nerd-icons-dired-mode))

  (leaf nerd-icons-ibuffer
    :ensure t
    :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode)))

(leaf tab-line-mode
  :global-minor-mode global-tab-line-mode
  :defvar tab-line-tabs-buffer-list-function
  :defun
  tab-line-tab-name-truncated-buffer
  tab-line-tabs-mode-buffers
  my/tab-line-tabs-buffer-list
  :custom
  (tab-line-new-button-show . nil)
  (tab-line-close-button-show . nil)
  ;; タブ名を短く
  (tab-line-tab-name-function . #'tab-line-tab-name-truncated-buffer)
  ;; メジャーモードでタブをまとめる
  (tab-line-tabs-function . #'tab-line-tabs-mode-buffers)
  (tab-line-switch-cycling . t)
  ;; lisp-interactionではタブを出さない
  (tab-line-exclude-modes . '(completion-list-mode lisp-interaction-mode))
  :custom-face
  (tab-line . '((t (:foreground "black" :background "gray90"))))
  (tab-line-tab-current . '((t (:inherit highlight))))
  (tab-line-tab-modified . '((t (:foreground "red"))))
  (tab-line-tab-inactive . '((t (:background "gray84"))))
  :init
  ;; scratchバッファがemacs lispタブグループを壊す
  ;; scratchバッファをタブの対象外にする
  (defun my/tab-line-tabs-buffer-list ()
    "Return a list of buffers excluded Lisp Interaction mode.

Because, for example, scratch buffer is matching with Lisp Interaction mode,
and also Emacs Lisp mode.
So this means that scratch buffer breaks Emacs Lisp mode tabs."
    (seq-filter
     (lambda (b) (and (buffer-live-p b)
                      (/= (aref (buffer-name b) 0) ?\s)
                      ;; ADDED: not lisp-interaction-mode
                      (with-current-buffer b
                        (not (derived-mode-p 'lisp-interaction-mode)))))
     (seq-uniq (append (list (current-buffer))
                       (mapcar #'car (window-prev-buffers))
                       (buffer-list)))))
  (setq tab-line-tabs-buffer-list-function #'my/tab-line-tabs-buffer-list)
  :bind (("C-<tab>" . tab-line-switch-to-next-tab)
         ("C-<iso-lefttab>" . tab-line-switch-to-prev-tab)
         ;; ブラウザのタブ復元風ショートカット
         ("C-S-t" . recentf-open-most-recent-file)))

;;; OTHER

(leaf project
  :doc "C-x pにコマンドがまとまっている"
  :bind* ("C-c C-f" . project-find-file))

(leaf ripgrep :disabled t
  :ensure t
  :bind
  ("M-s r" . ripgrep-regexp))

(leaf rg
  :ensure t
  :bind
  ("M-s r" . rg)
  ("M-s p" . rg-project))

(leaf super-hint
  :doc "rg/xrefの検索結果にwhich-funcによる関数名やクラス名を追加表示する"
  :preface
  (leaf which-func
    :global-minor-mode which-function-mode
    :custom
    ;; mode line表示をしない
    (which-func-format . ""))

  :vc (:url "https://github.com/eval-exec/super-hint.el")
  :after rg
  :global-minor-mode super-hint-rg-mode super-hint-xref-mode
  :diminish super-hint-rg-mode super-hint-xref-mode
  :custom
  (super-hint-hint-width . 20))

(leaf grep-context
  :doc "grep系コマンドにて+e/-を使って周りの行を展開する"
  :vc (:url "https://github.com/emacs-pe/grep-context.git")
  :hook (compilation-mode-hook . grep-context-mode)
  :bind (:grep-context-mode-map ("e" . grep-context-mode-around-point)))

(leaf popper :ensure t
  :global-minor-mode t popper-echo-mode
  :custom
  (popper-reference-buffers . '(;; hide
                                ("\\*Warnings\\*" . hide)
                                (" \\*auto-async-byte-compile\\*" . hide)
                                ("\\*Compile-Log\\*" . hide)
                                ("[lL]og\\*$" . hide) ; log終わり
                                ;; not hide
                                compilation-mode
                                completion-list-mode ; 全completionを対象
                                ;; rustic-compilation-mode
                                ;; rustic-format-mode
                                ;; "\\*cargo-run-comint\\*"
                                ;; "\\*cargo-run\\*"
                                help-mode
                                helpful-mode
                                man-mode
                                inf-ruby-mode
                                ts-comint-mode
                                inferior-sml-mode
                                "\\*scratch\\*"
                                "\\*quickrun\\*"
                                "\\*xref\\*"
                                "\\*Backtrace\\*"
                                "\\*ripgrep-search\\*"
                                "\\*rg\\*"
                                "\\*Google Translate\\*"
                                "\\*robe-doc\\*"
                                "\\*lsp-bridge-doc\\*"
                                " \\*undo-tree\\*"
                                " \\*vundo tree\\*"
                                " \\*tip\\*"
                                " \\*eglot doc\\*"
                                "^ \\*Rubocop.*\\*$"
                                ))
  ;; popper-echoでk/^コマンドを有効化
  (popper-echo-dispatch-actions . t)
  (popper-echo-dispatch-keys . '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
  :config
  ;; popperの対象としつつ、フォーカスしない
  (add-to-list 'display-buffer-alist
               '("\\*lsp-bridge-doc\\*" (popper-display-popup-at-bottom)))
  :bind
  ("C-^" . popper-toggle-type)
  :bind*
  ("M-o" . popper-toggle))

(leaf google-translate
  :leaf-path nil
  :preface
  (leaf google-translate :ensure t)

  (leaf google-translate-smooth-ui
    :defvar google-translate-translation-directions-alist
    :defer-config
    (setq google-translate-translation-directions-alist
          '(("en" . "ja") ("ja" . "en")))
    :bind ("C-c C-t" . google-translate-smooth-translate)))

(leaf shell
  :leaf-path nil
  :preface
  (leaf ansi-color
    :doc "コマンドラインと同じ色付けを使う"
    :commands ansi-color-for-comint-mode-on
    :hook (shell-mode-hook . ansi-color-for-comint-mode-on))

  (leaf sh-script
    :mode (("Procfile" . sh-mode)
           ("dotenv" . sh-mode))
    :hook (sh-mode-hook
           . (lambda () (setq-local flycheck-checker 'sh-posix-bash)))
    :bind (:sh-mode-map
           ("C-c C-d" . nil)
           ("C-c C-p" . sh-cd-here)))

  (leaf shell
    :custom
    ;; Emacsを起動したshellを使用する（bashからの起動を前提）
    ;; TODO: バイトコンパイル時でなく起動時に評価するよう変更する
    `(explicit-shell-file-name . ,(getenv "SHELL"))
    ;; (explicit-shell-file-name . my-shell-file-name)
    (explicit-bash-args . '("--login" "-i"))
    ;; shell-modeでのファイル名補完
    (shell-file-name-chars . "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")
    ;; :hook
    ;; (shell-mode-hook
    ;;  . (lambda ()
    ;;      ;; SHELL で ^M が付く場合は ^M を削除する
    ;;      (set-process-coding-system
    ;;       'undecided-dos 'sjis-unix)))
    )

  (leaf bash-ts-mode
    :mode ("\\.bash_aliases\\'" . bash-ts-mode)
    :hook (bash-ts-mode-hook . eglot-ensure)))

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
  :bind ((:image-dired-thumbnail-mode-map
          :package image-dired
          ("r" . nil)
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

(leaf helpful :ensure t
  :bind* ("<f1> k" . helpful-key)
  :bind ("C-c C-d" . helpful-at-point))

(leaf smart-jump
  :req "ripgrepをpcre2サポートありでインストールしておく"
  :req "cargo install ripgrep --features 'pcre2'"
  :doc "またはデフォルトのripgrepを使う場合は、"
  :doc "custom値を設定してpcre2を使わないようにする"
  :doc ":custom (dumb-jump-rg-search-args . \"\")"
  :ensure t dumb-jump
  :defvar dumb-jump-find-rules
  :defun smart-jump-simple-find-references smart-jump-find-references-with-rg
  :custom
  ;; ripgrepを使う
  (smart-jump-find-references-fallback-function
   . #'smart-jump-find-references-with-rg)
  ;; xrefをdumb-jumpで行うhook
  ;; :hook (xref-backend-functions . dumb-jump-xref-activate)
  :defer-config
  ;; dump-jump対応言語はすべて登録する
  (require 'dumb-jump)
  (eval-when-compile (require 'dash))
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
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("M-/" . smart-jump-references)))

(leaf expreg
  :doc "モダンなexpand-region"
  :ensure t
  :bind (("C-`" . expreg-expand)
         ("C-{" . expreg-contract)))

(leaf which-key :ensure t
  :global-minor-mode t
  :diminish which-key-mode
  :custom
  (which-key-side-window-max-height . 0.4)
  (which-key-max-description-length . 50)
  :config
  (which-key-setup-side-window-bottom))

(leaf sudo-edit :ensure t)

(leaf visual-regexp :ensure t
  :bind ("M-&" . vr/query-replace))

(leaf async :ensure t
  :hook (emacs-lisp-mode-hook . async-bytecomp-package-mode))

(leaf vundo
  :doc "モダンなundo-tree"
  :ensure t
  :bind ("C-c C-/" . vundo))

(leaf activities
  :doc "activities-modeは自動保存を行うグローバルモード"
  :doc "そこで、activities-resumeするまでグローバルモードを遅延する"
  :doc "これによりactivitiesの読み込みを遅延する"
  :ensure t
  :config (activities-mode 1) ; HACK
  :bind
  (("C-x a n" . activities-new)
   ("C-x a r" . activities-resume)
   ("C-x a a" . activities-resume)
   ("C-x a s" . activities-suspend)
   ("C-x a k" . activities-kill)
   ("C-x a RET" . activities-switch)
   ("C-x a b" . activities-switch-buffer)
   ("C-x a g" . activities-revert)
   ("C-x a l" . activities-list)))

(leaf theme
  :leaf-path nil
  :preface
  (leaf rebecca-theme :ensure t :disabled t)

  (leaf solo-jazz-theme :disabled t
    :ensure t :require t
    :config
    (load-theme 'solo-jazz t))

  (leaf humanoid-themes :ensure t :disabled t)

  (leaf github-modern-theme :ensure t :disabled t))

(leaf mozc
  :if (getenv "WSLENV")
  :leaf-path nil
  :init
  (leaf mozc
    :req "予め${HOME}/bin/mozc_emacs_helperを用意するか、"
    :req "aptでemacs-mozc-binを入れておく。"
    :url "https://w.atwiki.jp/ntemacs/pages/61.html"
    :url "https://github.com/smzht/mozc_emacs_helper"
    :ensure t
    :defun mozc-session-sendkey
    :if (executable-find "mozc_emacs_helper")
    ;; mozcモードで一部キーバインドが外れるので再設定
    :init
    (defun my/mozc-mode-disable ()
      "Disable mozc mode"
      (interactive)
      (mozc-mode -1))
    :bind*
    ("<zenkaku-hankaku>" . mozc-mode)
    ("<henkan>" . mozc-mode)

    ("<muhenkan>" . my/mozc-mode-disable)
    :bind (:mozc-mode-map
           ("C-x C-s" . save-buffer)
           ("C-x h" . mark-hole-buffer))
    :custom
    (default-input-method . "japanese-mozc")
    (mozc-mode-string . " [も]")
    ;; WindowsのGoogle日本語入力を使う
    ;; :advice (:after mozc-session-execute-command
    ;;                 (lambda (&rest args)
    ;;                   (when (eq (nth 0 args) 'CreateSession)
    ;;                     (mozc-session-sendkey '(Hankaku/Zenkaku)))))
    :defer-config
    (set-language-environment "Japanese")
    (prefer-coding-system 'utf-8))

  (leaf mozc-cand-posframe
    :ensure t
    :after mozc
    :require t
    :custom
    (mozc-candidate-style . 'posframe)))

(leaf keyfreq
  :ensure t
  :global-minor-mode t keyfreq-autosave-mode
  :custom (keyfreq-file . "~/.config/emacs/keyfreq")
  )

(leaf editorconfig :ensure t
  :doc "projectの.editorconfigファイルを読み込む"
  :diminish t
  :global-minor-mode t)

(leaf imenu
  :leaf-path nil
  :preface
  (leaf imenu-list :ensure t
    :bind ("C->" . imenu-list-smart-toggle)
    :custom
    (imenu-list-focus-after-activation . t)))

(leaf buffer-move :ensure t
  :bind* (("C-S-h" . buf-move-left)
          ("C-S-j" . buf-move-down)
          ("C-S-k" . buf-move-up)
          ("C-S-l" . buf-move-right)))

(leaf flyspell
  :leaf-path nil
  :preface
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
  (add-to-list 'aggressive-indent-excluded-modes 'js-base-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'typescript-ts-base-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'python-base-mode)
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

(leaf demap :ensure t
  :bind* ("M-m" . demap-toggle))

;;; Emacs default (not package.el)

(leaf dired
  :leaf-path nil
  :preface
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

  (leaf dired-single
    :doc "diredバッファが複数開くのを防ぐ"
    :vc (:url "https://github.com/emacsattic/dired-single.git")
    :bind (:dired-mode-map
           :package dired
           ("C-m" . dired-single-buffer)
           ("^" . dired-single-up-directory)
           ("C-." . my-toggle-dired-listing-switches)
           ("r" . wdired-change-to-wdired-mode)))

  (leaf dired-subtree
    :doc "diredでiを使うとサブツリー展開をする"
    :ensure t
    ;; nerd-iconsをうまく処理する
    ;; https://github.com/rainstormstudio/nerd-icons-dired/issues/3#issuecomment-1663217291
    :preface
    (defun my/dired-subtree-reflesh-nerd-icons ()
      (interactive)
      (revert-buffer))
    :advice (:after dired-subtree-toggle my/dired-subtree-reflesh-nerd-icons)
    :bind (:dired-mode-map
           :package dired
           ("i" . dired-subtree-toggle))))

(leaf help-mode
  ;; Alt+左右でヘルプの進む・戻るを行う、デフォルトはl/r
  :bind (:help-mode-map
         ("M-<left>" . help-go-back)
         ("M-<right>". help-go-forward)))

(leaf ibuffer
  :defun ibuffer-current-buffer
  :bind* ("C-x C-b" . ibuffer))

(leaf winner
  :global-minor-mode t
  :bind (("C-M-<left>" . winner-undo)
         ("C-M-<right>" . winner-redo)))

(leaf line-number
  :leaf-autoload nil
  :leaf-defun nil
  :leaf-path nil
  :global-minor-mode column-number-mode
  :preface
  ;; mode-lineの表示からラインナンバーを消す ex. (10,1) -> 1
  (line-number-mode -1)

  (leaf display-line-numbers
    :emacs>= 26
    :global-minor-mode global-display-line-numbers-mode)

  (leaf linum
    :emacs<= 25
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
  (setq auto-insert-directory "~/.config/emacs/autoinsert/")
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
  :leaf-path nil
  :preface
  (leaf recentf
    :doc "最近使ったファイルを.recentfファイルに保存する"
    :global-minor-mode t
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
  :smartrep* ("C-c"
              (("l" . windmove-right)
               ("h" . windmove-left)
               ("j" . windmove-down)
               ("k" . windmove-up)
               ;; ("<left>" . windmove-right)
               ;; ("<right>" . windmove-left)
               ;; ("<down>" . windmove-down)
               ;; ("<up>" . windmove-up)
               )))

(leaf my/window-resizer
  :doc "分割ウィンドウのサイズを変更するmy/window-resizer"
  :doc "smartrep用に改変している。"
  :doc "オリジナルは以下。"
  :url "https://khiker.hatenablog.jp/entry/20100119/window_resize"
  :leaf-path nil
  :leaf-autoload nil
  :init
  (defun my/window-resizer-right ()
    "Resize window by right key"
    (interactive)
    (if (<= (nth 2 (window-edges)) (frame-width))
        (enlarge-window-horizontally 1)
      (shrink-window-horizontally 1)))  ; 右端frameのとき
  (defun my/window-resizer-left ()
    "Resize window by left key"
    (interactive)
    (if (<= (nth 2 (window-edges)) (frame-width))
        (shrink-window-horizontally 1)
      (enlarge-window-horizontally 1))) ; 右端frameのとき
  (defun my/window-resizer-down ()
    "Resize window by down key"
    (interactive)
    (if (< (nth 3 (window-edges)) (1- (frame-height))) ; minibuffer分を-1
        (enlarge-window 1)
      (shrink-window 1)))               ; 下端frameのとき
  (defun my/window-resizer-up ()
    "Resize window by up key"
    (interactive)
    (if (< (nth 3 (window-edges)) (1- (frame-height))) ; minibuffer分を-1
        (shrink-window 1)
      (enlarge-window 1)))              ; 下端frameのとき
  :smartrep* ("C-c r"
              (("l" . my/window-resizer-right)
               ("h" . my/window-resizer-left)
               ("j" . my/window-resizer-down)
               ("k" . my/window-resizer-up))))

(leaf my/swap-window
  :doc "現在のウィンドウと次のウィンドウを入れ替えする"
  :leaf-path nil
  :leaf-autoload nil
  :init
  (defun my/swap-window ()
    "Swap two screen, leaving cursor at current window."
    (interactive)
    (let ((thiswin (selected-window))
          (nextwin (window-buffer (next-window))))
      (set-window-buffer (next-window) (window-buffer))
      (set-window-buffer thiswin nextwin)))
  :smartrep* ("C-x" (("O" . my/swap-window))))

(leaf indent
  :leaf-path nil
  :preface
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
  ("C-c s" . sticky-buffer-mode))

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
  :req "aptでwsluをいれておく"
  :if (getenv "WSLENV") (executable-find "wslview")
  :custom
  (browse-url-browser-function . #'browse-url-generic)
  (browse-url-generic-program . "wslview"))

(leaf clipboard
  :doc "emacs29 + pureGTKでクリップボードが文字化けする問題を対処"
  :doc "credit: yorickvP on Github"
  :req "wl-clipboardをインストールしておく"
  :req "sudo apt install wl-clipboard"
  :url "https://zenn.dev/ignorant/scraps/4456a9fb017eb3"
  :url "https://www.emacswiki.org/emacs/CopyAndPaste#h5o-4"
  :leaf-path nil
  :emacs>= 29
  :if (and (getenv "WSLENV")
           (executable-find "wl-copy")
           (executable-find "wl-paste")
           (string-match "--with-pgtk" system-configuration-options))
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
  :emacs>= 29
  :config
  (pixel-scroll-precision-mode))

(leaf comp
  :leaf-path nil
  :doc "native compile"
  :if (native-comp-available-p)
  :preface
  (defvar my/comp-init-files-list
    '("~/.config/emacs/init.el"
      "~/.config/emacs/early-init.el"
      "~/.config/emacs/lisp"))
  (defun my/comp-all-files ()
    "Compile configuration files with native compilation."
    (interactive)
    (native-compile-async
     (append
      ;; directories
      '("~/.config/emacs/lisp" "~/.config/emacs/elpa")
      my/comp-init-files-list)
     'recursively))
  (defun my/comp-init-files ()
    "Compile configuration files with native compilation."
    (interactive)
    (native-compile-async my/comp-init-files-list 'recursively))
  :custom (native-comp-async-jobs-number . 3))

(leaf savehist
  :doc "minibufferの履歴を保存する"
  :leaf-path nil
  :global-minor-mode t)

(leaf ellama
  :doc "cl-defstruct生成make-llm-ollamaを手動展開しコンパイル可にした"
  :if (executable-find "ollama")
  :ensure t
  :defer-config
  (require 'llm-ollama)
  :custom
  (ellama-major-mode . 'markdown-mode)
  (ellama-naming-scheme . 'ellama-generate-name-by-time)
  (ellama-providers
   . '(("deepseek-coder-v2"
        . #s(llm-ollama nil nil nil "http" "localhost" 11434
                        "deepseek-coder-v2:16b-lite-instruct-q4_K_S"
                        "deepseek-coder-v2:16b-lite-instruct-q4_K_S"))
       ("gemma2"
        . #s(llm-ollama nil nil nil "http" "localhost" 11434
                        "gemma2:2b-instruct-q4_K_S"
                        "gemma2:2b-instruct-q4_K_S"))
       ("codeqwen1.5"
        . #s(llm-ollama nil nil nil "http" "localhost" 11434
                        "codeqwen:7b-chat-v1.5-q4_K_S"
                        "codeqwen:7b-chat-v1.5-q4_K_S"))))
  ;; translation
  (ellama-language . "日本語")
  (ellama-translation-provider
   . #s(llm-ollama nil nil nil "http" "localhost" 11434
                   "gemma2:2b-instruct-q4_K_S"
                   "gemma2:2b-instruct-q4_K_S"))
  (ellama-translation-template . "%sで話して。「%s」を%sに翻訳して。")
  ;; code generation
  (ellama-provider
   . #s(llm-ollama nil nil nil "http" "localhost" 11434
                   "deepseek-coder-v2:16b-lite-instruct-q4_K_S"
                   "deepseek-coder-v2:16b-lite-instruct-q4_K_S"))
  (ellama-define-word-prompt-template . "%s の定義を教えて")
  (ellama-summarize-prompt-template . "Text:\n%s\n要約して")
  (ellama-code-review-prompt-template . "以下のコードのレビューと改善案をだして:\n```\n%s\n```")
  (ellama-change-prompt-template . "以下のテキストを「%s」と変更して、引用符なしで出力して:\n%s")
  (ellama-improve-grammar-prompt-template . "誤字脱字・文法を校正して")
  (ellama-improve-wording-prompt-template . "語句を推敲して")
  (ellama-improve-conciseness-prompt-template . "できるだけ簡潔にして")
  (ellama-code-edit-prompt-template
   . "以下のコードを「%s」と変更して、プロンプト無しでコードだけを\n```language\n...\n```\nの形式で出力して:\n```\n%s\n```\n")
  (ellama-code-improve-prompt-template
   . "以下のコードを改善して、プロンプト無しでコードだけを\n```language\n...\n```の形式で出力して:\n```\n%s\n```\n")
  (ellama-code-complete-prompt-template
   . "以下のコードの続きを書いて、プロンプト無しでコードだけを\n```language\n...\n```の形式で出力して:\n```\n%s\n```\n")
  (ellama-code-add-prompt-template
   . "Context: \n```\n%s\n```\nこのコードを文脈として、%s、プロンプト無しでコードだけを\n```\n...\n```\nの形式で出力して\n")
  (ellama-generate-commit-message-template
   . "あなたは熟練プログラマーです。後の変更点をもとに簡潔なコミットメッセージを書いてください。コミットメッセージの形式は、1行目は変更点の要約、2行目は空行、それ以降の行は変更全体の詳細な説明、です。出力はプロンプト無しで最終的なコミットメッセージだけにしてください。\n\n変更点:\n%s\n")
  (ellama-make-format-prompt-template . "以下のテキストを%sの形式に変換して:\n%s")
  (ellama-make-list-prompt-template . "Markdownのリスト形式にして")
  (ellama-make-table-prompt-template . "Markdownのテーブル形式にして"))

;;; init_package.el ends here
