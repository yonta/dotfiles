;;; package list
; インストールするパッケージのリスト
(defvar my-package-list
  '(
    ; なぜか1つ目のパッケージがエラー、`packagename-`とハイフンがついた
    ; パッケージが無いと怒られる。
    ; 初回起動はlist-packageの更新が間に合ってない？
    ; 一度list-packageを実行すると治る。
    ;; ocaml
    flycheck-ocaml tuareg yasnippet
    ;; arduino
    company-arduino arduino-mode
    ;; C++
    rtags flycheck flycheck-irony irony
    auto-complete-clang auto-complete-c-headers
    ;; python
    python-mode elpy py-autopep8 flymake-python-pyflakes flymake-cursor quickrun
    ;; markdown
    ;; コードブロックの編集のため、edit-indirectが必要
    markdown-mode edit-indirect
    ;; other mode
    csv-mode
    sml-mode
    twittering-mode
    gnuplot-mode
    graphviz-dot-mode
    google-translate
    bash-completion
    haxe-mode
    proof-general
    ;; view
    popwin
    image-dired+
    image+
    fill-column-indicator
    hiwin
    highlight-current-line
    rainbow-mode
    w3m
    swiper
    smartparens
    browse-kill-ring
    ; 起動時間測定
    esup
    ;; doctags
    ))
;(unless package-archive-contents (package-refresh-contents))
; インストール
(dolist (pkg my-package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))
