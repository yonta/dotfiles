;;; package list
; インストールするパッケージのリスト
(defvar my-package-list
  '(
    ; なぜか1つ目のパッケージがエラー、`packagename-`とハイフンがついた
    ; パッケージが無いと怒られる。
    ; 初回起動はlist-packageの更新が間に合ってない？
    ; 一度list-packageを実行すると治る。
    yasnippet
    ;; C++
    rtags
    ;; other mode
    csv-mode
    gnuplot-mode
    graphviz-dot-mode
    bash-completion
    proof-general
    ;; view
    image-dired+
    image+
    w3m
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
