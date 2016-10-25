;;; package list
; インストールするパッケージのリスト
(defvar my-package-list
  '(
    ; なぜか1つ目のパッケージがエラー、`packagename-`とハイフンがついた
    ; パッケージが無いと怒られる、手動で入れるとOK
    ; なので、dummyとして既に入っているbuilt-inパッケージを記述する
    json
    ;; ocaml
    flycheck-ocaml tuareg yasnippet
    ;; arduino
    company-arduino arduino-mode
    ;; C++
    rtags flycheck flycheck-irony irony
    auto-complete-clang auto-complete-c-headers
    ;; python
    python-mode elpy py-autopep8 flymake-python-pyflakes flymake-cursor quickrun
    ;; other mode
    markdown-mode csv-mode csv-nav sml-mode twittering-mode gnuplot-mode dos
    google-translate bash-completion
    ;; view
    popwin image-dired+ image+
    hiwin highlight-current-line rainbow-mode
    w3m swiper smartparens
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
