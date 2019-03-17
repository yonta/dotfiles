;;; ansi-term
;; コマンドラインと同じ色付けを使う
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
          "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; タブ文字を禁止してスペースを使う
(setq-default tab-width 4 indent-tabs-mode nil)

;;; sml-mode
(add-hook 'sml-mode-hook
          '(lambda()
             ;; sml-modeのインデント幅を2にする
             (setq sml-indent-level 2)
             (setq sml-indent-args 2)
             ;; sml-modeのrun-smlでデフォルトSMLコマンドをsmlsharpにする
             (setq sml-program-name "smlsharp")
             ))

;; SML#ファイルを関連付ける
(setq auto-mode-alist
      (cons '("\\.\\(smi\\|ppg\\)$" . sml-mode) auto-mode-alist))

;;; c/c++-mode
;; K&Rスタイルを使う
(add-hook 'c-mode-hook
          '(lambda()
             (c-set-style "k&r")
             (setq c-basic-offset 2)
             (setq tab-width c-basic-offset)
             (setq indent-tabs-mode nil)
             ))
(add-hook 'c++-mode-hook
          '(lambda()
             (c-set-style "k&r")
             (setq c-basic-offset 2)
             (setq tab-width c-basic-offset)
             (setq indent-tabs-mode nil)
             (setq flycheck-gcc-language-standard "c++11")
             (setq flycheck-clang-language-standard "c++11")
             ))

;;; dired-mode
;; サイズや拡張子による並び替えを追加する．
;; http://d.hatena.ne.jp/mooz/20091207/p1
(defvar dired-various-sort-type
  '(("S" . "size")
    ("X" . "extension")
    ("v" . "version")
    ("t" . "date")
    (""  . "name")))
(defun dired-various-sort-change (sort-type-alist &optional prior-pair)
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
  "Hehe"
  (interactive "P")
  (when dired-sort-inhibit
    (error "Cannot sort this dired buffer"))
  (if arg
      (dired-sort-other
       (read-string "ls switches (must contain -l): " dired-actual-switches))
    (dired-various-sort-change dired-various-sort-type)))
(defvar anything-c-source-dired-various-sort
  '((name . "Dired various sort type")
    (candidates . (lambda ()
                    (mapcar (lambda (x)
                              (cons (concat (cdr x) " (" (car x) ")") x))
                            dired-various-sort-type)))
    (action . (("Set sort type" .
                (lambda (candidate)
                  (dired-various-sort-change
                   dired-various-sort-type candidate)))))
    ))
(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "s" 'dired-various-sort-change-or-edit)
             (define-key dired-mode-map "c"
               '(lambda ()
                  (interactive)
                  (anything '(anything-c-source-dired-various-sort))))
             ))

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
(define-key dired-mode-map (kbd "C-m") 'dired-my-advertised-find-file)
(define-key dired-mode-map "^" 'dired-my-up-directory)

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
  (if (string-match "[Aa]" dired-listing-switches)
      (progn (setq dired-listing-switches "-lgGhF")
             (reload-current-dired-buffer))
    (progn (setq dired-listing-switches "-lgGhFA")
           (reload-current-dired-buffer))))
(define-key dired-mode-map (kbd "C-.") 'toggle-dired-listing-switches)

;;; OMakerootをmakefile-modeに追加
(setq auto-mode-alist (cons '("^OMakeroot$" . makefile-mode) auto-mode-alist))

;;; Delphi-mode
(autoload 'delphi-mode "delphi")
(setq auto-mode-alist
      (cons '("\\.\\(pas\\|dpr\\|dpk\\)$" . delphi-mode) auto-mode-alist))
(add-hook 'delphi-mode-hook 'turn-on-font-lock)
;; (autoload 'font-lock-mode "font-lock")
;; (autoload 'turn-on-font-lock "font-lock")
;; (setq font-lock-support-mode 'lazy-lock-mode)

;;; Sticky Buffer Mode (minor mode)、バッファを固定する
(defvar sticky-buffer-previous-header-line-format)
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (if sticky-buffer-mode
      (progn
        (set (make-local-variable 'sticky-buffer-previous-header-line-format)
             header-line-format)
        (set-window-dedicated-p (selected-window) sticky-buffer-mode))
    (set-window-dedicated-p (selected-window) sticky-buffer-mode)
    (setq header-line-format sticky-buffer-previous-header-line-format)))

;;; company-mode
(when (locate-library "company")
  (global-company-mode 1)
  (company-quickhelp-mode t)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  (if (window-system)
      (global-set-key (kbd "C-M-i") 'company-complete)
    (global-set-key (kbd "C-c M-i") 'company-complete))
  ;; (setq company-idle-delay nil) ; 自動補完をしない
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key
    company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "C-f") 'company-complete-selection)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  (define-key company-active-map (kbd "C-h") 'backward-delete-char)
  )

;;; company-quickhelp
(setq company-quickhelp-delay 1)
(setq company-transformers '(company-sort-by-backend-importance))

;;; smartparens-modeを自動オンにする
(smartparens-global-mode t)
; 一部のモードでは'での補完を行わない
(sp-local-pair '(emacs-lisp-mode) "'" nil :actions nil)
(sp-local-pair '(lisp-mode) "'" nil :actions nil)
(sp-local-pair '(sml-mode) "'" nil :actions nil)
(sp-local-pair '(inferior-sml-mode) "'" nil :actions nil)
(sp-local-pair '(tuareg-mode) "'" nil :actions nil)

;;; rainbow-modeを自動オンにする
(setq rainbow-r-colors t) ; R color listを使う
(setq rainbow-html-colors t) ; html color listを使う
(autoload 'rainbow-mode "rainbow-mode" nil t)
; 各modeでrainbow modeを起動
(add-hook 'c++-mode-hook 'rainbow-mode)
(add-hook 'arduino-mode-hook 'rainbow-mode)

;;; markdown-modeの設定
; プレビューコマンドのパス追加
; msys2でmsys/markdownパッケージをインストールする
(setq markdown-command "markdown")
; style sheetは生成HTMLと同フォルダにあるstyle.cssにする
(setq markdown-css-paths '("style.css"))
; ファイルロック機能と競合してハングするため、leoさんの松葉杖対処を導入
; https://groups.google.com/forum/#!topic/gnu.emacs.help/AIy5megeSHA
(defun leo-markdown-fontify-buffer-wiki-links-empty ()
  "Empty replacement for `markdown-fontify-buffer-wiki-links` due to hanging bug."
  (interactive))
(eval-after-load "markdown-mode"
  '(progn (fset 'markdown-fontify-buffer-wiki-links
                'leo-markdown-fontify-buffer-wiki-links-empty)))

;;; shell-modeの設定
; Emacsを起動したshellを使用する（bashからの起動を前提）
(setq explicit-shell-file-name (getenv "SHELL"))
(setq explicit-bash-args '("--login" "-i"))
; SHELL で ^M が付く場合は ^M を削除します。
(add-hook 'shell-mode-hook
          (lambda ()
            (set-buffer-process-coding-system 'undecided-dos 'sjis-unix)))
; shell-modeでのファイル名補完
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")
(bash-completion-setup)

;;; autoinsertを使ってファイル作成時にテンプレートを使う
(require 'autoinsert)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/autoinsert/")
(setq auto-insert-alist
      (nconc '(
               ("Test\\.cpp$" . ["templateTest.cpp" my-template])
               ("\\.cpp$" . ["template.cpp" my-template])
               ("\\.c$"   . ["template.c" my-template])
               ("\\.h$"   . ["template.h" my-template])
               ("\\.ino$" . ["template.ino" my-template])
               ("\\.py$" . ["template.py" my-template])
               ) auto-insert-alist))
(defvar template-replacements-alists
  '(
    ("%file%" . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%without-test%" .
     (lambda ()
       ((lambda (arg)(replace-regexp-in-string "Test$" "" arg))
        (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
    ("%file-without-ext%" .
     (lambda ()
       (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ("%include-guard%" .
     (lambda ()
       (format "%s_H"
               (upcase (file-name-sans-extension
                        (file-name-nondirectory buffer-file-name))))))
    ))
(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
            (progn
              (goto-char (point-min))
              (replace-string (car c) (funcall (cdr c)) nil)))
        template-replacements-alists)
  (goto-char (point-max))
  (message "done."))
(add-hook 'find-file-not-found-hooks 'auto-insert)

;;; twittering mode
; use master passworad compressed by GnuPG
(setq twittering-use-master-password t)
(setq twittering-private-info-file "~/.emacs.d/twittering-mode.gpg")
; use icon
(setq twittering-icon-mode t)
;(setq twittering-convert-fix-size 24)
(setq twittering-use-icon-storage t)
(setq twittering-icon-storage-file "~/.emacs.d/icons.gz")
(setq
 twittering-status-format
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
(setq twittering-timer-interval 600)
(setq twittering-number-of-tweets-on-retrieval 100)
(setq twittering-display-remaining t)
(setq twittering-initial-timeline-spec-string
      '(
        "keita44_f4/friend"
        ":replies"
        ":home"
        ))
(setq twittering-tinyurl-service 'goo.gl)
;(setq twittering-retweet-format " RT @%s %t")
(setq twittering-retweet-format " %u")
(add-hook 'twittering-mode-hook
          (lambda ()
            ; URLを青文字にする
            (set-face-attribute 'twittering-uri-face nil :foreground "blue")
            (mapc (lambda (pair)
                    (let ((key (car pair))
                          (func (cdr pair)))
                      (define-key twittering-mode-map
                        (read-kbd-macro key) func)))
                  '(
                    ("R" . twittering-native-retweet)
                    ("r" . twittering-enter)
                    ("T" . twittering-organic-retweet)
                    ("t" . twittering-update-status-interactive)
                    ("o" . twittering-goto-next-uri)
                    ("O" . twittering-push-uri-onto-kill-ring)
                    ("J" . end-of-buffer)
                    ("K" . twittering-goto-first-status)
                    ("u" . twittering-toggle-show-replied-statuses)
                    ))))
;; (add-hook 'twittering-mode-hook
;;           (lambda ()
;;             (cond ((>= (string-to-number emacs-version) 23)
;;                    (cond (window-system
;;                           (set-default-font "VL ゴシック-11")
;;                           (set-fontset-font
;;                            (frame-parameter nil 'font)
;;                            'japanese-jisx0208
;;                            '("VL ゴシック" . "unicode-bmp")
;;                           )))))))
(setq twittering-fill-column 80)
;(setq twittering-suffix-space-size 8)
(setq twittering-edit-skeleton 'inherit-mentions)

;;; Google Translate mode
(autoload 'google-translate "google-translate" nil t)
(autoload 'google-translate-smooth-ui "google-translate-smooth-ui" nil t)
(global-set-key (kbd "C-c C-t") 'google-translate-smooth-translate)
(global-set-key (kbd "C-c t") 'google-translate-query-translate)
(setq google-translate-translation-directions-alist
      '(("en" . "ja") ("ja" . "en")))

;;; popwin mode
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'bottom)
; popwin対象
(setq popwin:special-display-config
      '(
        ("*quickrun*" :stick t)
        ("*Google Translate*")
        ("*Completions*")
        ("*Ibuffer*")
        (completion-list-mode :noselect t)
        ))

;;; quickrun mode
; タイムアウトで処理を中止させない
(setq quickrun-timeout-seconds -1)

;;; python mode
(add-hook 'python-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq indent-level 4)
             (setq python-indent 4)
             (setq tab-width 4)
             (local-set-key (kbd "C-c c") 'quickrun)
             ))

;; company-jedi
;; 初回にM-x jedi:install-serverを実行する
;; pipのjediはいらない
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t) ; 定義ジャンプM-.とM-,
(add-to-list 'company-backends 'company-jedi)
(add-hook 'python-mode-hook 'jedi:setup)

;; py-autopep8
;; pythonのautopep8のパッケージを入れておくこと
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; flycheck-pyflakes
;; pipでflake8を入れておく
(require 'flycheck-pyflakes)

;;; flycheck modeの設定
;; 対応するメジャーモードでオート起動する
(global-flycheck-mode)
;; エラー箇所に背景色をつける
(set-face-background 'flycheck-error "pink")
;; キーバインド設定
(define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)
(define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)

;;; help-modeの設定
;; Alt+左右でヘルプの進む・戻るを行う、デフォルトはl/r
(define-key help-mode-map (kbd "M-<left>") 'help-go-back)
(define-key help-mode-map (kbd "M-<right>") 'help-go-forward)

;;; haxe-modeの設定
(require 'haxe-mode)
(add-hook 'haxe-mode-hook
          (function (lambda () (c-add-style "haxe" my-haxe-style t))))
(add-hook 'haxe-mode-hook
          (function
           (lambda ()
             (setq tab-width 4)
             (setq indent-tabs-mode nil)
             (setq fill-column 80)
             )))

;;; ivy
(setq ivy-count-format "(%d/%d) ")
;(setq ivy-extra-directories nil)

;;; counsel
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-r") 'counsel-command-history)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(require 'counsel)
(define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory)
(global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)

;;; swiper
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c s") 'isearch)
(setq swiper-include-line-number-in-search t)
