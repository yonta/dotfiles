;;; ansi-term
;; コマンドラインと同じ色付けを使う
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
          "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; タブ文字を禁止してスペースを使う
(setq-default tab-width 4 indent-tabs-mode nil)

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
