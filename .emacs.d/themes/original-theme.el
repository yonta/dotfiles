(deftheme original
  "Created 2020-06-23.")

;; LightSkyBlue #bfe6ff #e0f3ff
;; #456980
;; #fad987 #ffedbf #fff6e0
;; #fa8d87 #ffc2bf #ffe2e0
;; #95fa87 #c7ffbf #e4ffe0

(custom-theme-set-faces
 'original
 '(default
    ((t (
         :family "VL ゴシック"
         :width normal
         :weight normal
         :slant normal
         :foreground "gray10"
         :background "WhiteSmoke"
         :underline nil
         :overline nil
         :strike-through nil
         :box nil
         :inverse-video nil
         :stipple nil
         :inherit nil))))
 '(shadow ((t (:foreground "gray50"))))
 '(cursor ((t (:background "#456980"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial"))
                   (t (:family "Sans Serif"))))
 '(highlight ((t (:background "#bfe6ff"))))
 '(lazy-highlight ((t (:background "#e0f3ff"))))
 '(region ((t (:background "#bfe6ff"))))
 '(secondary-selection ((t (:background "#e0f3ff"))))
 '(tooltip ((t (:foreground "gray10" :background "#fff6e0"))))
 '(success ((t (:foreground "ForestGreen"))))
 ;; 基本色、HSVでH203 S46 V98
 '(mode-line ((t (:box (:line-width -1 :color nil :style released-button)
                       :foreground "gray10" :background "LightSkyBlue"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight
   ((((class color) (min-colors 88))
     (:box (:line-width 2 :color "grey40" :style released-button)))
    (t (:inherit (highlight)))))
 '(mode-line-inactive
   ((t (:weight light :box (:line-width -1 :color "grey75" :style nil)
                :foreground "DimGray" :background "LightGray"
                :inherit (mode-line)))))
 ;; font-lock、プログラムのキーワード
 '(font-lock-keyword-face ((t (:foreground "Purple"))))
 '(font-lock-function-name-face ((t (:foreground "Blue"))))
 '(font-lock-string-face ((t (:foreground "ForestGreen"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-variable-name-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-type-face ((t (:foreground "Chocolate"))))
 '(font-lock-builtin-face ((t (:foreground "DeepPink3"))))
 '(font-lock-constant-face ((t (:foreground "SlateGray"))))
 '(font-lock-comment-face ((t (:foreground "Sienna"))))
 '(font-lock-comment-delimiter-face
   ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash
   ((t (:foreground "#ccc" :inherit (bold)))))
 '(font-lock-regexp-grouping-construct
   ((t (:foreground "#faa" :inherit (bold)))))
 '(font-lock-warning-face ((t (:inherit (error)))))
 ;; 元からあったやつ
 '(escape-glyph ((t (:inherit (font-lock-comment-face)))))
 '(homoglyph ((t (:inherit (font-lock-comment-face)))))
 '(minibuffer-prompt ((t (:foreground "blue"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line)
                        :foreground "RoyalBlue3"))))
 '(link-visited ((t (:foreground "purple" :inherit (link)))))
 '(fringe ((t (:background "WhiteSmoke"))))
 '(header-line
   ((t (:foreground "gray10" :background "gray90" :inherit (mode-line)))))
 '(match ((t (:background "#ffedbf"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 ;; パッケージカスタマイズ
 '(isearch ((t (:foreground "gray10" :background "#fa8d87"))))
 '(isearch-fail ((t (:foreground "gray10" :background "#ffc2bf"))))
 '(ivy-current-match ((t (:foreground "gray90" :inherit (cursor)))))
 '(ivy-minibuffer-match-face-1 ((t (:weight bold :background "#fa8d87"))))
 '(ivy-minibuffer-match-face-2 ((t (:weight bold :background "#fa8d87"))))
 '(ivy-minibuffer-match-face-4 ((t (:weight bold :background "#fa8d87"))))
 '(twittering-uri-face ((t :inherit (link))))
 '(git-gutter-fr:added
   ((t (:foreground "LimeGreen" :background "WhiteSmoke"
                    :inherit (git-gutter:added fringe)))))
 '(git-gutter-fr:modified
   ((t (:foreground "blue" :background "WhiteSmoke"
                    :inherit (git-gutter:modified fringe)))))
 '(vhl/default-face ((t :inherit (secondary-selection))))
 '(ahs-face ((t (:inherit (region)))))
 '(ahs-plugin-defalt-face ((t (:inherit (ahs-face)))))
 '(ahs-plugin-default-face-unfocused ((t (:inherit (ahs-plugin-defalt-face)))))
 '(ahs-plugin-whole-buffer-face ((t (:inherit ahs-plugin-defalt-face))))
 '(ahs-face-unfocused ((t (:inherit (ahs-face)))))
 '(ahs-definition-face ((t (:underline t :weight bold :inherit (ahs-face)))))
 '(ahs-definition-face-unfocused ((t (:inherit ahs-definition-face))))
 '(ahs-edit-mode-face ((t (:background "#ffc2bf"))))
 '(hl-line ((t (:inherit (lazy-highlight)))))
 '(whitespace-trailing ((t (:background "#fff6e0"))))
 '(whitespace-space ((t (:background "#ffedbf")))) ; "　"
 '(whitespace-tab ((t (:background "#ffedbf")))) ; "	"
 '(whitespace-empty ((t (:background "#ffc2bf"))))
 '(whitespace-line ((t (:background "#ffc2bf"))))
 '(line-number ((t (:background "gray90" :inherit (shadow default)))))
 '(web-mode-symbol-face ((t (:inherit (font-lock-builtin-face)))))
 '(web-mode-current-element-highlight-face ((t (:inherit (highlight)))))
 '(web-mode-current-column-highlight-face ((t (:background "gray90"))))
 '(highlight-indentation-face ((t (:background "gray90"))))
 '(flycheck-error
   ((t (:underline (:color "red" :style wave) :background "#ffc2bf"))))
 ;; mode-lineのLightSkyBlueから算出、S46、V98
 '(flycheck-color-mode-line-error-face ((t (:background "#fa8d87"))))   ; H3
 '(flycheck-color-mode-line-warning-face ((t (:background "#fad987")))) ; H43
 '(flycheck-color-mode-line-running-face
   ((t (:weight bold :foreground "gray40"))))
 '(company-tooltip ((t (:inherit (tooltip)))))
 '(company-scrollbar-bg ((t (:background "#ffedbf"))))
 '(company-scrollbar-fg ((t (:background "LightSkyBlue"))))
 '(company-tooltip-annotation ((t (:foreground "#456980"))))
 '(company-tooltip-selection ((t (:inherit (region)))))
 '(company-tooltip-mouse ((t (:inherit (secondary-selection)))))
 '(lsp-ui-doc-background ((t (:inherit (tooltip)))))
 '(lsp-ui-doc-header ((t (:inherit (diff-file-header)))))
 ;; code in lsp-ui-doc
 '(markdown-inline-code-face
   ((t (:inherit (fixed-pitch font-lock-keyword-face)))))
 ;; signature in lsp-ui-doc
 '(markdown-pre-face ((t (:inherit (markdown-inline-code-face)))))
 '(mozc-cand-overlay-even-face
   ((t (:inherit (tooltip default)))))
 '(mozc-cand-overlay-odd-face
   ((t (:inherit (tooltip default)))))
 '(mozc-cand-overlay-focused-face ((t (:inherit (region)))))
 '(mozc-cand-overlay-footer-face
   ((t (:background "#fad987" :inherit (default)))))
 '(diff-refine-added ((t (:background "#c7ffbf"))))
 '(diff-refine-changed ((t (:background "#ffedbf"))))
 '(diff-refine-removed ((t (:background "#ffc2bf"))))
 )
(provide-theme 'original)

;;; original-theme.el ends here
