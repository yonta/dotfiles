;;; original-theme.el --- Original Nameless Theme

;;; Commentary:
;; This is a original theme, but now it does not have name.

;;; Code:
(deftheme original
  "Created 2020-06-23.")

;; ベースカラーは LightSkyBlue
;; これをもとに、HSVで設計している
;;
;; 色相 Hue
;; red: 3 green: 112.7 blue: 203 yellow: 43 purple: 293.2 orange: 23
;; 彩度 Saturation 3段階
;; 1: 46 2: 25 3: 12
;; 明度 Value
;; 98固定

(let* ((dark1 "#1a1a1a")  ; 0, 0, 10 gray10
       (dark2 "#808080")  ; 0, 0, 50 gray50
       (dark3 "#cccccc")  ; 0, 0, 80
       (light1 "#f5f5f5") ; 0, 0, 96 WhiteSmoke
       (light2 "#e6e6e6") ; 0, 0, 90 gray90
       (blue1 "#87cefa")
       (blue2 "#bbe2fa")
       (blue3 "#dceefa")
       (dark-blue "#0869a6")
       (yellow1 "#fad987")
       (yellow2 "#fae8bb")
       (yellow3 "#faf1dc")
       (red1 "#fa8d87")
       (red2 "#fabfbb")
       (red3 "#fadddc")
       (green1 "#95fa87")
       (green2 "#c3fabb")
       (green3 "#e0fadc")
       (purple1 "#ed87fa")
       (purple2 "#f3bbfa")
       (purple3 "#f7dcfa")
       (orange1 "#fab387")
       (orange2 "#fad3be")
       (orange3 "#fae7dc"))
  (custom-theme-set-faces
   'original
   `(default
     ((t (
          :family "VL ゴシック"
          :width normal
          :weight normal
          :slant normal
          :foreground ,dark1
          :background ,light1
          :underline nil
          :overline nil
          :strike-through nil
          :box nil
          :inverse-video nil
          :stipple nil
          :inherit nil))))
   `(shadow ((t (:foreground ,dark2))))
   `(cursor ((t (:background ,dark-blue))))
   '(fixed-pitch ((t (:family "Monospace"))))
   '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial"))
                     (t (:family "Sans Serif"))))
   `(highlight ((t (:background ,blue2))))
   `(lazy-highlight ((t (:background ,blue3))))
   `(region ((t (:background ,blue2))))
   `(secondary-selection ((t (:background ,blue3))))
   `(tooltip ((t (:foreground ,dark1 :background ,light2))))
   `(success ((t (:foreground "ForestGreen"))))
   ;; 基本色、HSVでH203 S46 V98
   `(mode-line ((t (:box (:line-width -1 :color nil :style released-button)
                         :foreground ,dark1 :background ,blue1))))
   '(mode-line-buffer-id ((t (:weight bold))))
   '(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-highlight
     ((((class color) (min-colors 88))
       (:box (:line-width 2 :color ,dark2 :style released-button)))
      (t (:inherit (highlight)))))
   `(mode-line-inactive
     ((t (:weight light :box (:line-width -1 :color ,dark3 :style nil)
                  :foreground ,dark2 :background ,dark3
                  :inherit (mode-line)))))
   ;; font-lock、プログラムのキーワード
   '(font-lock-keyword-face ((t (:foreground "Purple"))))
   '(font-lock-function-name-face ((t (:foreground "Blue"))))
   '(font-lock-string-face ((t (:foreground "ForestGreen"))))
   '(font-lock-doc-face ((t (:foreground "DodgerBlue"))))
   '(font-lock-variable-name-face ((t (:foreground "DodgerBlue"))))
   '(font-lock-type-face ((t (:foreground "Chocolate"))))
   '(font-lock-builtin-face ((t (:foreground "DeepPink3"))))
   '(font-lock-constant-face ((t (:foreground "Sienna"))))
   '(font-lock-comment-face ((t (:foreground "#667580"))))
   '(font-lock-comment-delimiter-face ((t (:foreground "Sienna"))))
   '(font-lock-negation-char-face ((t nil)))
   '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash
     ((t (:foreground ,dark3 :inherit (bold)))))
   `(font-lock-regexp-grouping-construct
     ((t (:foreground ,red1 :inherit (bold)))))
   '(font-lock-warning-face ((t (:inherit (error)))))
   ;; 元からあったやつ
   '(escape-glyph ((t (:inherit (font-lock-comment-face)))))
   '(homoglyph ((t (:inherit (font-lock-comment-face)))))
   `(minibuffer-prompt ((t (:foreground ,dark-blue :background ,blue3))))
   '(button ((t (:inherit (link)))))
   '(link ((t (:underline (:color foreground-color :style line)
                          :foreground "RoyalBlue3"))))
   '(link-visited ((t (:foreground "purple" :inherit (link)))))
   `(fringe ((t (:background ,light1))))
   `(header-line
     ((t (:foreground ,dark1 :background ,light2 :inherit (mode-line)))))
   `(match ((t (:background ,yellow2))))
   '(next-error ((t (:inherit (region)))))
   '(query-replace ((t (:inherit (isearch)))))
   ;; パッケージカスタマイズ
   `(isearch ((t (:foreground ,dark1 :background ,red1))))
   `(isearch-fail ((t (:foreground ,dark1 :background ,red2))))
   `(ivy-current-match ((t (:foreground ,light2 :inherit (cursor)))))
   `(ivy-minibuffer-match-face-1 ((t (:weight bold :background ,red1))))
   `(ivy-minibuffer-match-face-2 ((t (:weight bold :background ,red1))))
   `(ivy-minibuffer-match-face-4 ((t (:weight bold :background ,red1))))
   '(twittering-uri-face ((t :inherit (link))))
   `(git-gutter-fr:added
     ((t (:foreground ,green1 :background ,light1
                      :inherit (git-gutter:added fringe)))))
   `(git-gutter-fr:modified
     ((t (:foreground ,blue1 :background ,light1
                      :inherit (git-gutter:modified fringe)))))
   `(git-gutter-fr:deleted
     ((t (:foreground ,red1 :background ,light1
                      :inherit (git-gutter:modified fringe)))))
   '(vhl/default-face ((t :inherit (secondary-selection))))
   '(ahs-face ((t (:inherit (region)))))
   '(ahs-plugin-defalt-face ((t (:inherit (ahs-face)))))
   '(ahs-plugin-default-face-unfocused ((t (:inherit (ahs-plugin-defalt-face)))))
   '(ahs-plugin-whole-buffer-face ((t (:inherit ahs-plugin-defalt-face))))
   '(ahs-face-unfocused ((t (:inherit (ahs-face)))))
   '(ahs-definition-face ((t (:underline t :weight bold :inherit (ahs-face)))))
   '(ahs-definition-face-unfocused ((t (:inherit ahs-definition-face))))
   `(ahs-edit-mode-face ((t (:background ,red2))))
   '(hl-line ((t (:inherit (lazy-highlight)))))
   `(whitespace-trailing ((t (:background ,yellow2))))
   `(whitespace-space ((t (:background ,green1)))) ; "　"
   `(whitespace-tab ((t (:background ,orange1)))) ; "	"
   `(whitespace-empty ((t (:background ,red2))))
   `(whitespace-line ((t (:background ,red2))))
   `(line-number ((t (:background ,light2 :inherit (shadow default)))))
   '(web-mode-symbol-face ((t (:inherit (font-lock-builtin-face)))))
   '(web-mode-current-element-highlight-face ((t (:inherit (highlight)))))
   `(web-mode-current-column-highlight-face ((t (:background ,light2))))
   `(highlight-indentation-face ((t (:background ,light2))))
   `(flycheck-error
     ((t (:underline (:color "red" :style wave) :background ,red2))))
   `(flycheck-color-mode-line-error-face ((t (:background ,red1))))
   `(flycheck-color-mode-line-warning-face ((t (:background ,yellow1))))
   `(flycheck-color-mode-line-running-face
     ((t (:weight bold :foreground ,dark2))))
   '(company-tooltip ((t (:inherit (tooltip)))))
   `(company-scrollbar-bg ((t (:background ,blue3))))
   `(company-scrollbar-fg ((t (:background ,blue1))))
   `(company-tooltip-annotation ((t (:foreground ,dark-blue))))
   '(company-tooltip-selection ((t (:inherit (region)))))
   '(company-tooltip-mouse ((t (:inherit (secondary-selection)))))
   '(company-tooltip-search ((t (:inherit (match)))))
   '(company-tooltip-search-selection ((t (:inherit (match)))))
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
   `(mozc-cand-overlay-footer-face
     ((t (:background ,yellow1 :inherit (default)))))
   `(diff-refine-added ((t (:background ,green2))))
   `(diff-refine-changed ((t (:background ,yellow2))))
   `(diff-refine-removed ((t (:background ,red2))))
   ))
(provide-theme 'original)

;;; original-theme.el ends here
