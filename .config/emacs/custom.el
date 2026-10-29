;;; custom.el --- emacs costum variables file -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my custom file.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(native-comp-async-report-warnings-errors nil)
 '(package-selected-packages
   '(ace-window activities add-node-modules-path aggressive-indent async
                auto-async-byte-compile auto-compile better-jumper bind-key
                biomejs-format browse-at-remote buffer-move cape cargo
                colorful-mode company company-bootstrap-icons company-bootstrap5
                company-mlton company-web consult consult-better-jumper
                consult-eglot copilot copilot-chat corfu csv-mode demap diff-hl
                diminish direnv dirvish docker docker-compose-mode
                dockerfile-mode dumb-jump edit-indirect editorconfig
                eglot-booster elm-mode embark embark-consult erblint expreg
                flycheck flycheck-color-mode-line flycheck-docker-build-checks
                flycheck-eglot flycheck-markuplint flycheck-ocaml
                flycheck-posframe flycheck-rust flycheck-smlsharp flyspell-popup
                fontaine git-commit-ts-mode git-link git-modes git-timemachine
                gnuplot-mode goggles google-translate graphviz-dot-mode
                grep-context grip-mode haxe-mode helpful highlight-indentation
                highlight-parentheses hiwin hotfuzz htmlbeautifier
                humanoid-themes ialign idle-highlight-mode imenu-list
                impatient-mode inf-ruby initchart jenkinsfile-mode keyfreq
                leaf-keywords lin lispxmp macrostep marginalia markdown-mode
                migemo mozc mozc-cand-posframe mozc-modeless nerd-icons
                nerd-icons-completion nerd-icons-corfu nerd-icons-ibuffer
                orderless origami pip-requirements popper prettier-js pyvenv
                pyvenv-auto quickrun rainbow-delimiters rainbow-mode rbs-mode
                recentf-ext reformatter rg rspec-mode rubocop rubocopfmt
                ruby-tools ruff-format rust-mode seeing-is-believing show-font
                smart-jump sml-mode solo-jazz-theme sudo-edit tempel
                tempel-collection terraform-mode treemacs treesit-auto ts-comint
                typescript-mode vc-msg vertico visual-regexp vundo wakatime-mode
                web-mode which-key yaml-mode yard-mode))
 '(package-vc-selected-packages
   '((sml-mode :url "https://github.com/yonta/sml-mode.git" :branch "add-smlsharp")))
 '(savehist-additional-variables '(kill-ring))
 '(warning-suppress-log-types '((comp) (copilot) (nativecomp)))
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#fad987"))))
 '(diff-hl-delete ((t (:background "#fa8d87"))))
 '(diff-hl-insert ((t (:background "#95fa87"))))
 '(tab-line ((t (:foreground "black" :background "gray90"))))
 '(tab-line-tab-current ((t (:inherit highlight))))
 '(tab-line-tab-inactive ((t (:background "gray84"))))
 '(tab-line-tab-modified ((t (:foreground "red")))))

;;; custom.el ends here
