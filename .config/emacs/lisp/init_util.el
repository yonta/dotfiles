;;; init_util --- util for init -*- lexical-binding: t -*-

;;; Commentary:
;; This file has util functions for Emacs init.

;;; Code:

(defun call-with-region-or-line (func-symbol)
  "Call FUNC-SYMBOL with marked region or current line.

If the region is active, beggining and end of region is used for the function
 arguments, othewise current line is used."
  (if (region-active-p)
      (funcall func-symbol (region-beginning) (region-end))
    (let* ((begin (line-beginning-position))
           (end (1+ (line-end-position))))
      (funcall func-symbol begin end))))

(defmacro p (form)
  "Output pretty `macroexpand-1'ed given FORM."
  `(progn
     (pp (macroexpand-1 ',form))
     nil))

;;; init_util.el ends here
