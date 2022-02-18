;;; formfeeder.el --- Display ^L glyphs as horizontal lines -*- lexical-binding: t; -*-

;; Author: Jade Michael Thornton
;; URL: https://gitlab.com/thornjad/formfeeder
;; Keywords: faces
;; Version: v1.1.0

;;; Commentary:

;; This minor mode displays form feed characters (which often appear as ^L) as a single
;; horizontal line, spanning the window.
;;
;; There are a bunch of ways of attacking this problem, one of the more obscure ones is
;; manipulating the display table of every window displaying the buffer. Unfortunately
;; this approach is limited to replacing a glyph with an array of other glyphs, but
;; guaranteed to work on non-graphical displays. The other approach is putting an
;; overlay or text property over the glyph which manipulates its look. Since a face on
;; its own won't do the trick, this package uses font-lock to add text properties to the
;; page delimiter glyph. This also means that while this package is conceptually very
;; simple and non-invasive, it might not work on non-graphical displays. As a workaround
;; we also use underlining when GUI isn't available.

;; Usage:
;;
;; This package is not available from MELPA at this time. Manually install, or use
;; use-package with straight.el:
;;
;;    (use-package formfeeder
;;      :straight (:host gitlab :repo "thornjad/formfeeder" :branch "main")
;;      :config (global-formfeeder-mode))
;;
;; Alternatively to the global minor mode, call `M-x formfeeder-mode' manually or use a
;; hook:
;;
;;    (add-hook 'help-mode #'formfeeder-mode)

;; Configuration:
;;
;; Use manual or hook-based activation to enable only in the exact buffers you want, or
;; customize the way `global-formfeeder-mode' works using `formfeeder-enable-modes'. Set
;; this variable to your desired modes in a list. Here's the default value as an
;; example:
;;
;;    (setq formfeeder-enable-modes '(prog-mode text-mode help-mode eww-mode))

;; License:
;;
;; This package is based on earlier work by Vasilij Schneidermann's form-feed package.
;;
;; Copyright (c) 2019-2022 Jade Michael Thornton<br>
;; Copyright (c) 2014-2016 Vasilij Schneidermann
;;
;; This program is free software; you may redistribute it and/or modify it under the
;; terms of the GNU General Public License version 3, as published by the Free Software
;; Foundation. This program carries no warranty whatsoever, without even the implied
;; warranty of merchantability or fitness for a particular purpose. See
;; <https://www.gnu.org/licenses/> for more details.

;;; Code:


;;; variables

(defgroup formfeeder nil
  "Turn ^L glyphs into horizontal lines."
  :prefix "formfeeder-"
  :group 'faces)

(defface formfeeder-line
  '((((type graphic) (background light)) :strike-through "black")
    (((type graphic) (background dark)) :strike-through "white")
    (((type tty)) :inherit font-lock-comment-face :underline t))
  "Face for formfeeder-mode lines."
  :group 'formfeeder)

(defcustom formfeeder-line-width t
  "Width of the form feed line.

It may be one of the following values:

t: Full width.

floating point number: Ratio of full width. A value of 0.5 would use half the
width.

positive integer number: Width as measured in columns. A value of 88 would use
a 88 characters wide line.

negative integer number: Full width minus specified number of columns. A value
of -1 would leave the last column empty."
  :type '(choice (const :tag "Full width" t)
                 (float :tag "Ratio")
                 (integer :tag "Columns"))
  :group 'formfeeder)

(defsubst formfeeder--line-width ()
  "Get the line width to use."
  (cond
   ((integerp formfeeder-line-width)
    (if (>= formfeeder-line-width 0)
        formfeeder-line-width
      `(- text ,(abs formfeeder-line-width))))
   ((floatp formfeeder-line-width)
    `(,formfeeder-line-width . text))
   (t 'text)))

(defcustom formfeeder-extra-properties nil
  "List of additional text properties to add to form feeds."
  :type '(plist)
  :group 'formfeeder)

(defvar formfeeder--font-lock-face
  ;; NOTE see (info "(elisp) Search-based fontification") and the `(MATCHER . FACESPEC)'
  ;; section
  `(face formfeeder-line display (space :width ,(formfeeder--line-width))
         ,@formfeeder-extra-properties))

(defvar formfeeder--font-lock-keywords
  ;; NOTE see (info "(elisp) Search-based fontification") and the `(MATCHER .
  ;; SUBEXP-HIGHLIGHTER)' section
  `((,page-delimiter 0 formfeeder--font-lock-face t)))

(defcustom formfeeder-lighter " ^L"
  "Lighter string when formfeeder mode is active."
  :type 'string
  :group 'formfeeder
  :risky t)

(defcustom formfeeder-enable-modes
  '(prog-mode text-mode help-mode)
  "List of major modes in which `formfeeder-mode' is activated.

When using `global-formfeeder-mode', this is used to choose in which buffers to
activate.

See also `formfeeder-disable-modes'."
  :type '(repeat symbol)
  :group 'formfeeder)

(defcustom formfeeder-disable-modes
  '(eww-mode)
  "List of major modes in which `formfeeder-mode' is deactivated.

The default is to disable in `eww-mode', since search-based
fontification seems to break everything in eww.

See also `formfeeder-enable-modes'."
  :type '(repeat symbol)
  :group 'formfeeder)


;;; Functions

(defun formfeeder--add-font-lock-keywords ()
  "Add buffer-local keywords to display page delimiter lines.

Make sure the special properties involved get cleaned up on removal of the
keywords via `formfeeder-remove-font-lock-keywords'."
  (font-lock-add-keywords nil formfeeder--font-lock-keywords)
  (make-local-variable 'font-lock-extra-managed-props)
  (dolist (prop `(display ,@formfeeder-extra-properties))
    (unless (memq prop font-lock-extra-managed-props)
      (push prop font-lock-extra-managed-props))))

(defun formfeeder--remove-font-lock-keywords ()
  "Remove buffer-local keywords displaying page delimiter lines.

Reversal of `formfeeder--add-font-lock-keywords'."
  (font-lock-remove-keywords nil formfeeder--font-lock-keywords)
  (dolist (property (append '(display) formfeeder-extra-properties))
    (setq font-lock-extra-managed-props
          (delq property font-lock-extra-managed-props))))

(defun formfeeder--turn-on-mode-if-desired ()
  "Activate function `formfeeder-mode' if desired.
Activates when `major-mode' is in or derived from `formfeeder-enable-modes'."
  (when (and (apply #'derived-mode-p formfeeder-enable-modes)
             (not (apply #'derived-mode-p formfeeder-disable-modes))
             (not (bound-and-true-p enriched-mode)))
    (formfeeder-mode)))

;;;###autoload
(define-minor-mode formfeeder-mode
  "Toggle formfeeder-mode.
This minor mode displays page delimiters which usually appear as ^L glyphs on a
single line as horizontal lines spanning the entire window.

If called with a prefix argument, activate if ARG is positive, disable
otherwise.

If called from Lisp, also enables the mode if ARG is omitted or nil, and toggles
it if ARG is `toggle'."
  :lighter formfeeder-lighter
  :group 'formfeeder
  (if formfeeder-mode
      (formfeeder--add-font-lock-keywords)
    (formfeeder--remove-font-lock-keywords))

  ;; Flush changes
  (when (called-interactively-p 'interactive)
    (font-lock-flush)))

;;;###autoload
(define-globalized-minor-mode global-formfeeder-mode
  formfeeder-mode formfeeder--turn-on-mode-if-desired)

(provide 'formfeeder)
;;; formfeeder.el ends here
