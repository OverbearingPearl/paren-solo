;;; pearl-paren-style.el --- Toggle Lisp paren styles between compact and dangling  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: Kimi:kimi-k2.5, DeepSeek:deepseek-v3.2, Claude:claude-sonnet-4.6
;; URL: https://github.com/OverbearingPearl/pearl-paren-style
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: lisp, tools, convenience, parentheses, formatting
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Toggle Lisp code between compact (community standard) and dangling
;; (each closing parenthesis on its own line) styles.
;;
;; The dangling style may help AI-assisted editing tools (Aider,
;; Copilot, etc.) by isolating each closing delimiter on its own line.
;; This is an experimental hypothesis — see README.md for details.
;;
;; Features:
;;
;; - Convert between compact and dangling styles
;; - Annotation display for dangling style showing opening \(parenthesis location
;;   with distance-based color gradient (closer = fainter, 20 lines = full color)
;; - Preserves single-line expressions like (foo)
;; - Handles inline and trailing comments
;; - Process regions, files, or Dired selections
;; - Validates parenthesis balance before conversion
;; - Convert annotations to permanent comments for AI tool compatibility
;; - Restore annotations from comments
;; - Comprehensive test suite
;; - Context-aware DWIM (Do What I Mean) command
;;
;; Core Commands:
;; - pearl-paren-style-toggle: Auto-detect and toggle style
;; - pearl-paren-style-compact: Force compact style
;; - pearl-paren-style-dangling: Force dangling style
;; - pearl-paren-style-convert: Interactive style selection
;;
;; Region Operations:
;; - pearl-paren-style-compact-region: Convert region to compact style
;; - pearl-paren-style-dangling-region: Convert region to dangling style
;; - pearl-paren-style-toggle-region: Toggle style in region
;; - pearl-paren-style-convert-region: Interactive style selection for region
;;
;; File Operations:
;; - pearl-paren-style-compact-files: Convert files/directories to compact style
;; - pearl-paren-style-dangling-files: Convert files/directories to dangling style
;; - pearl-paren-style-convert-files: Interactive style selection for files
;;
;; Annotation and Comment Conversion (The AI Bridge):
;; - pearl-paren-style-annotations-to-comments: Convert overlays to permanent comments
;; - pearl-paren-style-comments-to-annotations: Restore overlays from comments
;;
;; Smart Operations:
;; - pearl-paren-style-dwim: Context-aware conversion (region/files/buffer)
;;
;; Testing:
;; - pearl-paren-style-run-tests: Run the full test suite
;;
;; Suggested workflow for AI coding:
;; 1. M-x pearl-paren-style-dangling
;; 2. (Optional) M-x pearl-paren-style-annotations-to-comments
;;    - Makes bracket correspondence visible to AI tools during generation
;;    - Trade-off: increases token usage; skip if AI handles dangling style well
;;    - Only closing parens at least `pearl-paren-style-annotation-min-distance'
;;      lines from their opener are annotated (default 5), reducing token cost
;; 3. Generate/modify code with AI tools
;; 4. M-x pearl-paren-style-compact before committing
;;
;; For detailed examples and configuration, see README.md.

;;; Code:

;; Silence byte-compiler warnings about dired and ert functions
(declare-function dired-get-marked-files "dired")
(declare-function ert-delete-all-tests "ert")

(require 'cl-lib)
(require 'color)

(defgroup pearl-paren-style nil
  "Toggle Lisp paren style."
  :group 'lisp)

(defface pearl-paren-style-annotation
  '((t :inherit font-lock-comment-face))
  "Face for parenthesis annotations."
  :group 'pearl-paren-style)

(defcustom pearl-paren-style-default 'compact
  "Default style when detection is ambiguous."
  :type '(choice (const compact) (const dangling)))

(defcustom pearl-paren-style-show-annotations t
  "Whether to show annotations in dangling style.
When non-nil, closing parentheses in dangling style will display
annotations showing the corresponding opening parenthesis location.
Annotations fade based on distance from the opening parenthesis
\(closer = fainter), reaching full color at 20 lines of separation."
  :type 'boolean
  :group 'pearl-paren-style)

(defcustom pearl-paren-style-annotation-min-distance 5
  "Minimum line distance to show annotation for a closing parenthesis.
Closing parentheses whose opening counterpart is fewer than this many
lines away will not receive an annotation, reducing token usage when
converting annotations to comments for AI tools."
  :type 'integer
  :group 'pearl-paren-style)

(defconst pearl-paren-style--annotation-arrow " ← "
  "Arrow marker used in annotation overlays and comments.  Internal protocol.")

(defconst pearl-paren-style--annotation-end "⟩"
  "End marker for annotation text.  Internal protocol.")

(defconst pearl-paren-style--annotation-comment-prefix ";; ← "
  "Full prefix for annotation comments.  Internal protocol.")

(defvar-local pearl-paren-style--annotation-overlays nil
  "List of overlays used for annotation display.")

(defun pearl-paren-style--in-string-or-comment-p (&optional pos)
  "Return non-nil if point is inside a string or comment.
If POS is provided, check at that position instead of current point."
  (save-excursion
    (when pos (goto-char pos))
    (let ((state (syntax-ppss)))
      (or (nth 3 state)  ; inside string
          (nth 4 state)  ; inside comment
          ;; Check for character literal starting with ?\
          (save-excursion
            (and (>= (point) 2)
                 (eq (char-before) ?\\)
                 (eq (char-before (1- (point))) ??)))
          ;; Check for multi-line comment #| ... |#
          (save-excursion
            (let ((pos (point)))
              (when (re-search-backward "#|" nil t)
                (let ((start (point)))
                  (goto-char start)
                  (when (re-search-forward "|#" nil t)
                    (<= pos (point)))))))))))

(defun pearl-paren-style--line-has-comment-p ()
  "Return non-nil if current line has a comment (starting with ;)."
  (save-excursion
    (beginning-of-line)
    (let ((line-end (line-end-position)))
      (when (search-forward ";" line-end t)
        ;; Verify it's really a comment, not inside string or character literal
        (nth 4 (syntax-ppss))))))

(defun pearl-paren-style--should-dangle-p (open-pos closing-pos)
  "Return t if the paren at CLOSING-POS should be dangled relative to OPEN-POS.
OPEN-POS is the position of the opening parenthesis.
CLOSING-POS is the position of the closing parenthesis."
  (/= (line-number-at-pos open-pos) (line-number-at-pos closing-pos)))

(defun pearl-paren-style--get-annotation (closing-pos)
  "Get annotation for closing parenthesis at CLOSING-POS.
Returns (STRING . LINE-DISTANCE) where LINE-DISTANCE is line difference, or nil."
  (save-excursion
    (goto-char closing-pos)
    (let ((open-pos (condition-case nil
                       ;; Use scan-lists with depth 1 to find matching opening parenthesis
                       (scan-lists (point) -1 1)
                     (scan-error nil))))
      (when open-pos
        (let ((open-line (line-number-at-pos open-pos))
              (close-line (line-number-at-pos closing-pos)))
          (when (/= open-line close-line)
            (let ((open-col (save-excursion
                              (goto-char open-pos)
                              (current-column)))
                  (open-text (save-excursion
                               (goto-char open-pos)
                               (buffer-substring
                                (point)
                                (min (line-end-position)
                                     (+ (point) 20))))))
              (cons (format "%s%d:%d %s%s" pearl-paren-style--annotation-arrow open-line open-col (string-trim-right open-text) pearl-paren-style--annotation-end)
                    (- close-line open-line)))))))))

(defun pearl-paren-style--annotation-color-for-distance (line-distance)
  "Calculate annotation color based on LINE-DISTANCE (lines from opening paren).
Closer distance = blend more toward background (less visible)."
  ;; In batch mode, return a simple default color to avoid color parsing issues
  (if noninteractive
      "#888888"  ; Default gray color for batch mode
    (let* ((frame (selected-frame))
           (base-color (face-attribute 'font-lock-comment-face :foreground frame))
           (bg-color (face-attribute 'default :background frame)))
      (when (eq base-color 'unspecified)
        (error "font-lock-comment-face foreground color is unspecified"))
      (when (eq bg-color 'unspecified)
        (error "Default face background color is unspecified"))
      (let* ((threshold 20.0)
             (ratio (min 1.0 (/ (float line-distance) threshold)))
             (base-rgb (color-name-to-rgb base-color))
             (bg-rgb (color-name-to-rgb bg-color)))
        (cl-assert (and base-rgb bg-rgb)
                   nil "pearl-paren-style: failed to parse colors: base=%s bg=%s"
                   base-color bg-color)
        (apply #'color-rgb-to-hex
               (cl-mapcar (lambda (b g)
                            (+ (* b ratio) (* g (- 1.0 ratio))))
                          base-rgb bg-rgb))))))

(defun pearl-paren-style--annotation-to-comment (closing-pos)
  "Convert annotation at CLOSING-POS to comment text.
Returns t if conversion was performed, nil otherwise."
  (when (and pearl-paren-style-show-annotations
             (not (pearl-paren-style--in-string-or-comment-p closing-pos)))
    (let ((result (pearl-paren-style--get-annotation closing-pos)))
      (when (and result
                 (>= (cdr result) pearl-paren-style-annotation-min-distance))
        (let* ((annotation (car result))
               (trimmed-annotation (string-trim-right annotation))
               (comment-text (concat pearl-paren-style--annotation-comment-prefix
                                     (substring trimmed-annotation (length pearl-paren-style--annotation-arrow)))))
          (save-excursion
            (goto-char closing-pos)
            (forward-char 1)
            (let* ((space-start (point))
                   (_ (skip-chars-forward " \t"))
                   (line-end (line-end-position))
                   (has-comment (< (point) line-end))
                   (orig-leading-spaces (buffer-substring space-start (point))))
              (cond
               ((and has-comment
                     (looking-at (regexp-quote pearl-paren-style--annotation-comment-prefix)))
                (delete-region space-start line-end)
                (insert "  " comment-text))
               (has-comment
                (let ((orig-comment (buffer-substring (point) line-end)))
                  (delete-region space-start line-end)
                  (insert "  " comment-text
                          (if (string-empty-p orig-leading-spaces) "  " orig-leading-spaces)
                          orig-comment)))
               (t
                (delete-region space-start line-end)
                (insert "  " comment-text)))))
          t)))))

(defun pearl-paren-style--comment-to-annotation (closing-pos comment-text)
  "Convert comment text back to annotation overlay at CLOSING-POS.
CLOSING-POS is the position of the closing parenthesis.
COMMENT-TEXT is the text of the annotation comment containing:
  - The line number and column of opening parenthesis
  - The opening parenthesis text
Returns the created overlay or nil."
  (when (and pearl-paren-style-show-annotations
             (not (pearl-paren-style--in-string-or-comment-p closing-pos)))
    ;; Parse comment text to extract annotation information
    (when (string-match (concat "\\([0-9]+\\):\\([0-9]+\\) \\(.*" (regexp-quote pearl-paren-style--annotation-end) "\\)") comment-text)
      (let* ((open-line (string-to-number (match-string 1 comment-text)))
             (_open-col (string-to-number (match-string 2 comment-text))) ; unused
             (_open-text (match-string 3 comment-text)) ; unused
             (close-line (line-number-at-pos closing-pos))
             (line-distance (- close-line open-line))
             (annotation-text (concat " ← " comment-text))
             (color (pearl-paren-style--annotation-color-for-distance line-distance))
             (ov (make-overlay closing-pos (1+ closing-pos))))
        (overlay-put ov 'category 'pearl-paren-style-annotation)
        (overlay-put ov 'after-string
                     (propertize annotation-text 'face `(:foreground ,color)))
        (overlay-put ov 'pearl-paren-style-closing-pos closing-pos)
        ov))))

(defun pearl-paren-style--find-annotation-comments ()
  "Find all lines with annotation comment pattern.
Returns a list of (closing-pos . comment-text) pairs.
comment-text contains only the annotation detail, not trailing user comments."
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
                  (re-search-forward (regexp-quote pearl-paren-style--annotation-comment-prefix)
                                     nil t))
        (let ((comment-start (match-beginning 0))
              (comment-end (match-end 0)))
          (save-excursion
            (goto-char comment-start)
            (skip-chars-backward " \t")
            (when (and (not (bobp))
                       (= (char-before) ?\)))
              (let* ((closing-pos (1- (point)))
                     (line-end (line-end-position))
                     (rest (buffer-substring comment-end line-end))
                     ;; annotation detail: "LINE:COL OPEN-TEXT⟩" possibly followed by "  ; user-comment"
                     ;; Split at "⟩" boundary
                     (annotation-text
                      (if (string-match (concat "\\(.*?" (regexp-quote pearl-paren-style--annotation-end) "\\)") rest)
                          (match-string 1 rest)
                        rest))
                     (comment-text annotation-text))
                (when (not (pearl-paren-style--in-string-or-comment-p closing-pos))
                  (push (cons closing-pos comment-text) result))))))))
    (nreverse result)))

(defun pearl-paren-style--annotation-enabled-p ()
  "Return non-nil if annotations are enabled.
This is a convenience function used by tests."
  pearl-paren-style-show-annotations)

(defun pearl-paren-style--create-annotation-overlay (closing-pos)
  "Create annotation overlay for closing parenthesis at CLOSING-POS.
Returns the overlay or nil if no annotation needed."
  (when (and pearl-paren-style-show-annotations
             (not (pearl-paren-style--in-string-or-comment-p closing-pos)))
    (let ((result (pearl-paren-style--get-annotation closing-pos)))
      (when (and result
                 (>= (cdr result) pearl-paren-style-annotation-min-distance))
        (let* ((annotation (car result))
               (line-distance (cdr result))
               (ov (make-overlay closing-pos (1+ closing-pos)))
               (color (pearl-paren-style--annotation-color-for-distance line-distance)))
          (overlay-put ov 'category 'pearl-paren-style-annotation)
          (overlay-put ov 'after-string
                       (propertize annotation 'face `(:foreground ,color)))
          (overlay-put ov 'pearl-paren-style-closing-pos closing-pos)
          ov)))))

(defun pearl-paren-style--clear-annotations ()
  "Remove all annotation overlays."
  (mapc 'delete-overlay pearl-paren-style--annotation-overlays)
  (setq pearl-paren-style--annotation-overlays nil)
  (remove-overlays (point-min) (point-max) 'category 'pearl-paren-style-annotation))

(defun pearl-paren-style--on-after-revert ()
  "Clear stale annotation overlays after buffer revert."
  (pearl-paren-style--clear-annotations))


(defun pearl-paren-style--update-annotations-full ()
  "Create annotations for all closing parentheses in buffer."
  (pearl-paren-style--clear-annotations)
  (when pearl-paren-style-show-annotations
    (save-excursion
      (goto-char (point-max))
      (while (search-backward ")" nil t)
        (unless (pearl-paren-style--in-string-or-comment-p)
          (let ((ov (pearl-paren-style--create-annotation-overlay (point))))
            (when ov
              (push ov pearl-paren-style--annotation-overlays))))))))

(defun pearl-paren-style--dangle-target-indent (open-pos)
  "Calculate target indentation column for OPEN-POS.
OPEN-POS is the position of the opening parenthesis."
  (save-excursion
    (goto-char open-pos)
    (current-column)))

(defun pearl-paren-style--dangle-is-correct-p (line-start current-col target-col)
  "Check if paren is already correctly dangled.
LINE-START is the position at the beginning of the line.
CURRENT-COL is the current column of the closing paren.
TARGET-COL is the target column for dangling."
  (and (looking-back "^\\s-*" line-start)
       (= current-col target-col)))

(defun pearl-paren-style--dangle-fix-indent (_line-start target-col)
  "Fix indentation for a paren already on its own line.
_LINE-START is the position at the beginning of the line (unused).
TARGET-COL is the target column for indentation."
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to target-col)))

(defun pearl-paren-style--dangle-move-to-new-line (_open-pos closing-pos target-col)
  "Move paren at CLOSING-POS to a new line with TARGET-COL indent.
Handles trailing comments.
_OPEN-POS is the position of the opening parenthesis (unused).
CLOSING-POS is the position of the closing parenthesis.
TARGET-COL is the target column for indentation."
  (let* ((end-of-line (line-end-position))
         comment-text)
    (save-excursion
      (goto-char closing-pos)
      (forward-char)
      (let ((after-paren (point)))
        (skip-chars-forward " \t" end-of-line)
        (when (and (< (point) end-of-line)
                   (= (char-after) ?\;))
          (setq comment-text (buffer-substring after-paren end-of-line))
          (delete-region after-paren end-of-line))))
    (save-excursion
      (goto-char closing-pos)
      (delete-char 1)
      (delete-region (point) (line-end-position))
      (insert "\n")
      (indent-to target-col)
      (insert ")")
      (when comment-text
        (insert (if (string-match-p "^[ \t]" comment-text)
                    comment-text
                  (concat " " comment-text)))))))

(defun pearl-paren-style--line-has-code-p ()
  "Return non-nil if current line has actual code (not just parens and whitespace)."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (while (and (not (eolp))
                (or (= (char-after) ?\()
                    (= (char-after) ?\))))
      (forward-char)
      (skip-chars-forward " \t"))
    (not (eolp))))

(defun pearl-paren-style--calculate-compact-indent (pos)
  "Calculate proper indentation for line at POS in compact style.
Uses `calculate-lisp-indent' to determine the correct indentation
column for the current line, based on Lisp syntax.
POS is the position in the buffer."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (calculate-lisp-indent)))

(defun pearl-paren-style--check-balanced-p (&optional beg end)
  "Check if parentheses are balanced in region from BEG to END.
Return t if balanced, nil otherwise.
BEG is the beginning position of the region.
END is the end position of the region."
  (condition-case nil
      (progn
        (if (and beg end)
            (save-restriction
              (narrow-to-region beg end)
              (check-parens))
          (check-parens))
        t)
    (error nil)))

(defun pearl-paren-style--classify-closing-paren (closing-pos)
  "Classify the closing paren at CLOSING-POS.
Returns \\='dangling, \\='compact, or nil if it should be ignored.
Examples: single-line or backquote forms are ignored.
CLOSING-POS is the position of the closing parenthesis."
  (let ((open-pos (condition-case nil
                     (save-excursion
                       (goto-char closing-pos)
                       (scan-lists (point) -1 1))
                   (scan-error nil))))
    (when open-pos
      ;; Ignore backquote forms like `(foo)
      (unless (save-excursion
                (goto-char open-pos)
                (eq (char-before) ?\`))
        ;; Only classify if it spans multiple lines
        (when (/= (line-number-at-pos open-pos)
                  (line-number-at-pos closing-pos))
          (save-excursion
            (goto-char closing-pos)
            (beginning-of-line)
            (skip-chars-forward " \t")
            (if (= (point) closing-pos)
                'dangling
              'compact)))))))

(defun pearl-paren-style--detect ()
  "Return current style: `compact', `dangling', or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((dangling 0)
          (compact 0)
          (has-parens nil))
      (while (search-forward ")" nil t)
        (unless (pearl-paren-style--in-string-or-comment-p)
          (setq has-parens t)
          (let ((style (pearl-paren-style--classify-closing-paren (match-beginning 0))))
            (when style
              (pcase style
                ('dangling (cl-incf dangling))
                ('compact (cl-incf compact)))))))
      (cond ((> dangling 0) 'dangling)
            ((> compact 0) 'compact)
            (has-parens 'compact)
            (t nil)))))

(defun pearl-paren-style--to-dangling ()
  "Convert buffer to dangling style.
Single-line parens like (foo) remain unchanged.
Creates annotations if `pearl-paren-style-show-annotations' is non-nil."
  (pearl-paren-style--clear-annotations) ; Clear any existing state
  (add-hook 'after-revert-hook #'pearl-paren-style--on-after-revert nil t)
  (save-excursion
    (goto-char (point-max))
    (while (search-backward ")" nil t)
      (unless (pearl-paren-style--in-string-or-comment-p)
        (let ((closing-pos (point))
              (line-start (line-beginning-position))
              (current-col (current-column)))
          (let ((open-pos (condition-case nil
                             (scan-lists (point) -1 1)
                           (scan-error nil))))
            (when (and open-pos
                       (pearl-paren-style--should-dangle-p open-pos closing-pos))
              (let ((target-col (pearl-paren-style--dangle-target-indent open-pos)))
                (cond
                 ;; Already correct: do nothing
                 ((pearl-paren-style--dangle-is-correct-p line-start current-col target-col)
                  nil)
                 ;; At line start but wrong indent: fix indent
                 ((looking-back "^\\s-*" line-start)
                  (pearl-paren-style--dangle-fix-indent line-start target-col))
                 ;; Not at line start: move to new line
                 (t
                  (pearl-paren-style--dangle-move-to-new-line open-pos closing-pos target-col))))))))))
  (pearl-paren-style--update-annotations-full)
  (message "Converted to dangling style%s"
           (if pearl-paren-style-show-annotations " with annotations" "")))

(defun pearl-paren-style--is-dangling-p (closing-pos)
  "Return t if the paren at CLOSING-POS is dangling (on its own line).
CLOSING-POS is the position of the closing parenthesis."
  (let ((open-pos (condition-case nil
                     (scan-lists closing-pos -1 1)
                   (scan-error nil))))
    (and open-pos
         (/= (line-number-at-pos open-pos) (line-number-at-pos closing-pos))
         (save-excursion
           (goto-char closing-pos)
           (beginning-of-line)
           (skip-chars-forward " \t")
           (= (point) closing-pos)))))

(defun pearl-paren-style--prev-line-has-comment-p (line-start)
  "Return t if the line before LINE-START has a comment.
LINE-START is the position at the beginning of the line."
  (save-excursion
    (goto-char line-start)
    (forward-line -1)
    (pearl-paren-style--line-has-comment-p)))

(defun pearl-paren-style--compact-fix-indent (pos)
  "Fix indentation for the line containing POS.
Returns t if indentation was changed, nil otherwise.
POS is the position in the buffer."
  (let* ((target-indent (pearl-paren-style--calculate-compact-indent pos))
         (current-indent
          (save-excursion
            (goto-char pos)
            (beginning-of-line)
            (skip-chars-forward " \t")
            (current-column))))
    (when (and target-indent
               (/= current-indent target-indent))
      (save-excursion
        (goto-char pos)
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to target-indent))
      t)))

(defun pearl-paren-style--compact-merge-to-prev-line (closing-pos)
  "Move the dangling paren at CLOSING-POS to the end of the previous line.
Handles trailing comments.
CLOSING-POS is the position of the closing parenthesis."
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (comment-text nil)
         (comment-leading-spaces "")
         (rest-of-line (buffer-substring closing-pos line-end)))

    ;; Extract comment if exists
    (save-excursion
      (goto-char closing-pos)
      (forward-char)
      (let ((space-start (point)))
        (skip-chars-forward " \t" line-end)
        (when (and (< (point) line-end)
                   (= (char-after) ?\;))
          (setq comment-leading-spaces (buffer-substring space-start (point)))
          (setq comment-text (buffer-substring (point) line-end))
          (setq rest-of-line ")"))))

    (delete-region line-start (line-beginning-position 2))

    (save-excursion
      (goto-char line-start)
      (forward-line -1)
      (end-of-line)
      (insert rest-of-line)
      (when comment-text
        (insert (if (string-empty-p comment-leading-spaces)
                    " "
                  comment-leading-spaces)
                comment-text)))))

(defun pearl-paren-style--cleanup-trailing-blank-lines ()
  "Remove trailing blank lines, ensuring file ends with a single newline."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward "\n")
    (unless (eobp)
      (delete-region (point) (point-max))
      (insert "\n"))))

(defun pearl-paren-style--to-compact ()
  "Convert buffer to compact style."
  (pearl-paren-style--clear-annotations)
  (remove-hook 'after-revert-hook #'pearl-paren-style--on-after-revert t)
  (save-excursion
    (goto-char (point-max))
    (let ((changed t))
      (while changed
        (setq changed nil)
        (while (search-backward ")" nil t)
          (unless (pearl-paren-style--in-string-or-comment-p)
            (let ((closing-pos (point))
                  (line-start (line-beginning-position)))
              (when (and (looking-back "^\\s-*" line-start)
                         (pearl-paren-style--is-dangling-p closing-pos))
                (if (pearl-paren-style--prev-line-has-comment-p line-start)
                    (when (pearl-paren-style--compact-fix-indent closing-pos)
                      (setq changed t))
                  (pearl-paren-style--compact-merge-to-prev-line closing-pos)
                  (setq changed t))))))
        (when changed
          (goto-char (point-max)))))
    (pearl-paren-style--cleanup-trailing-blank-lines))
  (message "Converted to compact style"))

(defun pearl-paren-style--process-file (file style)
  "Process FILE converting to STYLE.
Returns (success . file) if successful, (error . message) for external errors.
Signals internal logic errors directly.
FILE is the path to the file to process.
STYLE is either \\='compact or \\='dangling."
  (cond
   ((not (file-exists-p file))
    (cons 'error (format "File not found: %s" file)))
   ((not (file-readable-p file))
    (cons 'error (format "File not readable: %s" file)))
   (t
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (emacs-lisp-mode)
          ;; Balanced check is business validation, return error status if failed
          (if (pearl-paren-style--check-balanced-p)
              (progn
                ;; Core conversion logic: internal, fail fast on errors
                (pcase style
                  ('compact (pearl-paren-style--to-compact))
                  ('dangling (pearl-paren-style--to-dangling)))
                ;; File write is external IO, may signal file-error
                (write-region (point-min) (point-max) file nil 'silent)
                (cons 'success file))
            (cons 'error (format "Unbalanced parentheses in file: %s" file))))
      ;; Only catch file IO errors; let internal logic errors bubble up
      (file-error (cons 'error (format "IO error on %s: %s" file (error-message-string err))))))))

(defun pearl-paren-style--collect-el-files (files)
  "Collect all .el files from FILES list.
If a directory is included, recursively collect all .el files in it.
Follows symbolic links.
FILES is a list of file paths."
  (cl-loop for file in files
           append (if (file-directory-p file)
                      (directory-files-recursively file "\\.el$" t)  ; Add t to follow symlinks
                    (when (string-match "\\.el$" file)
                      (list file)))))

(defun pearl-paren-style--read-files ()
  "Read file selection interactively.
Return list of files, preferring Dired marked files when in Dired mode."
  (if (and (derived-mode-p 'dired-mode)
           (dired-get-marked-files))
      (dired-get-marked-files)
    (let ((selected (read-file-name "Select files (wildcards allowed): "
                                    nil nil t nil
                                    (lambda (name)
                                      (or (file-directory-p name)
                                          (string-match "\\.el$" name))))))
      (if (stringp selected)
          (if (file-directory-p selected)
              (list selected)
            (list selected))
        selected))))

(defun pearl-paren-style--with-el-files (files style)
  "Process .el FILES with STYLE, handling collection, confirmation, and reporting.
STYLE is \\='compact or \\='dangling.
FILES is a list of file paths."
  (let ((el-files (pearl-paren-style--collect-el-files files)))
    (when (null el-files)
      (user-error "No .el files selected"))
    (let ((count (length el-files)))
      (unless (y-or-n-p (format "Convert %d file(s) to %s style? " count style))
        (user-error "Operation cancelled"))
      (cl-loop for file in el-files
               for result = (pearl-paren-style--process-file file style)
               when (eq (car result) 'success)
               count 1 into processed
               when (eq (car result) 'error)
               do (message "Failed to process %s: %s" file (cdr result))
               finally do (message "Processed %d/%d file(s)" processed count)
               finally return (> processed 0)))))

;;;###autoload
(defun pearl-paren-style-toggle ()
  "Toggle paren style between compact and dangling."
  (interactive)
  (pcase (pearl-paren-style--detect)
    ('compact (pearl-paren-style--to-dangling))
    ('dangling (pearl-paren-style--to-compact))
    (_ (message "Unknown style, no toggle performed"))))

;;;###autoload
(defun pearl-paren-style-compact ()
  "Convert to compact style (closing parens on same line)."
  (interactive)
  (pearl-paren-style--to-compact))

;;;###autoload
(defun pearl-paren-style-dangling ()
  "Convert to dangling style (closing parens on separate lines).
Particularly recommended for:
- AI code generation sessions
- SEARCH/REPLACE operations (Aider/Copilot)
- Major refactoring operations

When `pearl-paren-style-show-annotations' is non-nil, closing
parentheses will display annotations showing the corresponding
opening parenthesis location.

Annotations are not automatically updated during editing.
To refresh annotations after making changes, run this command again."
  (interactive)
  (pearl-paren-style--to-dangling))

;;;###autoload
(defun pearl-paren-style-convert (style)
  "Convert to STYLE (\\='compact or \\='dangling)."
  (interactive
   (list (intern (completing-read "Convert to: " '("compact" "dangling") nil t))))
  (pcase style
    ('compact (pearl-paren-style--to-compact))
    ('dangling (pearl-paren-style--to-dangling))
    (_ (user-error "Unknown style: %s" style))))

;;;###autoload
(defun pearl-paren-style-compact-region (beg end)
  "Convert region from BEG to END to compact style."
  (interactive "r")
  (unless (pearl-paren-style--check-balanced-p beg end)
    (user-error "Unbalanced parentheses in selected region"))
  (save-restriction
    (narrow-to-region beg end)
    (pearl-paren-style--to-compact)))

;;;###autoload
(defun pearl-paren-style-dangling-region (beg end)
  "Convert region from BEG to END to dangling style."
  (interactive "r")
  (unless (pearl-paren-style--check-balanced-p beg end)
    (user-error "Unbalanced parentheses in selected region"))
  (save-restriction
    (narrow-to-region beg end)
    (pearl-paren-style--to-dangling)))

;;;###autoload
(defun pearl-paren-style-toggle-region (beg end)
  "Toggle paren style in region from BEG to END."
  (interactive "r")
  (unless (pearl-paren-style--check-balanced-p beg end)
    (user-error "Unbalanced parentheses in selected region"))
  (save-restriction
    (narrow-to-region beg end)
    (pcase (pearl-paren-style--detect)
      ('compact (pearl-paren-style--to-dangling))
      ('dangling (pearl-paren-style--to-compact))
      (_ (message "Unknown style in region, no toggle performed")))))

;;;###autoload
(defun pearl-paren-style-convert-region (style beg end)
  "Convert region from BEG to END to STYLE (\\='compact or \\='dangling)."
  (interactive
   (list (intern (completing-read "Convert to: " '("compact" "dangling") nil t))
         (region-beginning)
         (region-end)))
  (unless (use-region-p)
    (user-error "No region selected"))
  (unless (pearl-paren-style--check-balanced-p beg end)
    (user-error "Unbalanced parentheses in selected region"))
  (save-restriction
    (narrow-to-region beg end)
    (pcase style
      ('compact (pearl-paren-style--to-compact))
      ('dangling (pearl-paren-style--to-dangling))
      (_ (user-error "Unknown style: %s" style)))))

;;;###autoload
(defun pearl-paren-style-compact-files (files)
  "Convert FILES to compact style.
FILES is a list of file paths.
If called interactively without Dired selection, prompt for files."
  (interactive (list (pearl-paren-style--read-files)))
  (pearl-paren-style--with-el-files files 'compact))

;;;###autoload
(defun pearl-paren-style-dangling-files (files)
  "Convert FILES to dangling style.
FILES is a list of file paths.
If called interactively without Dired selection, prompt for files."
  (interactive (list (pearl-paren-style--read-files)))
  (pearl-paren-style--with-el-files files 'dangling))

;;;###autoload
(defun pearl-paren-style-convert-files (style files)
  "Convert FILES to STYLE (\\='compact or \\='dangling).
FILES is a list of file paths.
If called interactively without Dired selection, prompt for files."
  (interactive
   (list (intern (completing-read "Convert to: " '("compact" "dangling") nil t))
         (pearl-paren-style--read-files)))
  (pearl-paren-style--with-el-files files style))

;;;###autoload
(defun pearl-paren-style-annotations-to-comments ()
  "Convert all annotation overlays to comment text.
This creates permanent comments that can be read by AI tools
outside of Emacs sessions."
  (interactive)
  (unless pearl-paren-style-show-annotations
    (user-error "Annotations are disabled.  Enable with `pearl-paren-style-show-annotations'"))
  (cond
   ((> (length pearl-paren-style--annotation-overlays) 0)
    (let ((positions (sort (mapcar #'overlay-start pearl-paren-style--annotation-overlays) #'>)))
      (pearl-paren-style--clear-annotations)
      (let ((converted 0))
        (dolist (closing-pos positions)
          (when (pearl-paren-style--annotation-to-comment closing-pos)
            (cl-incf converted)))
        (message "Converted %d annotation(s) to comments" converted))))
   ((pearl-paren-style--find-annotation-comments)
    (message "Annotations are already comments"))
   (t
    (user-error "No annotations found to convert"))))

;;;###autoload
(defun pearl-paren-style-comments-to-annotations ()
  "Convert all annotation comments back to overlay annotations.
This restores interactive annotations from permanent comments."
  (interactive)
  (unless pearl-paren-style-show-annotations
    (user-error "Annotations are disabled.  Enable with `pearl-paren-style-show-annotations'"))
  (let ((comment-pairs (pearl-paren-style--find-annotation-comments)))
    (when (null comment-pairs)
      (user-error "No annotation comments found"))
    (let ((converted 0))
      (pearl-paren-style--clear-annotations) ; Clear any existing overlays first
      (dolist (pair comment-pairs)
        (let ((closing-pos (car pair))
              (comment-text (cdr pair)))
          (let ((ov (pearl-paren-style--comment-to-annotation closing-pos comment-text)))
            (when ov
              (push ov pearl-paren-style--annotation-overlays)
              (cl-incf converted)))))
      ;; Remove comment text after conversion
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (regexp-quote pearl-paren-style--annotation-comment-prefix) nil t)
          (let ((comment-start (match-beginning 0)))
            (save-excursion
              (goto-char comment-start)
              (skip-chars-backward " \t")
              (when (and (not (bobp))
                         (= (char-before) ?\)))
                (let ((closing-pos (1- (point))))
                  (when (not (pearl-paren-style--in-string-or-comment-p closing-pos))
                    (goto-char closing-pos)
                    (forward-char 1)
                    (let ((spaces-start (point)))
                      (skip-chars-forward " \t")
                      (when (looking-at (regexp-quote pearl-paren-style--annotation-comment-prefix))
                        ;; Exact match for annotation detail:
                        ;; 1. Prefix (escaped)
                        ;; 2. Coordinates (line:col)
                        ;; 3. One space
                        ;; 4. Detail text (up to ⟩)
                        (let ((re (concat (regexp-quote pearl-paren-style--annotation-comment-prefix)
                                          "[0-9]+:[0-9]+ .*?"
                                          (regexp-quote pearl-paren-style--annotation-end)
                                          "\\( *;.*\\)?$")))
                          (when (re-search-forward re (line-end-position) t)
                            (let ((trailing-comment (match-string 1)))
                              (delete-region spaces-start (point))
                              (when (and trailing-comment
                                         (not (string-empty-p trailing-comment)))
                                (let ((trimmed (string-trim-left trailing-comment)))
                                  (unless (string-prefix-p pearl-paren-style--annotation-comment-prefix trimmed)
                                    (insert "  " trimmed))))))))))))))))
      (message "Converted %d comment(s) to annotations" converted))))

;;;###autoload
(defun pearl-paren-style-run-tests ()
  "Run all test for pearl-paren-style."
  (interactive)
  (require 'ert)
  (ert-delete-all-tests)
  (let* ((this-file (symbol-file 'pearl-paren-style-run-tests))
         (dir (file-name-directory this-file)))
    ;; Unload old code
    (when (featurep 'pearl-paren-style)
      (unload-feature 'pearl-paren-style t))
    ;; Reload source files (force load .el, ignore .elc)
    (load (expand-file-name "pearl-paren-style" dir) nil t)
    ;; Load test files
    (load (expand-file-name "pearl-paren-style-test" dir) nil t))

  ;; Use batch-compatible function to ensure output is visible in terminal
  (if noninteractive
      (ert-run-tests-batch-and-exit)
    (ert t)))

;;;###autoload
(defun pearl-paren-style-dwim ()
  "Do What I Mean: convert based on context."
  (interactive)
  (cond
   ;; Case 1: region is active
   ((use-region-p)
    (call-interactively #'pearl-paren-style-convert-region))

   ;; Case 2: files/directories are selected in Dired
   ((and (derived-mode-p 'dired-mode)
         (dired-get-marked-files))
    (call-interactively #'pearl-paren-style-convert-files))

   ;; Case 3: default (entire buffer)
   (t
    (pearl-paren-style-toggle))))

(provide 'pearl-paren-style)
;;; pearl-paren-style.el ends here
