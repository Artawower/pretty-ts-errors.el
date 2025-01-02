;;; pretty-ts-errors.el --- Show TypeScript/Vue LSP errors as Markdown in a posframe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/blamer.el
;; Package-Requires: ((emacs "29.1") (posframe "1.4.4") (lsp-mode "9.0.0"))
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This package provides a minor mechanism to display TypeScript/Vue LSP errors
;; in a posframe, formatted as Markdown via an external CLI tool.  The posframe
;; is automatically hidden if the user moves the point.  The user can toggle it
;; at a specific diagnostic location.

;;; Code:

(require 'json)
(require 'seq)

;; We strongly rely on functions from lsp-mode and posframe.
;; If these packages are not loaded, please install/require them.
(declare-function lsp--workspace-server-id "lsp-mode")
(declare-function lsp--get-buffer-diagnostics "lsp-mode")
(declare-function lsp-get "lsp-mode")
(declare-function lsp--line-character-to-point "lsp-mode")
(declare-function lsp-workspaces "lsp-mode")
(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function gfm-mode "gfm-mode")

;;; Customizable variables
(defgroup pretty-ts-errors nil
  "Show TypeScript/Vue LSP errors as Markdown in a posframe on demand."
  :group 'tools
  :prefix "pretty-ts-errors-")

(defcustom pretty-ts-errors-cli-program "pretty-ts-errors-markdown"
  "Name of the CLI program used to format TypeScript/Vue errors as Markdown."
  :type 'string
  :group 'pretty-ts-errors)

(defcustom pretty-ts-errors-posframe-buffer "*pretty-ts-errors-posframe*"
  "Name of the posframe buffer used to display errors."
  :type 'string
  :group 'pretty-ts-errors)

(defcustom pretty-ts-errors-max-width 80
  "Maximum width of the posframe, in characters."
  :type 'integer
  :group 'pretty-ts-errors)

(defcustom pretty-ts-errors-max-height 20
  "Maximum height of the posframe, in lines."
  :type 'integer
  :group 'pretty-ts-errors)

(defcustom pretty-ts-errors-internal-border-width 1
  "Width of the internal border (padding) around the posframe content."
  :type 'integer
  :group 'pretty-ts-errors)

(defcustom pretty-ts-errors-internal-border-color "gray70"
  "Color of the internal border for the posframe."
  :type 'string
  :group 'pretty-ts-errors)

(defcustom pretty-ts-errors-hide-on-point-move t
  "If non-nil, automatically hide the posframe when the user moves the point."
  :type 'boolean
  :group 'pretty-ts-errors)

(defcustom pretty-ts-errors-left-fringe 0
  "Left fringe width for the posframe, to simulate horizontal padding."
  :type 'integer
  :group 'pretty-ts-errors)

(defcustom pretty-ts-errors-right-fringe 0
  "Right fringe width for the posframe, to simulate horizontal padding."
  :type 'integer
  :group 'pretty-ts-errors)

;;; Internal variables
(defvar pretty-ts-errors--posframe nil
  "Store the current posframe frame for toggling display.")

;;; Internal helper functions
(defun pretty-ts-errors--cli-installed-p ()
  "Return non-nil if the CLI program is installed."
  (executable-find pretty-ts-errors-cli-program))

(defun pretty-ts-errors--is-ts-or-vue-workspace (workspace)
  "Return non-nil if WORKSPACE belongs to a TypeScript or Vue server."
  (let ((sid (lsp--workspace-server-id workspace)))
    (member sid '(ts-ls
                  typescript-language-server
                  volar
                  vls
                  vue-ls
                  vue-semantic-server))))

(defun pretty-ts-errors--gather-diagnostics ()
  "Return all diagnostics for the current buffer, or nil if none."
  (when (fboundp 'lsp--get-buffer-diagnostics)
    (lsp--get-buffer-diagnostics)))

(defun pretty-ts-errors--point-in-range-p (pos range)
  "Return non-nil if POS (buffer position) is within RANGE (LSP range)."
  (let* ((start (lsp-get range :start))
         (end (lsp-get range :end))
         (start-pos (when (fboundp 'lsp--line-character-to-point)
                      (lsp--line-character-to-point
                       (lsp-get start :line)
                       (lsp-get start :character))))
         (end-pos (when (fboundp 'lsp--line-character-to-point)
                    (lsp--line-character-to-point
                     (lsp-get end :line)
                     (lsp-get end :character)))))
    (and start-pos end-pos
         (>= pos start-pos)
         (<= pos end-pos))))

(defun pretty-ts-errors--diagnostic-at-point (pos)
  "Return the first LSP diagnostic at POS, or nil if none is found."
  (let ((diags (pretty-ts-errors--gather-diagnostics)))
    (seq-find
     (lambda (d)
       (pretty-ts-errors--point-in-range-p pos (lsp-get d :range)))
     diags)))

(defun pretty-ts-errors--workspace-list ()
  "Return the list of workspaces for the current buffer."
  (when (fboundp 'lsp-workspaces)
    (lsp-workspaces)))

(defun pretty-ts-errors--ts-or-vue-workspace-present-p ()
  "Return non-nil if the current buffer has at least one TypeScript/Vue workspace."
  (seq-find #'pretty-ts-errors--is-ts-or-vue-workspace
            (pretty-ts-errors--workspace-list)))

(defun pretty-ts-errors--format-diagnostic (diag)
  "Use the external CLI to convert DIAG into Markdown text.
Return the resulting string.  DIAG must be an LSP diagnostic object."
  (unless (pretty-ts-errors--cli-installed-p)
    (user-error "CLI %s is not installed.  Run npm i -g pretty-ts-errors-markdown"
                pretty-ts-errors-cli-program))
  (let* ((json-diag (json-encode diag))
         (cmd-list (list pretty-ts-errors-cli-program "-i" json-diag))
         (cmd (mapconcat #'shell-quote-argument cmd-list " "))
         (md-result (shell-command-to-string cmd)))
    ;; Replace multiple blank lines with a single newline
    (replace-regexp-in-string "\n\n+" "\n" md-result)))

(defun pretty-ts-errors--posframe-hidehandler (initial-pos)
  "Return a function that check if point has changed from INITIAL-POS.
If so, it signals that the posframe should hide."
  (lambda (_frame-or-buffer)
    (and pretty-ts-errors-hide-on-point-move
         (not (eq (point) initial-pos)))))

;;; Posframe display functions
(defun pretty-ts-errors--show-posframe (text)
  "Show TEXT in a posframe using `gfm-mode`.
Return the posframe frame object."
  (when (not (fboundp 'posframe-show))
    (user-error "The posframe library is not available"))
  (let* ((lines (split-string text "\n"))
         (max-line-len (if (null lines)
                           0
                         (apply #'max (mapcar #'string-width lines))))
         (width (min max-line-len pretty-ts-errors-max-width))
         (height (min (length lines) pretty-ts-errors-max-height))
         (initial-pos (point))
         (frame
          (posframe-show
           pretty-ts-errors-posframe-buffer
           :string text
           :position (point)
           :width width
           :height height
           :internal-border-width pretty-ts-errors-internal-border-width
           :internal-border-color pretty-ts-errors-internal-border-color
           :left-fringe pretty-ts-errors-left-fringe
           :right-fringe pretty-ts-errors-right-fringe
           :hidehandler (pretty-ts-errors--posframe-hidehandler initial-pos)
           :background-color (face-background 'tooltip nil t))))
    (with-current-buffer pretty-ts-errors-posframe-buffer
      (when (fboundp 'gfm-mode)
        (gfm-mode)))
    frame))

(defun pretty-ts-errors--hide-posframe ()
  "Hide the posframe if it is visible."
  (when (and (fboundp 'posframe-hide)
             (frame-live-p pretty-ts-errors--posframe))
    (posframe-hide pretty-ts-errors-posframe-buffer)))

;;;###autoload
(defun pretty-ts-errors-show-error-at-point ()
  "Toggle a posframe with a Markdown-formatted TS/Vue LSP error at point.
If the posframe is visible, hide it.  Otherwise, if a relevant error is found,
format it and show it in the posframe.  The posframe also hides itself if user
moves point (when `pretty-ts-errors-hide-on-point-move' is non-nil)."
  (interactive)
  (if (and (frame-live-p pretty-ts-errors--posframe)
           (frame-visible-p pretty-ts-errors--posframe))
      (pretty-ts-errors-hide-posframe)
    (unless (pretty-ts-errors--ts-or-vue-workspace-present-p)
      (user-error "No TypeScript/Vue LSP workspace in this buffer"))
    (let ((diag (pretty-ts-errors--diagnostic-at-point (point))))
      (unless diag
        (user-error "No LSP diagnostic at point"))
      (let ((md (pretty-ts-errors--format-diagnostic diag)))
        (setq pretty-ts-errors--posframe (pretty-ts-errors--show-posframe md))))))

;;;###autoload
(defun pretty-ts-errors-hide-posframe ()
  "Hide the posframe if it is visible."
  (interactive)
  (pretty-ts-errors--hide-posframe)
  (setq pretty-ts-errors--posframe nil))

(provide 'pretty-ts-errors)
;;; pretty-ts-errors.el ends here
