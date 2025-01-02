;;; pretty-ts-errors-test.el --- Tests for pretty-ts-errors.el -*- lexical-binding: t; -*-

;; Author: Your Name <you@example.com>
;; Keywords: lsp, typescript, vue, tests
;; Package-Requires: ((emacs "29.1") (ert "0") (cl-lib "0.5")) ;; Adjust as needed

;;; Commentary:
;;
;; This file provides a suite of ERT tests that exercise the functionality
;; of pretty-ts-errors.el.  It mocks or stubs out certain functions from
;; lsp-mode and posframe to avoid requiring an actual LSP server or GUI.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pretty-ts-errors)  ;; Ensure your package is in the `load-path`


;;; Test Utilities (Mocks/Stubs)

(defvar pte--mock-cli-installed t
  "If non-nil, simulate that the CLI is installed.  Otherwise, pretend it is missing.")

(defvar pte--mock-workspaces nil
  "Simulated list of LSP workspaces for the current buffer.")

(defvar pte--mock-diagnostics nil
  "Simulated list of LSP diagnostics in the current buffer.")

(defun pte--mock-cli-installed-p ()
  "Return `pte--mock-cli-installed`."
  pte--mock-cli-installed)

(defun pte--mock-lsp-workspaces ()
  "Return `pte--mock-workspaces`."
  pte--mock-workspaces)

(defun pte--mock-lsp-get-buffer-diagnostics ()
  "Return `pte--mock-diagnostics`."
  pte--mock-diagnostics)

(defun pte--mock-shell-command-to-string (_cmd)
  "Stub that returns a fixed Markdown string for testing."
  "MOCKED-CLI-OUTPUT\n")

(defun pte--mock-lsp-workspace-server-id (ws)
  "Extract `:server-id` from WS, if present, else return nil."
  (plist-get ws :server-id))

(defun pte--mock-lsp-get (obj key)
  "Mock that reads plist OBJ by KEY. Ex: (lsp-get diag :range)."
  (plist-get obj key))

(defun pte--mock-lsp--line-character-to-point (_line _char)
  "Return a simulated buffer position from LINE, CHAR."
  ;; For simplicity, let's just combine them in a single integer.
  ;; e.g. line 10 char 5 => 1005
  ;; Real code in lsp-mode is more sophisticated, but we just need a stable mock.
  (+ (* _line 100) _char))

(defun pte--mock-posframe-show (&rest _args)
  "Simulate showing posframe, return a fake frame object symbol."
  'mock-posframe-frame)

(defun pte--mock-posframe-hide (&rest _args)
  "Simulate hiding posframe, returns nil."
  nil)


;;; Actual Tests

(ert-deftest pretty-ts-errors-test-cli-not-installed ()
  "If the CLI is not installed, calling format function should raise an error."
  (cl-letf (((symbol-function 'pretty-ts-errors--cli-installed-p)
             #'pte--mock-cli-installed-p)
            ((symbol-function 'shell-command-to-string)
             #'pte--mock-shell-command-to-string))
    (setq pte--mock-cli-installed nil)
    (should-error
     (pretty-ts-errors--format-diagnostic '(:message "foo"))
     :type 'error)))

(ert-deftest pretty-ts-errors-test-no-workspace ()
  "If there is no TS/Vue workspace, `pretty-ts-errors-show-error-at-point` should error out."
  (cl-letf (((symbol-function 'lsp-workspaces)
             #'pte--mock-lsp-workspaces)
            ((symbol-function 'pretty-ts-errors--cli-installed-p)
             #'pte--mock-cli-installed-p)
            ((symbol-function 'shell-command-to-string)
             #'pte--mock-shell-command-to-string)
            ((symbol-function 'posframe-show)
             #'pte--mock-posframe-show)
            ((symbol-function 'posframe-hide)
             #'pte--mock-posframe-hide))
    (setq pte--mock-cli-installed t
          pte--mock-workspaces nil)
    (should-error
     (pretty-ts-errors-show-error-at-point)
     :type 'error)))

(ert-deftest pretty-ts-errors-test-workspace-but-no-diagnostic ()
  "If we have a TS/Vue workspace but no diagnostic at point, we should error."
  (cl-letf (((symbol-function 'lsp-workspaces)
             #'pte--mock-lsp-workspaces)
            ((symbol-function 'pretty-ts-errors--cli-installed-p)
             #'pte--mock-cli-installed-p)
            ((symbol-function 'posframe-show)
             #'pte--mock-posframe-show)
            ((symbol-function 'posframe-hide)
             #'pte--mock-posframe-hide)
            ((symbol-function 'shell-command-to-string)
             #'pte--mock-shell-command-to-string)
            ((symbol-function 'lsp--get-buffer-diagnostics)
             #'pte--mock-lsp-get-buffer-diagnostics)
            ((symbol-function 'lsp--workspace-server-id)
             #'pte--mock-lsp-workspace-server-id)
            ((symbol-function 'lsp-get)
             #'pte--mock-lsp-get)
            ((symbol-function 'lsp--line-character-to-point)
             #'pte--mock-lsp--line-character-to-point))
    (setq pte--mock-cli-installed t
          pte--mock-workspaces (list '(:server-id ts-ls))
          pte--mock-diagnostics nil)  ;; no diagnostics
    (should-error
     (pretty-ts-errors-show-error-at-point)
     :type 'error)))

(ert-deftest pretty-ts-errors-test-show-error ()
  "Happy-path: we have a TS workspace and a diagnostic at point => posframe is shown."
  (cl-letf (((symbol-function 'lsp-workspaces)
             #'pte--mock-lsp-workspaces)
            ((symbol-function 'pretty-ts-errors--cli-installed-p)
             #'pte--mock-cli-installed-p)
            ((symbol-function 'shell-command-to-string)
             #'pte--mock-shell-command-to-string)
            ((symbol-function 'lsp--get-buffer-diagnostics)
             #'pte--mock-lsp-get-buffer-diagnostics)
            ((symbol-function 'lsp--workspace-server-id)
             #'pte--mock-lsp-workspace-server-id)
            ((symbol-function 'lsp-get)
             #'pte--mock-lsp-get)
            ((symbol-function 'lsp--line-character-to-point)
             #'pte--mock-lsp--line-character-to-point)
            ((symbol-function 'posframe-show)
             #'pte--mock-posframe-show)
            ((symbol-function 'posframe-hide)
             #'pte--mock-posframe-hide))
    (setq pte--mock-cli-installed t
          pte--mock-workspaces (list '(:server-id ts-ls))
          ;; Let's simulate a single diagnostic that covers position 0..999
          ;; so point=10 is inside it
          pte--mock-diagnostics (list '(:range
                                        (:start (:line 0 :character 0)
                                                :end   (:line 9 :character 99))
                                        :message "Some TS Error")))
    ;; Expect no error => success
    (pretty-ts-errors-show-error-at-point)
    ;; After success, we can check that `pretty-ts-errors--posframe`
    ;; has been set to our fake 'mock-posframe-frame
    (should (eq pretty-ts-errors--posframe 'mock-posframe-frame))))

(ert-deftest pretty-ts-errors-test-toggle-hide ()
  "If posframe is visible, calling show again should hide it."
  (cl-letf (((symbol-function 'frame-live-p) (lambda (_f) t))
            ((symbol-function 'frame-visible-p) (lambda (_f) t))
            ((symbol-function 'posframe-hide) #'pte--mock-posframe-hide))
    ;; Assume posframe is already visible
    (setq pretty-ts-errors--posframe 'some-non-nil-frame)
    ;; Calling show-error-at-point again => should hide posframe, no error
    (should (pretty-ts-errors-show-error-at-point))
    (should (null pretty-ts-errors--posframe))))

;;; pretty-ts-errors-test.el ends here
