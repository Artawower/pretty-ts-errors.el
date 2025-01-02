;;; test.el --- Tests for pretty-ts-errors.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'pretty-ts-errors)  ;; Make sure your package is in load-path

(defvar pte--mock-cli-installed t
  "If non-nil, simulate that the CLI is installed. Otherwise, pretend it is missing.")

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

(defun pte--mock-lsp-workspace-server-id (ws)
  "Return :server-id from WS if present."
  (plist-get ws :server-id))

(defun pte--mock-lsp-get (obj key)
  "Return the value of KEY in the plist OBJ, simulating `lsp-get`."
  (plist-get obj key))

(defun pte--mock-lsp--line-character-to-point (line char)
  "Mock line->point by combining LINE and CHAR into a single integer (just a stub)."
  ;; Example: line=0 char=10 => point=10
  (+ (* line 100) char))

(defun pte--mock-shell-command-to-string (_cmd)
  "Return a fixed markdown string for testing, ignoring _CMD."
  "MOCKED-CLI-OUTPUT\n")

(defun pte--mock-posframe-show (&rest _args)
  "Create the buffer so `with-current-buffer` doesn't fail, and return a fake frame."
  (get-buffer-create pretty-ts-errors-posframe-buffer)
  'mock-posframe-frame)

(defun pte--mock-posframe-hide (&rest _args)
  "No-op: pretend posframe is hidden."
  nil)


;;; Test: CLI missing

(ert-deftest pretty-ts-errors-test-cli-not-installed ()
  "If the CLI is not installed, calling format function should raise an error."
  (let ((pte--mock-cli-installed nil))
    (cl-letf (((symbol-function 'pretty-ts-errors--cli-installed-p)
               #'pte--mock-cli-installed-p)
              ((symbol-function 'shell-command-to-string)
               #'pte--mock-shell-command-to-string))
      (should-error
       (pretty-ts-errors--format-diagnostic '(:message "foo"))
       :type 'error))))


;;; Test: No workspace

(ert-deftest pretty-ts-errors-test-no-workspace ()
  "If there is no TS/Vue workspace, `pretty-ts-errors-show-error-at-point` errors."
  (let ((pte--mock-cli-installed t)
        (pte--mock-workspaces nil))
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
      (should-error
       (pretty-ts-errors-show-error-at-point)
       :type 'error))))


;;; Test: Workspace, no diag

(ert-deftest pretty-ts-errors-test-workspace-but-no-diagnostic ()
  "We have a TS/Vue workspace but no diagnostic => error."
  (let ((pte--mock-cli-installed t)
        (pte--mock-workspaces (list '(:server-id ts-ls)))
        (pte--mock-diagnostics nil))
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
      (should-error
       (pretty-ts-errors-show-error-at-point)
       :type 'error))))


;;; Test: Show error success

(ert-deftest pretty-ts-errors-test-show-error ()
  "Happy path: TS workspace + diagnostic at point => posframe is shown."
  (let ((pte--mock-cli-installed t)
        (pte--mock-workspaces (list '(:server-id ts-ls)))
        (pte--mock-diagnostics
         (list '(:range (:start (:line 0 :character 0)
                                :end   (:line 9 :character 99))
                        :message "Some TS Error"))))
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
      (setq pretty-ts-errors--posframe nil)
      (pretty-ts-errors-show-error-at-point)
      ;; After success, the posframe var should be 'mock-posframe-frame
      (should (eq pretty-ts-errors--posframe 'mock-posframe-frame)))))


;;; Test: Toggle hide

(ert-deftest pretty-ts-errors-test-toggle-hide ()
  "If posframe is visible, calling `pretty-ts-errors-show-error-at-point` hides it."
  (cl-letf (((symbol-function 'frame-live-p)    (lambda (_f) t))
            ((symbol-function 'frame-visible-p) (lambda (_f) t))
            ((symbol-function 'posframe-hide)    #'pte--mock-posframe-hide))
    ;; Suppose posframe is already showing
    (setq pretty-ts-errors--posframe 'some-non-nil-frame)
    ;; Simply call it (no error) => it should hide posframe
    (pretty-ts-errors-show-error-at-point)
    ;; Check that it's hidden
    (should (null pretty-ts-errors--posframe))))

(provide 'test)
;;; test.el ends here
