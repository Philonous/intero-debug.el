;;; -*- lexical-binding: t -*-

(require 'dash)
(require 's)

;; (defun intero-debug-get-buffer-create ()
;;   (-when-let* ((buffer (get-buffer-create "*haskell debugging*"))
;;                (source-buffer (intero-))
;;                (targets (with-current-buffer (intero-))))
;;     (targets )
;;     (with-current-buffer buffer
;;       (unless (process-live-p (get-buffer-process (current-buffer)))
;;         (intero-start-process-in-buffer buffer)))))

;; (defun intero-debug-setup ()
;;   (let* ((targets nil))
;;     (when-let ((intero-repl-buffer (get-haskell-buffer)))
;;       (with-current-buffer intero-repl-buffer
;;         (setq targets intero-targets)))
;;     (let ((filename (buffer-file-name)))
;;       (when-let (backend-buffer (intero-buffer-p 'backend))
;;         (with-current-buffer (intero-get-buffer-create 'debug)
;;           (set (make-variable-buffer-local 'intero-targets) targets)))
;;       (intero-restart)
;;       (intero-blocking-call 'debug ":set -fbyte-code")
;;       (message (intero-blocking-call 'debug (format ":load %s" filename)))
;;       (message (intero-blocking-call 'debug (format ":show modules" filename))))))

(defun intero-debug-guess-module-name ()
  "Guess the module name of the current buffer"
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^[ ]*module[ ]+\\([.[:alnum:]]+\\)" nil t)
        (match-string-no-properties 1)
      "Main")))


;; (defun intero-debug-add-breakpoint ()
;;   (interactive)
;;   (let* ((module (intero-debug-guess-module-name))
;;          (line (line-number-at-pos))
;;          (column (current-column)))
;;     (when-let ((haskell-buffer (get-haskell-buffer))
;;                (proc (get-buffer-process haskell-buffer)))
;;       (with-current-buffer haskell-buffer
;;         (comint-simple-send proc (format ":break %s %s %s" module line column))))))


(defvar intero-debug-return-functions nil)
;; (defvar intero-debug--stream-output nil)

;; (defun intero-debug-check-debug-context (str)
;;   "Check if we are in a debug context, and if yes start debug mode"
;;   ;; Reset output streaming
;;   (setq intero-debug--stream-output nil)
;;   (message "%s" str))

;;; Filter function that writes all incoming text into a buffer until we see "\4
;;; " (intero's prompt marker), then pass the entire section to the first
;;; callback in intero-debug-return-functiosn
(defun intero-debug-comint-filter-fun (input)
  (with-current-buffer (get-buffer-create "haskell:debug:comint")
    ;; If no function is waiting for input install the default function
    ;; (unless intero-debug-return-functions
    ;;   ;; Stream output while we're wating
    ;;   (setq intero-debug--stream-output t)
    ;;   (push intero-debug-return-functions
    ;;         'intero-debug-check-debug-context))
    (goto-char (point-max))
    (insert input)
    (goto-char (point-min))
    (-if-let* ((input-marker-end (search-forward "\4 " nil t))
               (input-end (- input-marker-end 2))
               (str (buffer-substring-no-properties (point-min) input-end)))
        (progn
          (delete-region (point-min) input-marker-end)
          (-if-let (cont (pop intero-debug-return-functions))
              (or (funcall cont (intero-debug--strip-debug-prompt str)) "")
            input))
      ""
      )))


(defun intero-debug-install-filter-fun ()
  (when-let ((intero-repl-buffer (get-haskell-buffer))
             (proc (get-buffer-process intero-repl-buffer)))
    (with-current-buffer intero-repl-buffer
      (unless (-contains? comint-preoutput-filter-functions
                          'intero-debug-comint-filter-fun)
        (push 'intero-debug-comint-filter-fun
              comint-preoutput-filter-functions)))))

(defun intero-debug-call-intero-async (str callback)
  (when-let ((intero-repl-buffer (get-haskell-buffer))
             (proc (get-buffer-process intero-repl-buffer)))
    (intero-debug-install-filter-fun)
    (with-current-buffer intero-repl-buffer
      (setq intero-debug-return-functions
            (append
             intero-debug-return-functions
             (list callback)))
      (comint-simple-send proc str))))

(defun intero-debug--has-debug-prompt (str)
  (string-match "\\`\\(\\(?:.\\|\n\\)+\\)\\[[^:]+:[^]]+\\][ ]+\\'"
                str))


(defun intero-debug--strip-debug-prompt (str)
  "Strip the last line when it looks like a debug prompt, e.g.
\"[/path/to/project/Test.hs:392:16-29] \"
"
  (if (intero-debug--has-debug-prompt str)
      (match-string 1 str)
    str))

(defun intero-debug-call-intero-blocking (command)
  (let ((res nil))
    (intero-debug-call-intero-async
     command
     (lambda (str)
       (setq res str)
       nil
       ))
    (while (not res)
      (sleep-for 0.01))
    res))

(defun intero-debug-make-buffer-position (line column)
  (save-excursion
    (goto-char (point-min))
    (forward-line (- line 1))
    (move-to-column (- column 1))
    (point)))

(defface intero-debug-breakpoint
  '((t (:underline (:color "deep sky blue" :style wave))))
  "Face for breakpoint markers"
  )

(defun intero-debug-match-region (str)
  (let ((start-line nil)
        (end-line nil)
        (start-column nil)
        (end-column nil))
    (cond
     ((string-match "\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)" str)
      (setq start-line (string-to-number (match-string 1 str)))
      (setq start-column (string-to-number (match-string 2 str)))
      (setq end-line (string-to-number (match-string 1 str)))
      (setq end-column (string-to-number (match-string 3 str))))
     ((string-match "(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))" str)
      (setq start-line (string-to-number (match-string 1 str)))
      (setq start-column (string-to-number (match-string 2 str)))
      (setq end-line (string-to-number (match-string 3 str)))
      (setq end-column (string-to-number (match-string 4 str))))
     (t (error "Could not match break point position %s" str)))
    (list :start-line start-line
          :start-column start-column
          :end-line end-line
          :end-column end-column)))

(defun intero-debug-add-breakpoint-overlay (bpnumber region)
  "Add a breakpoint overlay given the NUMBER of the breakpoint
  and the REGION string as given by GHCi"
  (-let* (((&plist :start-line start-line
                   :start-column start-column
                   :end-line end-line
                   :end-column end-column
                   ) (intero-debug-match-region region))
          (start (intero-debug-make-buffer-position
                  start-line start-column))
          (end (+ (intero-debug-make-buffer-position
                   end-line end-column)
                  1))
          (breakpoint-overlay (make-overlay start end)))
    (overlay-put breakpoint-overlay 'type 'breakpoint)
    (overlay-put breakpoint-overlay 'category 'intero-debug)
    (overlay-put breakpoint-overlay 'breakpoint bpnumber)
    (overlay-put breakpoint-overlay 'face 'intero-debug-breakpoint)
    (overlay-put breakpoint-overlay 'help-echo (format "Breakpoint %d" bpnumber))
    ))

(defun intero-debug-add-breakpoint-here ()
  (interactive)
  (let* ((module (intero-debug-guess-module-name)) (line (line-number-at-pos))
         (column (current-column))
         (res (intero-debug-call-intero-blocking
               (format ":break %s %d %d" module line column))))
    (cond
     ((string-match
       "Breakpoint \\([0-9]+\\) \\(activated\\|was already set\\) at \\([^:]+\\):\\(.+\\)"
       res)
      (-let* ((bpnumber (string-to-number (match-string 1 res)))
              (verb (match-string 2 res))
              (_path (match-string 3 res))
              (region (match-string 4 res)))
        (cond
         ((string= "activated" verb)
          (intero-debug-add-breakpoint-overlay bpnumber region)
          (message "Added breakpoint %d" bpnumber))
        ((string= "was already set" verb)
         (message "Breakpoint was already set as %d" bpnumber)
         ;; Breakpoint was already set, nothing to do
         )
        (t (error "Could not understand %s in result: %s" verb res))
        )))
    ((string-match "Could not find module ‘\\([^’]+\\)’" res)
     (error "Module %s not loaded" (match-string 1 res)))
    (t (error "Could not parse result: %s" res)))))

(defun intero-debug-show-breakpoints ()
  (interactive)
  (remove-overlays nil nil 'type 'breakpoint)
  (-when-let* ((file (buffer-file-name))
               (breaks-str (intero-debug-call-intero-blocking ":show breaks")))
    (if (string-match "No Active breakpoints" breaks-str)
        nil
      (progn
        (dolist (line (-filter (lambda (l) (s-present? (s-trim l)))
                               (s-lines breaks-str)))
          (if (string-match "\\[\\([0-9]+\\)\\] \\([[:alnum:]]+\\) \\([^:]+\\):\\(.+\\)" line)
              (let* ((bpnumber (string-to-number (match-string 1 line)))
                     (_module (match-string 3 line))
                     (path (match-string 3 line))
                     (region (match-string 4 line)))
                (when (string= file path)
                  (intero-debug-add-breakpoint-overlay bpnumber region)))
            (error "Could not parse break point line %s" line)))))))

(defun intero-debug-clear-breakpoints ()
  (interactive)
  (intero-debug-call-intero-blocking ":delete *")
  (remove-overlays nil nil 'type 'breakpoint))

(defun inter-debug--get-breakpoint-here ()
  (-first (lambda (o)
            (eq 'breakpoint (overlay-get o 'type)))
          (overlays-at (point))))

(defun intero-debug-remove-breakpoint (o)
  (-if-let (bpnumber (overlay-get o 'breakpoint))
      (progn
        (intero-debug-call-intero-blocking
         (format ":delete %d" bpnumber))
         (delete-overlay o))
    (message "Overlay did not have breakpoint attached")))

(defun intero-debug-remove-breakpoints-here ()
  (interactive)
  (dolist (o (overlays-at (point)))
    (when (eq (overlay-get o 'type) 'breakpoint)
      (intero-debug-remove-breakpoint o))))

(defun test ()
  (intero-debug-clear-breakpoints)
  (intero-debug-add-breakpoint)
  (intero-debug-remove-breakpoint-here)
  )

(defface intero-debug-current-context
  '((t :inherit highlight))
  "Face for the current debug context")

(defun intero-debug--clear-context-overlays ()
  (remove-overlays nil nil 'type 'intero-debug-context))

(defun intero-debug--get-context ()
  "Get the current debug context or nil if we there is no active computation"
  (let ((ctx-string (s-trim (intero-debug-call-intero-blocking ":show context"))))
    (if (string-match "Stopped in \\([^,]+\\), \\([^:]+\\):\\(.+\\)" ctx-string)
        ctx-string)))

(defun intero-debug-goto-context ()
  "Find and mark current debug context"
  (interactive)
  (intero-debug--clear-context-overlays)
  (-if-let* ((ctx (intero-debug--get-context)))
      (-let* ((binding (match-string 1 ctx))
              (file (match-string 2 ctx))
              ((&plist :start-line start-line
                       :start-column start-column
                       :end-line end-line
                       :end-column end-column
                       )
               (intero-debug-match-region
                (match-string 3 ctx)))
              )
        (find-file file)
        (let* ((start (intero-debug-make-buffer-position start-line start-column))
               (end (+ (intero-debug-make-buffer-position end-line end-column) 1))
               (ctx-overlay (make-overlay start end)))
          (overlay-put ctx-overlay 'category 'intero-debug)
          (overlay-put ctx-overlay 'type 'intero-debug-context)
          (overlay-put ctx-overlay 'face 'intero-debug-current-context)
          (goto-char start)))
    (message "Not in a debug context")))

(defun intero-debug-step ()
  "Take a step in the current debug context"
  (interactive)
  (intero-debug-call-intero-blocking ":steplocal")
  (intero-debug-goto-context)
  (intero-debug-show-bindings))

(defun intero-debug-toggle-breakpoint ()
  "Toggle breakpoint here"
  (interactive)
  (-if-let* ((bp-overlay (inter-debug--get-breakpoint-here))
             (bp-number (overlay-get bp-overlay 'breakpoint)))
      (progn
        (intero-debug-remove-breakpoint bp-overlay)
        (message "Removed breakpoint %d" bp-number))
    (intero-debug-add-breakpoint-here)))

(defun intero-debug-show-bindings ()
  "Show active bindings in another window"
  (let* ((bindings (intero-debug-call-intero-blocking ":show bindings")))
    (with-current-buffer (get-buffer-create "*intero-debug:bindings*")
      (let ((inhibit-read-only t))
        (setq buffer-read-only t)
        (delete-region (point-min) (point-max))
        (insert bindings)))))


(defun intero-debug-abandon ()
  "Abandong the current debug context"
  (interactive)
  (intero-debug-call-intero-blocking ":abandon")
  (intero-debug--clear-context-overlays)
  (message "Computation abandoned")
  )

(defun intero-debug-history-back ()
  "Go forward in evaluation history"
  (interactive)
  (intero-debug-call-intero-blocking ":back")
  (intero-debug-goto-context)
  (intero-debug-show-bindings))


(defun intero-debug-history-forward ()
  "Go forward in evaluation history"
  (interactive)
  (intero-debug-call-intero-blocking ":forward")
  (intero-debug-goto-context)
  (intero-debug-show-bindings))

(defvar intero-debug-mode-map (make-sparse-keymap))

(defun intero-debug-quit ()
  (interactive)
  (when (and (intero-debug--get-context)
             (yes-or-no-p "Abandon current computation?"))
    (intero-debug-abandon))
  (intero-debug-mode -1))

(defun intero-debug-refresh ()
  "Refresh the interactive debug display, i.e. breakpoints and
the current debug context"
  (interactive)
  (intero-debug-show-breakpoints)
  (when (intero-debug--get-context)
    (intero-debug-goto-context)
    (intero-debug-show-bindings)))

(define-key intero-debug-mode-map (kbd "b") 'intero-debug-toggle-breakpoint)
(define-key intero-debug-mode-map (kbd "g") 'intero-debug-refresh)
(define-key intero-debug-mode-map (kbd "a") 'intero-debug-abandon)
(define-key intero-debug-mode-map (kbd "q") 'intero-debug-quit)

(define-key intero-debug-mode-map (kbd "C-p") 'intero-debug-history-forward)
(define-key intero-debug-mode-map (kbd "C-n") 'intero-debug-history-back)

(define-key intero-debug-mode-map (kbd "<SPC>") 'intero-debug-step)


(define-minor-mode intero-debug-mode
  "Toggle intero debug mode
\\<intero-debug-mode-map>
"
  :lighter " Debug"
  :group 'haskell
  (if intero-debug-mode
      (progn
        (setq buffer-read-only t)
        (intero-debug-refresh))
    (progn
      (setq buffer-read-only nil)
      (remove-overlays nil nil 'category 'intero-debug))))
