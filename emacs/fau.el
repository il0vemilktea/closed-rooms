;;; fau.el --- Render Faust textures with the fau CLI -*- lexical-binding: t; -*-

;;; Commentary:

;; Edit-and-listen loop for .dsp files in a fau project:
;;
;;   `fau-render'        render the current file and play it (C-u: edit overrides first)
;;   `fau-live-mode'     do that now, and again whenever the piece, a library or an
;;                       imported CSV changes on disk (runs `fau watch')
;;   `fau-stop'          stop playback
;;   `fau-set-overrides' per-buffer params passed on the command line, e.g. "q=30 dur=60"
;;   `fau-toggle-loop'   loop playback
;;   `fau-diagram'       block diagram in the browser
;;   `fau-spectrogram'   spectrogram of the last render
;;   `fau-new'           new piece from the template
;;
;; In Doom:
;;
;;   SPC m p  render + play    SPC m l  live mode     SPC m k  stop
;;   SPC m a  overrides        SPC m L  toggle loop   SPC m g  diagram
;;   SPC m S  spectrogram      SPC m n  new piece
;;
;; Expects `fau' on `exec-path'

;;; Code:

(require 'compile)
(require 'subr-x)

(defgroup fau nil
  "Render Faust files with fau."
  :group 'tools)

(defcustom fau-executable "fau"
  "Name or path of the fau CLI."
  :type 'string)

(defcustom fau-play t
  "Play the result after rendering."
  :type 'boolean)

(defcustom fau-loop nil
  "Loop playback."
  :type 'boolean)

(defcustom fau-show-output 'on-error
  "When to show the *fau* buffer: `always', or only `on-error'."
  :type '(choice (const always) (const on-error)))

(defvar-local fau-overrides ""
  "Param overrides for this buffer, e.g. \"q=30 dur=60\".")

(defvar fau--source nil
  "The buffer whose file the running render belongs to.")

;; faust reports errors as "file.dsp:12 : ERROR : ...", "file.lib:12 ERROR : ..."
;; or "ERROR : [file file.dsp : 12] : ..."; fau's params block parser as
;; "file.dsp:12:5:" and its CSV reader as "file.csv:3: ..."
(defvar fau-compilation-error-regexp-alist '(fau fau-bracket fau-params fau-csv))
(defvar fau-compilation-error-regexp-alist-alist
  '((fau "^\\(.+?\\.\\(?:dsp\\|lib\\)\\):\\([0-9]+\\)\\(?: :\\)? \\(?:ERROR\\|\\(WARNING\\)\\)"
     1 2 nil (3 . nil))
    (fau-bracket "^\\(?:ERROR\\|\\(WARNING\\)\\) : \\[file \\(.+?\\) : \\([0-9]+\\)\\]"
                 2 3 nil (1 . nil))
    (fau-params "^\\(.+?\\.dsp\\):\\([0-9]+\\):\\([0-9]+\\):$" 1 2 3 2)
    (fau-csv "^\\(.+?\\.csv\\):\\([0-9]+\\): " 1 2 nil 2)))

(define-compilation-mode fau-compilation-mode "fau"
  "Output of fau renders."
  (add-hook 'compilation-filter-hook #'fau--echo-progress nil t)
  (add-hook 'compilation-finish-functions #'fau--finished nil t))

(defun fau--echo-progress ()
  "Echo compile/render status lines from new fau output."
  (save-excursion
    (goto-char compilation-filter-start)
    (forward-line 0)
    (while (re-search-forward "^\\(compiling .*\\|rendered .*\\|WARNING: .*\\|render failed: .*\\)\n" nil t)
      (let ((line (match-string-no-properties 1)))
        ;; `fau watch' keeps running after a failed render, so don't wait for an exit
        (when (string-prefix-p "render failed" line)
          (display-buffer (current-buffer)))
        (message "fau: %s%s" line (fau--state))))))

(defun fau--state ()
  "Tags for the echo area: whether the source buffer is live, and looping."
  (concat (when (and (buffer-live-p fau--source)
                     (buffer-local-value 'fau-live-mode fau--source))
            "  [live]")
          (when (and fau-play fau-loop) "  [loop]")))

(defun fau--finished (buf msg)
  "Show BUF if the render failed (MSG is the compilation exit message)."
  (when (string-prefix-p "exited abnormally" msg)
    (display-buffer buf)))

(defun fau--file ()
  "Return the .dsp file of the current buffer."
  (if (and buffer-file-name (string-suffix-p ".dsp" buffer-file-name))
      buffer-file-name
    (user-error "Not visiting a .dsp file")))

(defun fau--exe ()
  "Return the absolute path of the fau executable."
  (or (executable-find fau-executable)
      (user-error "Can't find `%s' (did you `direnv allow' this project?)" fau-executable)))

(defun fau--run (&rest args)
  "Run fau synchronously with ARGS and return the last line it printed."
  (let ((exe (fau--exe)))
    (with-temp-buffer
      (unless (eq 0 (apply #'process-file exe nil t nil args))
        (error "fau %s failed:\n%s" (car args) (buffer-string)))
      (car (last (split-string (buffer-string) "\n" t))))))

(defun fau--process ()
  "The running fau process (rendering or playing), if any."
  (when-let* ((buf (get-buffer "*fau*")))
    (get-buffer-process buf)))

(defun fau--kill ()
  "Stop the running fau process and the player it started; non-nil if one ran.
SIGINT reaches the whole process group, so the player stops at once and fau
cleans up; it's killed outright if it hasn't exited after 0.3 s. Doing this
ourselves avoids `compilation-start''s one-second wait before a restart."
  (when-let* ((proc (fau--process)))
    (process-put proc 'fau-stopped t)
    (interrupt-process proc)
    (let ((deadline (+ (float-time) 0.3)))
      (while (and (process-live-p proc) (< (float-time) deadline))
        (accept-process-output proc 0.02)))
    (when (process-live-p proc)
      (delete-process proc))
    t))

(defvar fau-live-mode)                  ; defined below

(defun fau--command ()
  "The fau command for the current file: `watch' in live mode, else `render'."
  (string-join
   (delq nil (list (shell-quote-argument (fau--exe))
                   (if fau-live-mode "watch" "render")
                   (shell-quote-argument (fau--file))
                   (cond (fau-live-mode (when fau-loop "--loop"))
                         (fau-play (if fau-loop "--loop" "--play")))
                   (unless (string-blank-p fau-overrides) fau-overrides)))
   " "))

(defun fau--start ()
  "Render the current file (watch it in live mode), replacing any fau run."
  (let* ((cmd (fau--command))
         (display-buffer-overriding-action
          (if (eq fau-show-output 'always)
              display-buffer-overriding-action
            '(display-buffer-no-window (allow-no-window . t)))))
    (fau--kill)
    (setq fau--source (current-buffer))
    (let* ((buf (compilation-start cmd #'fau-compilation-mode (lambda (_) "*fau*")))
           (proc (get-buffer-process buf))
           (sentinel (process-sentinel proc)))
      ;; compile.el echoes "fau interrupt" when a run ends; not when we stopped it
      (set-process-sentinel proc (lambda (p event)
                                   (let ((inhibit-message (process-get p 'fau-stopped)))
                                     (funcall sentinel p event)))))))

(defun fau--read-overrides ()
  (string-trim (read-string "fau overrides (name=value ...): " fau-overrides)))

(defun fau-render (&optional edit)
  "Render the current file and play it (in live mode, restart watching it).
With prefix arg EDIT, edit the param overrides first."
  (interactive "P")
  (when edit (setq fau-overrides (fau--read-overrides)))
  (when (buffer-modified-p) (save-buffer))
  (fau--start))

(define-minor-mode fau-live-mode
  "Render and play the current file now, and again whenever it changes.
Runs `fau watch', which also re-renders when a library the piece can import
or a CSV file it imports changes, whoever writes it (e.g. a notebook)."
  :lighter " fau▶"
  (if fau-live-mode
      (progn
        (message "fau: live, re-rendering on every change")
        (fau-render))
    (fau--kill)
    (message "fau: live off")))

(defun fau-stop ()
  "Stop rendering and playback, and live mode with them."
  (interactive)
  (let ((stopped (fau--kill)))
    (when (and (buffer-live-p fau--source)
               (buffer-local-value 'fau-live-mode fau--source))
      (with-current-buffer fau--source (fau-live-mode -1)))
    (message (if stopped "fau: stopped" "fau: nothing to stop"))))

(defun fau-set-overrides (overrides)
  "Set the param OVERRIDES of this buffer.
Re-renders at once if live mode is on or something is playing."
  (interactive (list (fau--read-overrides)))
  (setq fau-overrides overrides)
  (if (or fau-live-mode (fau--process))
      (fau-render)
    (message "fau: overrides %s" (if (string-empty-p overrides) "cleared" overrides))))

(defun fau-toggle-loop ()
  "Toggle looped playback at once.
Turning it on (re)starts playback looped. Turning it off stops playback, or
in live mode keeps watching without looping."
  (interactive)
  (setq fau-loop (not fau-loop))
  (if (or fau-loop fau-live-mode)
      (fau-render)
    (fau--kill))
  (message "fau: loop %s" (if fau-loop "on" "off")))

(defun fau-diagram ()
  "Show the block diagram of the current file in the browser."
  (interactive)
  (browse-url-of-file (fau--run "diagram" (fau--file) "--no-open")))

(defun fau-spectrogram ()
  "Show the spectrogram of the last render of the current file."
  (interactive)
  (let ((buf (find-file-noselect (fau--run "spec" (fau--file) "--no-open"))))
    (with-current-buffer buf (revert-buffer t t))
    (display-buffer buf)))

(defun fau-new (name)
  "Create src/NAME.dsp from the template and visit it."
  (interactive "sNew piece: ")
  (find-file (fau--run "new" name)))

(defvar fau-mode-map (make-sparse-keymap)
  "Keys of `fau-mode'.")

(define-minor-mode fau-mode
  "The fau commands, for the Faust buffers of a fau project."
  :lighter " fau"
  :keymap fau-mode-map
  ;; evil only sees a minor mode's state bindings once its keymaps are refreshed
  (when (fboundp 'evil-normalize-keymaps)
    (evil-normalize-keymaps))
  ;; override autosave and indentation
  (if fau-mode
      (setq-local auto-save-visited-predicate #'ignore
                  indent-line-function #'indent-relative-first-indent-point
                  electric-indent-inhibit t
                  indent-tabs-mode nil
                  tab-width 4)
    (mapc #'kill-local-variable '(auto-save-visited-predicate indent-line-function
                                  electric-indent-inhibit indent-tabs-mode tab-width))
    (when (fboundp 'faust-smie-reset)
      (faust-smie-reset))))

(when (fboundp 'map!)
  (map! :map fau-mode-map
        :i [tab] #'tab-to-tab-stop
        :i "TAB" #'tab-to-tab-stop
        :localleader
        "p" #'fau-render
        "l" #'fau-live-mode
        "k" #'fau-stop
        "a" #'fau-set-overrides
        "L" #'fau-toggle-loop
        "g" #'fau-diagram
        "S" #'fau-spectrogram
        "n" #'fau-new))

(provide 'fau)
;;; fau.el ends here
