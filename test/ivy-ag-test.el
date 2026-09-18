;;; ivy-ag-test.el --- Regression tests for ivy-ag -*- lexical-binding: t; -*-

(require 'ert)
(require 'ivy-ag)

(defconst ivy-ag-test--query
  "Do not use the Agent tool, workflows, or deep-research unless the user, a CLAUDE.md file, or a skill asks for it")

(defmacro ivy-ag-test--with-search (&rest body)
  "Run BODY with isolated search state and a temporary directory."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "ivy-ag-test-" t))
          (default-directory (file-name-as-directory directory))
          (ivy-ag--process nil)
          (ivy-ag--start-timer nil)
          (ivy-ag--update-timer nil)
          (ivy-ag--timeout-timer nil)
          (ivy-ag--last-input nil)
          (ivy-ag--preview-buffer nil)
          (ivy-ag--preview-window-configuration nil)
          (ivy-last (make-ivy-state :caller 'ivy-ag :directory default-directory
                                    :re-builder #'ivy--regex-plus
                                    :window (selected-window) :buffer (current-buffer)
                                    :prompt "ag: "))
          (ivy-text "needle")
          (counsel-ag-command '("ag" "--vimgrep" "%s"))
          (counsel-async-command-delay 0)
          (ivy-ag-search-timeout 2)
          (ivy--all-candidates nil)
          (ivy--old-cands nil)
          (ivy--index 0)
          (ivy--highlight-function #'ivy--highlight-default))
     (unwind-protect
         (cl-letf (((symbol-function 'active-minibuffer-window)
                    (lambda () (selected-window)))
                   ((symbol-function 'ivy--insert-minibuffer) #'ignore))
           ,@body)
       (ivy-ag--unwind)
       (delete-directory directory t))))

(defun ivy-ag-test--wait ()
  "Wait at most four seconds for this test's search to finish."
  (let ((deadline (+ (float-time) 4)))
    (while (and (< (float-time) deadline)
                (or ivy-ag--start-timer
                    (and ivy-ag--process (process-live-p ivy-ag--process))))
      (accept-process-output nil 0.02))
    (should-not ivy-ag--start-timer)
    (should ivy-ag--process)
    (should-not (process-live-p ivy-ag--process))
    (ivy-ag--publish ivy-ag--process)))

(ert-deftest ivy-ag-test-native-regexp-highlighting ()
  (ivy-ag-test--with-search
    (let* ((ivy-text "needle[0-9]+")
           (ivy-regex (ivy--regex-plus ivy-text))
           (ivy--subexps 0)
           (result (ivy--format-minibuffer-line "sample.txt:1:1:needle42")))
      (should (get-text-property (string-match "needle42" result) 'face result))
      (ivy-ag--collection ivy-text)
      (should (eq ivy--highlight-function #'ivy--highlight-default)))))

(ert-deftest ivy-ag-test-quit-during-rendering-cancels-search ()
  (ivy-ag-test--with-search
    (ivy-ag--start-search '("cat") default-directory ivy-last)
    (let ((process ivy-ag--process)
          (inhibit-quit t))
      (ivy-ag--filter process "a:1:1:needle\n")
      (cl-letf (((symbol-function 'ivy--format)
                 (lambda (&rest _)
                   (should-not inhibit-quit)
                   (signal 'quit nil))))
        (unwind-protect
            (progn
              (ivy-ag--publish process)
              (let ((pending quit-flag))
                (setq quit-flag nil)
                (should pending))
              (should-not (process-live-p process))
              (should-not ivy-ag--process)
              (should-not ivy-ag--update-timer))
          (setq quit-flag nil))))))

(ert-deftest ivy-ag-test-real-ag-both-command-formats ()
  (skip-unless (executable-find "ag"))
  (ivy-ag-test--with-search
    (with-temp-file "sample.txt" (insert "first\néé needle\nlast\n"))
    (dolist (command '("ag --nocolor --nogroup --silent %s"
                       ("ag" "--vimgrep" "%s")))
      (setq counsel-ag-command command)
      (ivy-ag--collection "needle")
      (ivy-ag-test--wait)
      (should (= 1 (length ivy--all-candidates)))
      (should (string-match-p "sample.txt:2:6:éé needle"
                              (car ivy--all-candidates))))))

(ert-deftest ivy-ag-test-long-line-retains-column ()
  (skip-unless (executable-find "ag"))
  (ivy-ag-test--with-search
    (with-temp-file "long.txt" (insert (make-string 50000 ?x) " needle\n"))
    (let ((ivy-ag-max-line-length 80))
      (ivy-ag--search "needle")
      (ivy-ag-test--wait))
    (should (= 1 (length ivy--all-candidates)))
    (should (< (length (car ivy--all-candidates)) 150))
    (should (string-match-p "long.txt:1:50002:" (car ivy--all-candidates)))))

(ert-deftest ivy-ag-test-result-limit ()
  (skip-unless (executable-find "ag"))
  (ivy-ag-test--with-search
    (with-temp-file "many.txt" (dotimes (_ 1000) (insert "needle\n")))
    (let ((ivy-ag-max-results 10))
      (ivy-ag--search "needle")
      (ivy-ag-test--wait))
    (should (= 10 (length ivy--all-candidates)))
    (should (equal "result limit"
                   (process-get ivy-ag--process 'ivy-ag--stop-reason)))))

(ert-deftest ivy-ag-test-output-limit ()
  (ivy-ag-test--with-search
    (ivy-ag--start-search '("cat") default-directory ivy-last)
    (let ((ivy-ag-max-output-size 30))
      (ivy-ag--filter ivy-ag--process
                      (concat "file:1:1:needle\nfile:2:1:" (make-string 10000 ?x))))
    (should-not (process-live-p ivy-ag--process))
    (should (equal "output limit"
                   (process-get ivy-ag--process 'ivy-ag--stop-reason)))
    (should (equal "file:1:1:needle\n"
                   (with-current-buffer (process-buffer ivy-ag--process)
                     (buffer-string))))))

(ert-deftest ivy-ag-test-chunks-preserve-incomplete-result ()
  (ivy-ag-test--with-search
    (ivy-ag--start-search '("cat") default-directory ivy-last)
    (let ((ivy-ag-max-results 2))
      (ivy-ag--filter ivy-ag--process "one:1:1:needle\ntwo:2:")
      (should (process-live-p ivy-ag--process))
      (ivy-ag--publish ivy-ag--process)
      (should (equal '("one:1:1:needle") ivy--all-candidates))
      (ivy-ag--filter ivy-ag--process "1:needle\nthree:3:1:needle\n")
      (ivy-ag--publish ivy-ag--process)
      (should (equal '("one:1:1:needle" "two:2:1:needle")
                     ivy--all-candidates)))))

(ert-deftest ivy-ag-test-timeout ()
  (ivy-ag-test--with-search
    (let ((ivy-ag-search-timeout 0.05))
      (ivy-ag--start-search '("sleep" "30") default-directory ivy-last))
    (ivy-ag-test--wait)
    (should (equal "time limit"
                   (process-get ivy-ag--process 'ivy-ag--stop-reason)))))

(ert-deftest ivy-ag-test-cancel-pending-search ()
  (ivy-ag-test--with-search
    (let ((counsel-async-command-delay 0.05))
      (ivy-ag--search "needle")
      (should (timerp ivy-ag--start-timer))
      (ivy-ag--unwind)
      (sleep-for 0.1)
      (should-not ivy-ag--start-timer)
      (should-not ivy-ag--process))))

(ert-deftest ivy-ag-test-replacement-and-short-input-cancel ()
  (ivy-ag-test--with-search
    (ivy-ag--start-search '("cat") default-directory ivy-last)
    (let ((old ivy-ag--process)
          (old-buffer (process-buffer ivy-ag--process)))
      (ivy-ag--search "replacement")
      (should-not (process-live-p old))
      (should-not (buffer-live-p old-buffer))
      ;; Late output must not reach a newer search or a dead buffer.
      (ivy-ag--filter old "stale:1:1:needle\n")
      (ivy-ag--search "")
      (should-not ivy-ag--start-timer)
      (should-not ivy-ag--process))))

(ert-deftest ivy-ag-test-preview-preserves-complete-ordinary-file ()
  (ivy-ag-test--with-search
    (let ((text (concat "first line\n"
                        (make-string 65 ?x) " needle42\n"
                        (apply #'concat (make-list 60 "ordinary context line\n"))
                        "last line without newline"))
          (ivy-ag-max-line-length 80)
          (ivy-ag-preview-context-lines 1)
          (ivy-text "needle[0-9]+")
          (ivy-exit nil))
      (with-temp-file "ordinary.txt" (insert text))
      (save-window-excursion
        (ivy-ag--grep-action "ordinary.txt:2:67:needle42")
        (should (equal text (buffer-string)))
        (should (looking-at-p "needle42"))
        (should (= (line-number-at-pos) 2))
        (should-not (string-match-p "excerpt" header-line-format))
        (should-not buffer-file-name)
        (should-not mark-active)))))

(ert-deftest ivy-ag-test-excerpt-keeps-short-target-line-intact ()
  (let ((ivy-ag-max-line-length 80))
    (with-temp-buffer
      (insert (make-string 300 ?x) "\n" (make-string 65 ?y) " needle42\n")
      (let* ((content (ivy-ag--preview-content 2 67))
             (text (car content)))
        (should (nth 3 content))
        (should (string-suffix-p (concat (make-string 65 ?y) " needle42") text))
        (should (string-prefix-p "needle42" (substring text (1- (nth 1 content)))))))))

(ert-deftest ivy-ag-test-preview-width-boundary ()
  (let ((ivy-ag-max-line-length 80))
    (with-temp-buffer
      (insert (make-string 80 ?x) "\n" (make-string 80 ?y))
      (let ((content (ivy-ag--preview-content 2 70)))
        (should-not (nth 3 content))
        (should (equal (car content) (buffer-string))))
      (goto-char (point-max))
      (insert "y")
      (should (nth 3 (ivy-ag--preview-content 2 70))))))

(ert-deftest ivy-ag-test-preview-is-non-visiting-and-highlights-full-match ()
  (ivy-ag-test--with-search
    (with-temp-file "preview.el" (insert ";; éé needle42\n"))
    (save-window-excursion
      (let ((ivy-exit nil)
            (ivy-text "needle[0-9]+")
            (find-file-hook (list (lambda () (ert-fail "Visited preview file"))))
            (emacs-lisp-mode-hook (list (lambda () (ert-fail "Ran preview mode hook")))))
        (ivy-ag--grep-action "preview.el:1:9:;; éé needle42")
        (should (eq (current-buffer) ivy-ag--preview-buffer))
        (should (eq major-mode 'emacs-lisp-mode))
        (should-not buffer-file-name)
        (should-not (get-file-buffer (expand-file-name "preview.el" directory)))
        (should-not mark-active)
        (should-not (mark t))
        (should (looking-at-p "needle42"))
        (should (seq-some
                 (lambda (overlay)
                   (equal "needle42"
                          (buffer-substring-no-properties
                           (overlay-start overlay) (overlay-end overlay))))
                 isearch-lazy-highlight-overlays))
        (ivy-ag--unwind)
        (should-not ivy-ag--preview-buffer)))))

(ert-deftest ivy-ag-test-preview-reads-disk-and-preserves-modified-buffer ()
  (ivy-ag-test--with-search
    (with-temp-file "preview.txt" (insert "prefix needle42\n"))
    (save-window-excursion
      (let ((source (find-file-noselect "preview.txt"))
            (ivy-exit nil)
            (ivy-text "needle[0-9]+"))
        (unwind-protect
            (progn
              (switch-to-buffer source)
              (erase-buffer)
              (insert "unsaved text without the match\n")
              (goto-char 3)
              (set-mark 8)
              (setq mark-active t)
              (narrow-to-region 2 12)
              (let ((before (list (point) (mark) mark-active (point-min) (point-max)
                                  (buffer-modified-p) major-mode (buffer-string))))
                (ivy-ag--grep-action "preview.txt:1:8:prefix needle42")
                (should (equal (buffer-string) "prefix needle42\n"))
                (should (looking-at-p "needle42"))
                (should-not mark-active)
                (with-current-buffer source
                  (should (equal before
                                 (list (point) (mark) mark-active (point-min) (point-max)
                                       (buffer-modified-p) major-mode (buffer-string)))))
                (ivy-ag--unwind)
                (should (eq (window-buffer (selected-window)) source))
                (with-current-buffer source
                  (should (equal before
                                 (list (point) (mark) mark-active (point-min) (point-max)
                                       (buffer-modified-p) major-mode (buffer-string)))))))
          (with-current-buffer source (set-buffer-modified-p nil))
          (kill-buffer source))))))

(ert-deftest ivy-ag-test-preview-errors-return-focus-through-ivy-call ()
  (ivy-ag-test--with-search
    (with-temp-file "large.txt" (insert (make-string 100 ?x)))
    (save-window-excursion
      (let ((input-buffer (generate-new-buffer " *ivy-ag-test-input*"))
            (large-buffer (find-file-noselect "large.txt"))
            (source-window (selected-window))
            (input-window (split-window-right))
            (ivy-exit nil)
            (ivy-inhibit-action nil)
            (ivy-marked-candidates nil)
            (ivy-recursive-restore nil)
            (large-file-warning-threshold 50))
        (unwind-protect
            (progn
              (with-current-buffer large-buffer
                (erase-buffer)
                (insert "short unsaved buffer"))
              (with-current-buffer input-buffer (insert "needle"))
              (set-window-buffer input-window input-buffer)
              (setf (ivy-state-window ivy-last) source-window
                    (ivy-state-action ivy-last) #'ivy-ag--grep-action)
              (cl-letf (((symbol-function 'active-minibuffer-window)
                         (lambda () input-window)))
                (dolist (case '(("large.txt:1:1:x" user-error)
                                ("missing.txt:1:1:x" file-missing)))
                  (select-window input-window)
                  (setf (ivy-state-current ivy-last) (car case))
                  (should-error (ivy-call) :type (cadr case))
                  (should (eq (selected-window) input-window))
                  (should (equal (with-current-buffer input-buffer (buffer-string))
                                 "needle"))
                  (should-not ivy-ag--preview-buffer))))
          (kill-buffer input-buffer)
          (with-current-buffer large-buffer (set-buffer-modified-p nil))
          (kill-buffer large-buffer))))))

(ert-deftest ivy-ag-test-preview-long-line-and-other-window ()
  (ivy-ag-test--with-search
    (with-temp-file "preview.txt" (insert (make-string 200000 ?x) " needle42\n"))
    (save-window-excursion
      (let ((ivy-exit nil)
            (ivy-text "needle[0-9]+")
            (ivy-ag-max-line-length 80)
            (configuration (current-window-configuration)))
        (ivy-ag--open-in-other-window-action "preview.txt:1:200002:snippet")
        (should (eq (current-buffer) ivy-ag--preview-buffer))
        (should (<= (buffer-size) 80))
        (should-not (get-file-buffer (expand-file-name "preview.txt" directory)))
        (should (looking-at-p "needle42"))
        (should (seq-some (lambda (overlay)
                            (= 8 (- (overlay-end overlay) (overlay-start overlay))))
                          isearch-lazy-highlight-overlays))
        (ivy-ag--unwind)
        (should (compare-window-configurations
                 configuration (current-window-configuration)))))))

(ert-deftest ivy-ag-test-accept-visits-file-without-extending-region ()
  (ivy-ag-test--with-search
    (with-temp-file "preview.txt" (insert "first\néé needle\n"))
    (save-window-excursion
      (let ((source (find-file-noselect "preview.txt"))
            (ivy-exit nil))
        (unwind-protect
            (progn
              (with-current-buffer source
                (goto-char 2) (set-mark 5) (setq mark-active t))
              (ivy-ag--grep-action "preview.txt:2:6:éé needle")
              (let ((ivy-exit 'done))
                (ivy-ag--grep-action "preview.txt:2:6:éé needle"))
              (should (eq (current-buffer) source))
              (should (looking-at-p "needle"))
              (should-not mark-active)
              (should (= (mark) 5))
              (should-not ivy-ag--preview-buffer))
          (kill-buffer source))))))

(ert-deftest ivy-ag-test-no-matches-and-process-error ()
  (skip-unless (executable-find "ag"))
  (ivy-ag-test--with-search
    (with-temp-file "sample.txt" (insert "unrelated\n"))
    (ivy-ag--search "needle")
    (ivy-ag-test--wait)
    (should (= 1 (process-exit-status ivy-ag--process)))
    (should-not ivy--all-candidates)
    (ivy-ag--cancel-search)
    (ivy-ag--start-search '("sh" "-c" "printf 'bad option\\n'; exit 2")
                          default-directory ivy-last)
    (ivy-ag-test--wait)
    (should-not ivy--all-candidates)
    (should (equal "bad option" counsel--async-last-error-string))))

(ert-deftest ivy-ag-test-occur-snapshot-does-not-restart-search ()
  (skip-unless (executable-find "ag"))
  (ivy-ag-test--with-search
    (with-temp-file "sample.txt" (insert "needle\n"))
    (ivy-ag--collection "needle")
    (ivy-ag-test--wait)
    (let ((process ivy-ag--process))
      (with-temp-buffer
        (ivy-ag--occur (ivy-ag--collection "needle"))
        (should (eq major-mode 'ivy-occur-mode))
        (should buffer-read-only)
        (should (string-match-p "sample.txt:1:1:needle" (buffer-string)))
        (should (eq process ivy-ag--process))
        (should-not ivy-ag--start-timer)))))

(ert-deftest ivy-ag-test-stale-display-cannot-change-another-session ()
  (ivy-ag-test--with-search
    (ivy-ag--start-search '("cat") default-directory ivy-last)
    (ivy-ag--filter ivy-ag--process "one:1:1:needle\n")
    (let ((ivy-last (make-ivy-state :caller 'ivy-ag))
          (ivy--all-candidates '("keep this")))
      (ivy-ag--publish ivy-ag--process)
      (should (equal '("keep this") ivy--all-candidates)))))

(ert-deftest ivy-ag-test-occur-navigation-does-not-run-regexp-overlays ()
  (ivy-ag-test--with-search
    (with-temp-file "sample.txt" (insert "needle\n"))
    (setf (ivy-state-action ivy-last) #'ivy-ag--grep-action
          (ivy-state-text ivy-last) ivy-ag-test--query)
    (save-window-excursion
      (let ((occur (generate-new-buffer " *ivy-ag-occur-test*")) file-buffer)
        (unwind-protect
            (progn
              (switch-to-buffer occur)
              (ivy-ag--occur '("sample.txt:1:1:needle"))
              (setq ivy-occur-last ivy-last)
              (forward-line 1)
              (cl-letf (((symbol-function 'swiper--add-overlays)
                         (lambda (&rest _) (ert-fail "Occur reran query regexp"))))
                (ivy-occur-press))
              (setq file-buffer (get-file-buffer
                                 (expand-file-name "sample.txt" directory)))
              (should file-buffer)
              (should (= (with-current-buffer file-buffer (point)) 1)))
          (kill-buffer occur)
          (when file-buffer (kill-buffer file-buffer)))))))

(provide 'ivy-ag-test)
;;; ivy-ag-test.el ends here
