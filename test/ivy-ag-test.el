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
          (ivy-case-fold-search 'auto)
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
    ;; An exited child can still have output and its sentinel queued.
    (while (accept-process-output ivy-ag--process 0.01))
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


;;; Search settings and menu integration

(defmacro ivy-ag-test--with-menu (state &rest body)
  "Initialize a non-displaying search menu from STATE, then run BODY."
  (declare (indent 1) (debug t))
  `(let* ((transient-values nil)
          (transient-history nil)
          (transient--prefix
           (transient--init-prefix 'ivy-ag-menu
                                  (list :value (ivy-ag--state-value ,state))))
          (transient--suffixes
           (transient--flatten-suffixes (transient--init-suffixes 'ivy-ag-menu)))
          (transient-current-command 'ivy-ag-menu)
          (transient-current-prefix transient--prefix)
          (transient-current-suffixes transient--suffixes))
     ,@body))

(defun ivy-ag-test--context-infix (field)
  "Find the current menu's context infix for FIELD."
  (seq-find (lambda (obj)
              (and (cl-typep obj 'ivy-ag--context-infix)
                   (eq (oref obj field) field)))
            transient--suffixes))

(ert-deftest ivy-ag-test-menu-types-and-complete-history ()
  (let* ((first (ivy-ag--make-state :directory "/tmp/first/" :input "foo.*bar"
                                   :flags '("--python" "--literal") :width 70
                                   :exclusions '("/tmp/first/a b")))
         (second (ivy-ag--make-state :directory "/tmp/second/" :input "other"
                                    :flags '("--elisp" "--case-sensitive") :width 42)))
    (ivy-ag-test--with-menu first
      (should (member "--python" (transient-args 'ivy-ag-menu)))
      (let ((types (seq-find (lambda (obj) (cl-typep obj 'ivy-ag--types-infix))
                             transient--suffixes)))
        (transient-infix-set types '("--elisp"))
        (should (member "--elisp" (transient-args 'ivy-ag-menu)))
        (should-not (member "--python" (transient-args 'ivy-ag-menu)))
        (transient-infix-set types nil)
        (should-not (member "--elisp" (transient-args 'ivy-ag-menu))))
      (transient-infix-set (ivy-ag-test--context-infix :input) "edited query")
      ;; Infix traversal order need not match the original argv order.
      (setf (ivy-ag--state-flags second) '("--case-sensitive" "--elisp"))
      (let* ((present (ivy-ag--menu-state))
             (transient-history `((ivy-ag-menu ,(ivy-ag--state-value second)))))
        (transient--history-init transient--prefix)
        (transient-history-prev)
        (should (equal (ivy-ag--state-context (ivy-ag--menu-state))
                       (ivy-ag--state-context second)))
        (should (equal (oref transient--prefix scope) (ivy-ag--state-context second)))
        (transient-history-next)
        (should (equal (ivy-ag--state-context (ivy-ag--menu-state))
                       (ivy-ag--state-context present)))))))

(ert-deftest ivy-ag-test-menu-default-args-without-active-prefix ()
  (let ((transient-values nil)
        (ivy-ag--last (ivy-ag--make-state))
        (ivy-ag-presets nil))
    (should (assoc "ivy-ag-context" (transient-args 'ivy-ag-menu)))))

(ert-deftest ivy-ag-test-real-literal-aliases-and-case ()
  (ivy-ag-test--with-search
    (with-temp-file "sample.txt" (insert "foo.bar\nfoo bar\nneedle\nNEEDLE\n"))
    (dolist (flag '("--literal" "--fixed-strings" "-F" "-Q"))
      (let ((counsel-ag-command (list "ag" flag "%s")))
        (dolist (query '("foo.bar" "foo bar"))
          (ivy-ag--search query)
          (ivy-ag-test--wait)
          (should (= 1 (length ivy--all-candidates)))
          (should (string-suffix-p query (car ivy--all-candidates))))))
    (dolist (case '(("--case-sensitive" "needle" 1)
                    ("--ignore-case" "NEEDLE" 2)
                    ("--smart-case" "NEEDLE" 1)
                    ("--smart-case" "needle" 2)))
      (let ((counsel-ag-command (list "ag" (car case) "%s")))
        (ivy-ag--search (cadr case))
        (ivy-ag-test--wait)
        (should (= (nth 2 case) (length ivy--all-candidates)))))))

(ert-deftest ivy-ag-test-inline-literal-and-highlighting ()
  (ivy-ag-test--with-search
    (with-temp-file "sample.txt" (insert "foo.bar foo bar\n"))
    (let* ((builder (ivy-ag--make-re-builder nil))
           (ivy-re-builders-alist '((t . ivy--regex-plus))))
      (setf (ivy-state-re-builder ivy-last) builder)
      (should (equal (regexp-quote "foo.bar") (funcall builder "-Q -- foo.bar")))
      (ivy-ag--search "-Q -- foo bar")
      (ivy-ag-test--wait)
      (should (= 1 (length ivy--all-candidates))))))

(ert-deftest ivy-ag-test-whitespace-arguments-and-exact-exclusions ()
  (ivy-ag-test--with-search
    (dolist (file '("sub/a[b].txt" "sub/ab.txt" "other/sub/a[b].txt"
                    "vendor cache/drop.txt" "keep name.txt"))
      (make-directory (or (file-name-directory file) ".") t)
      (with-temp-file file (insert "needle\n")))
    (let* ((exclusions (mapcar #'expand-file-name '("sub/a[b].txt" "vendor cache")))
           (args (ivy-ag--exclusion-args exclusions default-directory)))
      (dolist (base '(("ag" "%s") "ag %s"))
        (let ((counsel-ag-command (ivy-ag--format-command base (append args '("%s")))))
          (ivy-ag--search "needle")
          (ivy-ag-test--wait)
          (should (= 3 (length ivy--all-candidates)))
          (should (seq-some (lambda (s) (string-prefix-p "other/sub/a[b].txt:" s))
                            ivy--all-candidates)))))
    (let ((counsel-ag-command '("ag" "--file-search-regex=keep name" "%s")))
      (ivy-ag--search "needle")
      (ivy-ag-test--wait)
      (should (= 1 (length ivy--all-candidates)))
      (should (string-prefix-p "keep name.txt:" (car ivy--all-candidates))))))

(ert-deftest ivy-ag-test-fresh-input-versus-resumed-input ()
  (let ((ivy-ag-escape-initial-input-chars-regex #'regexp-quote))
    (should (equal "foo\\.bar" (ivy-ag--initial-text "foo.bar" nil)))
    (should (equal "foo.bar" (ivy-ag--initial-text "foo.bar" '("-F"))))))

(ert-deftest ivy-ag-test-nil-escape-and-explicit-empty-flags ()
  (let ((ivy-ag-escape-initial-input-chars-regex nil)
        (ivy-ag--last (ivy-ag--make-state :directory default-directory
                                         :flags '("--literal")))
        (ivy-ag--last-input nil)
        (counsel-ag-command nil)
        (counsel--regex-look-around nil)
        (history-add-new-input nil)
        (ivy-ag-presets nil)
        captured)
    (should (equal "foo.bar" (ivy-ag--initial-text "foo.bar" nil)))
    (cl-letf (((symbol-function 'ivy-read)
               (lambda (_prompt _collection &rest args)
                 (setq captured (plist-get args :initial-input))))
              ((symbol-function 'ivy-ag--unwind) #'ignore))
      (ivy-ag default-directory "foo.*bar" nil)
      (should (equal captured "foo.*bar"))
      (should-not (ivy-ag--state-flags ivy-ag--last))
      (ivy-ag default-directory "")
      (should (equal captured "")))))

(ert-deftest ivy-ag-test-width-and-output-destinations ()
  (ivy-ag-test--with-search
    (let* ((ivy-ag-max-line-length 17)
           (counsel-ag-command '("ag" "--vimgrep" "--width=3" "%s"))
           (command (ivy-ag--build-command "needle" 'ivy)))
      (should-not (member "--width=3" command))
      (should (equal (cadr (member "--width" command)) "17"))
      (let ((grep (ivy-ag--build-command "needle" 'grep)))
        (should-not (member "--vimgrep" grep))
        (should-not (seq-some (lambda (s) (string-prefix-p "--width" s)) grep))
        (should (member "--print-long-lines" grep)))
      (let ((output (ivy-ag--build-command "needle" 'output '("--count"))))
        (should (member "--count" output))
        (should-not (member "--vimgrep" output))))))

(ert-deftest ivy-ag-test-file-marks-retain-directory-identity ()
  (let ((ivy-ag--file-selection nil)
        (ivy--directory "/tmp/first/")
        (ivy-last (make-ivy-state :current "one.txt")))
    (cl-letf (((symbol-function 'ivy-next-line) #'ignore)
              ((symbol-function 'ivy--exhibit) #'ignore))
      (ivy-ag-file-mark)
      (setq ivy--directory "/tmp/second/")
      (ivy-ag-file-mark)
      (should (equal (sort (copy-sequence ivy-ag--file-selection) #'string<)
                     '("/tmp/first/one.txt" "/tmp/second/one.txt")))
      (ivy-ag-file-unmark)
      (should (equal ivy-ag--file-selection '("/tmp/first/one.txt")))
      (ivy-ag-file-clear-marks)
      (should-not ivy-ag--file-selection))))

(ert-deftest ivy-ag-test-named-presets-same-directory-and-defaults ()
  (let ((ivy-ag-presets
         '((:label "Home text" :directory "/tmp/home/" :flags ("--all-text"))
           (:label "Home hidden" :directory "/tmp/home/" :flags ("--hidden") :automatic t)
           (:label "Code" :directory "/tmp/home/code/" :flags ("--skip-vcs-ignores")
            :automatic t))))
    (should (= 3 (length (ivy-ag--preset-candidates))))
    (should (equal "Home hidden" (plist-get (ivy-ag--automatic-preset "/tmp/home/") :label)))
    (should (equal "Code" (plist-get (ivy-ag--automatic-preset "/tmp/home/code/pkg/") :label)))
    (should-not (ivy-ag--automatic-preset "/tmp/home-other/"))
    (ivy-ag-test--with-menu (ivy-ag--make-state :directory "/tmp/home/" :width 80)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
        (should-error (ivy-ag-menu-save-preset) :type 'user-error)))))

(ert-deftest ivy-ag-test-output-menu-retains-search-settings ()
  (let* ((state (ivy-ag--make-state :directory "/tmp/" :input "foo bar"
                                   :flags '("--literal" "--python") :width 80
                                   :output-flags '("--count")))
         (transient--prefix (transient--init-prefix
                             'ivy-ag-output-menu
                             (list :value (ivy-ag--state-value state t))))
         (transient--suffixes (transient--flatten-suffixes
                               (transient--init-suffixes 'ivy-ag-output-menu)))
         (transient-current-prefix transient--prefix)
         (transient-current-command 'ivy-ag-output-menu)
         (transient-current-suffixes transient--suffixes))
    (should (equal (ivy-ag--state-context state)
                   (ivy-ag--state-context (ivy-ag--menu-state))))))

(ert-deftest ivy-ag-test-copied-command-reproduces-query-and-root ()
  (ivy-ag-test--with-search
    (with-temp-file "a b.txt" (insert "foo.bar; $(not-a-command)\n"))
    (let* ((state (ivy-ag--make-state :directory default-directory
                                     :input "foo.bar; $(not-a-command)"
                                     :flags '("--literal") :width 90))
           (command (ivy-ag--reproducible-command state 'ivy))
           (output (shell-command-to-string command)))
      (should (string-match-p "a b.txt:1:1:foo" output))
      (should (string-match-p (regexp-quote "$(not-a-command)") output)))))

(ert-deftest ivy-ag-test-wgrep-edits-full-source-lines ()
  (skip-unless (require 'wgrep nil t))
  (let* ((directory (make-temp-file "ivy-ag-wgrep-" t))
         (default-directory (file-name-as-directory directory))
         (file (expand-file-name "long.txt"))
         (text (concat "needle needle " (make-string 5000 ?x) " tail\n"))
         (ivy-ag--last (ivy-ag--make-state))
         (ivy-ag--last-input nil)
         (ivy-ag-max-line-length 20)
         (wgrep-auto-save-buffer nil)
         output source)
    (unwind-protect
        (save-window-excursion
          (with-temp-file file (insert text))
          (setq output (ivy-ag--run-buffer
                        (ivy-ag--make-state :directory default-directory
                                            :input "needle" :flags '("--literal")
                                            :width 20)
                        'grep))
          (let ((deadline (+ (float-time) 5)))
            (while (and (get-buffer-process output) (< (float-time) deadline))
              (accept-process-output (get-buffer-process output) 0.02)))
          (should-not (get-buffer-process output))
          (with-current-buffer output
            (should (derived-mode-p 'grep-mode))
            (goto-char (point-min))
            (should (search-forward text nil t))
            ;; Ordinary grep output produces one editable record per source line.
            (should-not (search-forward text nil t))
            (should (eq (key-binding (kbd "C-x C-q")) #'wgrep-change-to-wgrep-mode))
            (call-interactively (key-binding (kbd "C-x C-q")))
            (goto-char (point-min))
            (search-forward "long.txt:1:")
            (search-forward "needle")
            (replace-match "renamed" t t)
            (wgrep-finish-edit))
          (setq source (get-file-buffer file))
          (should source)
          (with-current-buffer source
            (should (equal (buffer-string)
                           (concat "renamed needle " (make-string 5000 ?x) " tail\n")))))
      (when (buffer-live-p output) (kill-buffer output))
      (when (buffer-live-p source)
        (with-current-buffer source (set-buffer-modified-p nil))
        (kill-buffer source))
      (delete-directory directory t))))

(ert-deftest ivy-ag-test-output-filename-list-has-no-content-pattern ()
  (ivy-ag-test--with-search
    (with-temp-file "one.el" (insert "content\n"))
    (with-temp-file "two.txt" (insert "content\n"))
    (let* ((state (ivy-ag--make-state :directory default-directory :input "irrelevant"
                                     :width 80 :output-flags '("--filename-pattern=\\.el$")))
           (command (ivy-ag--menu-command state 'output))
           (output (shell-command-to-string (ivy-ag--shell-command command))))
      (should (string-match-p "one.el" output))
      (should-not (string-match-p "two.txt" output))
      (should-not (member "irrelevant" command)))))

(ert-deftest ivy-ag-test-saved-presets-are-independent-and-labeled ()
  (let ((ivy-ag-presets nil)
        saved)
    (ivy-ag-test--with-menu (ivy-ag--make-state :directory "/tmp/" :input "first"
                                              :flags '("--literal") :width 77)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Personal notes"))
                ((symbol-function 'customize-save-variable)
                 (lambda (_ value &rest _) (setq ivy-ag-presets value saved value))))
        (ivy-ag-menu-save-preset))
      (should (equal "Personal notes" (plist-get (car saved) :label)))
      (should (equal "first" (plist-get (car saved) :input)))
      (transient-infix-set (ivy-ag-test--context-infix :input) "changed")
      (should (equal "first" (plist-get (car saved) :input)))
      (should (= 77 (plist-get (car saved) :width))))))

(ert-deftest ivy-ag-test-legacy-shell-template-quotes-only-at-execution ()
  (ivy-ag-test--with-search
    (with-temp-file "keep %s file.txt" (insert "foo bar; $(not-a-command)\n"))
    (dolist (template '(("ag" "--vimgrep" "%s") "ag --nocolor --nogroup --column %s"))
      (let* ((counsel-ag-base-command template)
             (state (ivy-ag--make-state
                     :directory default-directory :input "foo bar; $(not-a-command)"
                     :flags '("--literal" "--file-search-regex=keep %s file") :width 100))
             (output (shell-command-to-string (ivy-ag--reproducible-command state 'ivy))))
        (should (string-match-p "keep %s file.txt:1:1:" output))
        (should (string-match-p (regexp-quote "$(not-a-command)") output))))))

(ert-deftest ivy-ag-test-file-reader-returns-action-result-not-ivy-candidate ()
  ;; Ivy executes the action, but normally returns its current candidate.
  (cl-letf (((symbol-function 'ivy-read)
             (lambda (_prompt _collection &rest args)
               (funcall (plist-get args :action) "/tmp/chosen.txt")
               "chosen.txt")))
    (should (equal (ivy-ag-read-files "Files: " "/tmp/") '("/tmp/chosen.txt")))
    (should (equal (ivy-ag-read-files "Files: " "/tmp/" '("/tmp/marked.txt"))
                   '("/tmp/marked.txt")))))

(ert-deftest ivy-ag-test-file-reader-can-return-an-empty-list ()
  (cl-letf (((symbol-function 'ivy-read)
             (lambda (&rest _)
               (setq ivy-ag--file-selection nil)
               (ivy-ag-file-finish)
               "current.txt"))
            ((symbol-function 'ivy-exit-with-action)
             (lambda (action &rest _) (funcall action "current.txt"))))
    (should-not (ivy-ag-read-files "Files: " "/tmp/" '("/tmp/old.txt")))))

(ert-deftest ivy-ag-test-legacy-transient-history-restores-context ()
  (let* ((transient--prefix
          (transient--init-prefix 'ivy-ag-menu
                                 '(:value ("--directory=/tmp/legacy/"
                                           "--width=51" "--python"))))
         (transient--suffixes
          (transient--flatten-suffixes (transient--init-suffixes 'ivy-ag-menu)))
         (transient-current-command 'ivy-ag-menu)
         (transient-current-prefix transient--prefix)
         (transient-current-suffixes transient--suffixes)
         (state (ivy-ag--menu-state)))
    (should (equal (ivy-ag--state-directory state) "/tmp/legacy/"))
    (should (= (ivy-ag--state-width state) 51))
    (should (equal (ivy-ag--state-flags state) '("--python")))))

(ert-deftest ivy-ag-test-types-reader-respects-customized-map-and-prompt ()
  (let ((ivy-ag-types-map (copy-keymap ivy-ag-types-map))
        prompt actual-map)
    (cl-letf (((symbol-function 'ivy-ag-read-multi)
               (lambda (text _collection &rest args)
                 (setq prompt text actual-map (plist-get args :keymap)))))
      (ivy-ag--read-types)
      (should (eq actual-map ivy-ag-types-map))
      (should (string-match-p "C-c C-k" prompt))
      (should (eq (keymap-lookup actual-map "C-j") #'ivy-alt-done))
      (should (eq (lookup-key actual-map [remap next-line]) #'ivy-next-line))
      (keymap-unset ivy-ag-types-map "C-c C-k")
      (keymap-set ivy-ag-types-map "C-c C-z" #'ivy-ag--clear-type-selection)
      (ivy-ag--read-types)
      (should (string-match-p "C-c C-z" prompt))
      (should-not (string-match-p "C-c C-k" prompt))
      (should (eq (keymap-lookup actual-map "C-c C-z")
                  #'ivy-ag--clear-type-selection))
      (keymap-unset ivy-ag-types-map "C-c C-z")
      (ivy-ag--read-types)
      (should (string-match-p "ivy-ag--clear-type-selection" prompt))
      (should-not (keymap-lookup actual-map "C-c C-k")))))

(ert-deftest ivy-ag-test-change-types-outside-transient ()
  (let ((transient--prefix nil)
        (transient-current-prefix nil)
        (transient-current-command nil)
        (ivy-ag--last (ivy-ag--make-state :directory default-directory
                                         :input "foo.*bar" :width 80
                                         :flags '("--literal" "--python")))
        (ivy-ag--last-input nil)
        (ivy-ag-max-line-length 1000)
        captured premarked)
    (cl-letf (((symbol-function 'ivy-ag-read-multi)
               (lambda (_prompt _collection &rest args)
                 (setq premarked (plist-get args :premarked))
                 '("--elisp")))
              ((symbol-function 'ivy-ag)
               (lambda (&rest args) (setq captured args))))
      (ivy-ag-change-file-type)
      (should (equal premarked '("--python")))
      (should (equal captured
                     (list default-directory "foo.*bar" '("--literal" "--elisp")))))))

(ert-deftest ivy-ag-test-file-reader-help-follows-custom-bindings ()
  (let ((ivy-ag-read-files-map (copy-keymap ivy-ag-read-files-map)))
    (keymap-unset ivy-ag-read-files-map "M-m")
    (keymap-set ivy-ag-read-files-map "C-c C-z" #'ivy-ag-file-mark)
    (let ((help (substitute-command-keys (documentation 'ivy-ag-read-files t))))
      (should (string-match-p "C-c C-z" help))
      (should-not (string-match-p "M-m" help)))
    (should (eq (keymap-lookup ivy-ag-read-files-map "C-j") #'ivy-alt-done))
    (keymap-unset ivy-ag-read-files-map "C-c C-z")
    (should (string-match-p
             "ivy-ag-file-mark"
             (substitute-command-keys (documentation 'ivy-ag-read-files t))))))

(ert-deftest ivy-ag-test-grep-mode-preserves-custom-bindings ()
  (skip-unless (require 'wgrep nil t))
  (let ((ivy-ag-grep-mode-map (copy-keymap ivy-ag-grep-mode-map))
        (wgrep-mode-map (copy-keymap wgrep-mode-map)))
    (keymap-set ivy-ag-grep-mode-map "C-x C-q" #'ignore)
    (define-key ivy-ag-grep-mode-map wgrep-enable-key #'ignore)
    (keymap-set ivy-ag-grep-mode-map "C-c C-z" #'wgrep-change-to-wgrep-mode)
    (with-temp-buffer
      (dotimes (_ 2)
        (ivy-ag-grep-mode)
        (should (eq (key-binding (kbd "C-x C-q")) #'ignore))
        (should (eq (key-binding wgrep-enable-key) #'ignore))
        (should (eq (key-binding (kbd "C-c C-z")) #'wgrep-change-to-wgrep-mode)))
      ;; Remove every binding, including wgrep's own setup binding.
      (dolist (key (where-is-internal #'wgrep-change-to-wgrep-mode
                                     ivy-ag-grep-mode-map))
        (define-key ivy-ag-grep-mode-map key nil))
      (should (string-match-p "wgrep-change-to-wgrep-mode" (ivy-ag--grep-header)))
      (keymap-set ivy-ag-grep-mode-map "C-c C-z" #'wgrep-change-to-wgrep-mode)
      (should (string-match-p "C-c C-z" (ivy-ag--grep-header)))
      (should (eq (key-binding (kbd "n")) #'next-error-no-select))
      (dolist (key (where-is-internal #'wgrep-finish-edit wgrep-mode-map))
        (define-key wgrep-mode-map key nil))
      (keymap-set wgrep-mode-map "C-c C-z" #'wgrep-finish-edit)
      (use-local-map wgrep-mode-map)
      (should (string-match-p "C-c C-z" (ivy-ag--grep-header)))
      (keymap-unset wgrep-mode-map "C-c C-z")
      (should (string-match-p "wgrep-finish-edit" (ivy-ag--grep-header))))))

(ert-deftest ivy-ag-test-reloading-preserves-public-keymaps ()
  (let ((ivy-ag-map (copy-keymap ivy-ag-map))
        (ivy-ag-read-files-map (copy-keymap ivy-ag-read-files-map))
        (ivy-ag-types-map (copy-keymap ivy-ag-types-map))
        (ivy-ag-grep-mode-map (copy-keymap ivy-ag-grep-mode-map)))
    (dolist (map (list ivy-ag-map ivy-ag-read-files-map
                       ivy-ag-types-map ivy-ag-grep-mode-map))
      (keymap-set map "C-c C-z" #'ignore))
    (keymap-unset ivy-ag-types-map "C-c C-k")
    (load (locate-library "ivy-ag.el") nil t t)
    (dolist (map (list ivy-ag-map ivy-ag-read-files-map
                       ivy-ag-types-map ivy-ag-grep-mode-map))
      (should (eq (keymap-lookup map "C-c C-z") #'ignore)))
    (should-not (keymap-lookup ivy-ag-types-map "C-c C-k"))))

(provide 'ivy-ag-test)
;;; ivy-ag-test.el ends here
