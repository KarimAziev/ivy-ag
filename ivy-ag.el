;;; ivy-ag.el --- A front-end for ag (the silver searcher) with ivy interface -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/ivy-ag
;; Keywords: matching, tools
;; Version: 0.3.0
;; Package-Requires: ((emacs "30.1") (ivy "0.13.4") (counsel "0.13.4") (transient "0.13.4"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Search with ag (the silver searcher) through Ivy and Transient.  Change
;; directories and filters while retaining the query, preview matches without
;; visiting files, and save complete searches as named presets.
;;
;; Requires Emacs 30.1 or later, Ivy, Counsel, Transient, and the ag executable.
;; The optional wgrep package enables editing full-line search results.
;;
;; Entry points:
;;
;; `ivy-ag' searches the current project, falling back to the nearest Git root
;; or `default-directory'.  `ivy-ag-default-dir' searches `default-directory'.
;; `ivy-ag-menu' configures the query, directory, file types, filename regexp,
;; exclusions, matching behavior, traversal options, and display width.
;; `ivy-ag-output-menu' configures counts, filenames, context, statistics, and
;; output formats for an asynchronous search in a separate buffer.
;;
;; During an Ivy search, use `ivy-ag-up', `ivy-ag-cd', and
;; `ivy-ag-switch-next-dir' or `ivy-ag-switch-prev-dir' to change the root.
;; `ivy-ag-change-file-type', `ivy-ag-change-file-pattern',
;; `ivy-ag-change-exclusions', and `ivy-ag-toggle-vcs-ignores' change filters.
;; `ivy-ag-minibuffer-menu' opens the menu with the current search settings.
;; `ivy-call' previews a match; accepting it visits the file.  Use
;; `ivy-ag-open-in-other-window' to visit it in another window.
;;
;; `ivy-ag-read-files' returns marked files and directories as absolute paths,
;; retaining marks across directory changes.  Selected exclusions are escaped
;; and converted to relative ag ignore arguments for the current search root.
;; `ivy-ag-read-multi' is also available for selecting multiple candidates.
;;
;; Transient history and saved values restore the complete search context.
;; Named records in `ivy-ag-presets' require a descriptive label and can share
;; a directory.  A preset can optionally supply automatic defaults for its
;; directory and descendants; the most specific matching directory wins.
;;
;; `ivy-ag-edit-results' reruns the search with complete source lines in a
;; grep buffer for wgrep editing.  `ivy-occur' instead exports the collected
;; results as a read-only snapshot.  Output and editable-result searches are
;; independent of the minibuffer's result, output-size, and time limits.
;;
;; Customize bindings in `ivy-ag-map', `ivy-ag-read-files-map',
;; `ivy-ag-types-map', and `ivy-ag-grep-mode-map'.  Prompts and help reflect
;; current bindings.  `ivy-ag-switchable-directories' controls directory
;; cycling.  `ivy-ag-max-results', `ivy-ag-max-output-size', and
;; `ivy-ag-search-timeout' bound minibuffer searches; `ivy-ag-max-line-length'
;; controls result width and long-line previews.  The preview context is set
;; by `ivy-ag-preview-context-lines'.
;;
;; Explicit initial queries retain their Ivy regexp syntax.  Text inferred
;; from a region or symbol uses `ivy-ag-escape-initial-input-chars-regex'.
;; Literal searches bypass quoting and use literal matching and highlighting.
;; See README.org for installation, default bindings, presets, and examples.

;;; Code:

(require 'subr-x)
(require 'seq)

(require 'cl-lib)
(require 'ivy)
(require 'counsel)
(require 'transient)
(require 'grep)

(defgroup ivy-ag nil
  "Search with ag and ivy."
  :link '(url-link :tag "Repository"
                   "https://github.com/KarimAziev/ivy-ag")
  :group 'ivy-ag)

(defcustom ivy-ag-switchable-directories (list user-emacs-directory)
  "List of directories available for switching during searches in minibuffer.


The switching commands (`ivy-ag-switch-next-dir' and `ivy-ag-switch-prev-dir')
cycle through the listed directories while an `ivy-ag' search is active,
restarting the search in the selected directory with the current input and
flags."
  :type '(repeat directory)
  :group 'ivy-ag)

(defcustom ivy-ag-escape-initial-input-chars-regex 'regexp-quote
  "Escaping rule for special characters in the initial search input.

Controls escaping of special characters in the initial minibuffer input.

The value can be nil, a function, or a regular expression string.

When nil, the initial input is inserted unchanged.

When a function, it is called with one argument, the initial input
string, and should return a string to insert or nil to insert nothing.

When a regular expression string, each match in fresh initial text is
prefixed with a backslash.  Existing queries supplied as INIT-INPUT to
`ivy-ag' are preserved verbatim.  Literal searches bypass this option."
  :group 'ivy-ag
  :type '(radio
          (const :tag "Don't escape" nil)
          (function
           :tag "Use custom function"
           :doc
           "Function must accept one argument-an initial input-and return a string or nil"
           regexp-quote)
          (regexp :tag "Regex" "[$*+.?^-]")))


(defcustom ivy-ag-max-results 2000
  "Maximum number of results collected by one search."
  :type 'natnum
  :group 'ivy-ag)

(defcustom ivy-ag-max-line-length 1000
  "Maximum number of characters shown from a matching line.
Navigation uses ag's column number, even when the match is beyond this limit."
  :type 'natnum
  :group 'ivy-ag)

(defcustom ivy-ag-max-output-size (* 2 1024 1024)
  "Maximum number of output characters collected by one search."
  :type 'natnum
  :group 'ivy-ag)

(defcustom ivy-ag-search-timeout 10
  "Maximum number of seconds an ag search may run."
  :type 'number
  :group 'ivy-ag)

(defcustom ivy-ag-preview-context-lines 5
  "Number of context lines around a match when previewing long-line files.
Files whose lines fit `ivy-ag-max-line-length' are previewed in full."
  :type 'natnum
  :group 'ivy-ag)

(defvar ivy-ag--last-input nil)
(defvar ivy-ag--process nil)
(defvar ivy-ag--start-timer nil)
(defvar ivy-ag--update-timer nil)
(defvar ivy-ag--timeout-timer nil)

(defun ivy-ag--cancel-search ()
  "Cancel pending work and dispose of the current search."
  (dolist (timer (list ivy-ag--start-timer ivy-ag--update-timer
                       ivy-ag--timeout-timer))
    (when (timerp timer)
      (cancel-timer timer)))
  (setq ivy-ag--start-timer nil
        ivy-ag--update-timer nil
        ivy-ag--timeout-timer nil)
  (when (processp ivy-ag--process)
    (let ((process ivy-ag--process))
      (setq ivy-ag--process nil)
      (set-process-filter process #'ignore)
      (set-process-sentinel process #'ignore)
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p (process-buffer process))
        (kill-buffer (process-buffer process))))))

(defun ivy-ag--stop-search (process reason)
  "Stop PROCESS, retaining complete results and recording REASON."
  (when (eq process ivy-ag--process)
    (process-put process 'ivy-ag--stop-reason reason)
    (process-put process 'ivy-ag--dirty t)
    (set-process-filter process #'ignore)
    (set-process-sentinel process #'ignore)
    (when (process-live-p process)
      (delete-process process))
    (when (timerp ivy-ag--timeout-timer)
      (cancel-timer ivy-ag--timeout-timer))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (unless (bolp)
        (delete-region (line-beginning-position) (point-max))))))

(defun ivy-ag--filter (process output)
  "Collect bounded OUTPUT from PROCESS without running Ivy or regexps."
  (when (and (eq process ivy-ag--process)
             (buffer-live-p (process-buffer process)))
    (process-put process 'ivy-ag--dirty t)
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (let ((remaining (max 0 (- ivy-ag-max-output-size (buffer-size)))))
        (insert (substring output 0 (min (length output) remaining)))))
    (let ((count (with-current-buffer (process-buffer process)
                   (save-excursion
                     (goto-char (point-max))
                     (count-lines (point-min) (line-beginning-position))))))
      (cond
       ((>= count ivy-ag-max-results)
        (with-current-buffer (process-buffer process)
          (goto-char (point-min))
          (forward-line ivy-ag-max-results)
          (delete-region (point) (point-max)))
        (ivy-ag--stop-search process "result limit"))
       ((with-current-buffer (process-buffer process)
          (>= (buffer-size) ivy-ag-max-output-size))
        (ivy-ag--stop-search process "output limit"))))))

(defun ivy-ag--sentinel (process _event)
  "Record completion of PROCESS; leave display work to the update timer."
  (when (and (eq process ivy-ag--process)
             (memq (process-status process) '(exit signal)))
    (when (timerp ivy-ag--timeout-timer)
      (cancel-timer ivy-ag--timeout-timer))
    (process-put process 'ivy-ag--dirty t)))

(defmacro ivy-ag--with-quit (&rest body)
  "Run BODY interruptibly and cancel the search if the user quits."
  (declare (indent 0) (debug t))
  `(with-local-quit
     (condition-case nil
         (progn ,@body)
       (quit
        (ivy-ag--cancel-search)
        (signal 'quit nil)))))

(defun ivy-ag--publish (process)
  "Display complete results from PROCESS outside its process filter."
  (when (and (eq process ivy-ag--process)
             (active-minibuffer-window)
             (eq (ivy-state-caller ivy-last) 'ivy-ag)
             (eq (process-get process 'ivy-ag--state) ivy-last)
             (process-get process 'ivy-ag--dirty))
    (process-put process 'ivy-ag--dirty nil)
    (ivy-ag--with-quit
      (let* ((finished (not (process-live-p process)))
             (reason (process-get process 'ivy-ag--stop-reason))
             (candidates
              (with-current-buffer (process-buffer process)
                (save-excursion
                  (goto-char (point-max))
                  (unless finished
                    (beginning-of-line))
                  (split-string (buffer-substring-no-properties
                                 (point-min)
                                 (point))
                                "\n" t))))
             (ivy--prompt
              (format "%d%s %s" (length candidates)
                      (cond (reason (concat " [" reason "]"))
                            (finished "")
                            (t "+"))
                      (ivy-state-prompt ivy-last))))
        (when (and finished (not reason)
                   (not (memq (process-exit-status process) '(0 1))))
          (setq counsel--async-last-error-string
                (mapconcat #'identity candidates "\n")
                candidates nil)
          (setq ivy--prompt (format "ag exited %d: %s"
                                    (process-exit-status process)
                                    (ivy-state-prompt ivy-last))))
        (setf (ivy-state-extra-props ivy-last)
              (plist-put (ivy-state-extra-props ivy-last)
                         :ivy-ag--candidates candidates))
        (setq ivy--all-candidates candidates
              ivy--old-cands candidates)
        ;; Do not ask Ivy to re-match/re-sort the results with the query regexp.
        (ivy--insert-minibuffer (ivy--format candidates))
        (when finished
          (when (timerp ivy-ag--update-timer)
            (cancel-timer ivy-ag--update-timer))
          (setq ivy-ag--update-timer nil))))))

(defun ivy-ag--start-search (command directory state)
  "Start COMMAND in DIRECTORY for Ivy STATE."
  (setq ivy-ag--start-timer nil)
  (when (and (active-minibuffer-window) (eq state ivy-last))
    (let* ((default-directory directory)
           (process-connection-type nil)
           (buffer (generate-new-buffer " *ivy-ag*")))
      (condition-case err
          (progn
            (setq ivy-ag--process
                  (if (listp command)
                      (apply #'start-file-process "ivy-ag" buffer command)
                    (start-file-process-shell-command "ivy-ag" buffer command)))
            (set-process-query-on-exit-flag ivy-ag--process nil)
            (setq counsel--async-last-error-string nil)
            (process-put ivy-ag--process 'ivy-ag--state state)
            (set-process-filter ivy-ag--process #'ivy-ag--filter)
            (set-process-sentinel ivy-ag--process #'ivy-ag--sentinel)
            (setq ivy-ag--update-timer
                  (run-at-time 0.1 0.1 #'ivy-ag--publish ivy-ag--process)
                  ivy-ag--timeout-timer
                  (run-at-time ivy-ag-search-timeout nil #'ivy-ag--stop-search
                               ivy-ag--process "time limit")))
        (error (kill-buffer buffer)
               (signal (car err) (cdr err)))))))

(defun ivy-ag--collection (input &rest _)
  "Search for INPUT, or return the current query's collected results.
Ivy calls the collection again to export an occur buffer.  Reuse the
snapshot instead of starting another search for unchanged input."
  (let ((query (list input ivy-case-fold-search
                     (ivy-state-re-builder ivy-last) counsel-ag-command
                     (plist-get (ivy-state-extra-props ivy-last) :ivy-ag--args))))
    (if (and (not (eq this-command 'ivy-resume))
             (equal query (plist-get (ivy-state-extra-props ivy-last)
                                     :ivy-ag--query)))
        (plist-get (ivy-state-extra-props ivy-last) :ivy-ag--candidates)
      (prog1 (ivy-ag--search input)
        (setf (ivy-state-extra-props ivy-last)
              (plist-put (ivy-state-extra-props ivy-last)
                         :ivy-ag--query query))))))

(defun ivy-ag--search (input)
  "Cancel the previous query and search asynchronously for INPUT."
  (ivy-ag--cancel-search)
  (setq ivy-ag--last-input input
        ivy--all-candidates nil
        ivy--old-cands nil)
  (setf (ivy-state-extra-props ivy-last)
        (plist-put (ivy-state-extra-props ivy-last) :ivy-ag--candidates nil))
  (let ((ivy-text (cdr (counsel--split-command-args input))))
    (unless (ivy-more-chars)
      (setq ivy-ag--start-timer
            (run-at-time counsel-async-command-delay nil
                         #'ivy-ag--start-search
                         (ivy-ag--build-command input 'ivy)
                         (ivy-state-directory ivy-last) ivy-last))
      nil)))

(defun ivy-ag--occur (candidates)
  "Export collected CANDIDATES as a read-only Ivy snapshot.
Lines may be truncated, so this deliberately uses `ivy-occur-mode'
instead of an editable grep buffer."
  (let ((directory (ivy-state-directory ivy-last)))
    (ivy-occur-mode)
    ;; Ivy otherwise re-runs the combined regexp for overlays after an occur
    ;; action, bypassing our column-based navigation.
    (setq-local ivy-highlight-grep-commands
                (remq 'ivy-ag ivy-highlight-grep-commands))
    (setq default-directory directory)
    (let ((inhibit-read-only t))
      (insert (format "%d collected candidates (snapshot; lines may be truncated):\n"
                      (length candidates)))
      (dolist (candidate candidates)
        (insert "    " (ivy--format-minibuffer-line candidate) "\n")))
    (goto-char (point-min))
    (read-only-mode 1)))

(defun ivy-ag--unwind ()
  "Cancel search work and remove preview overlays."
  (ivy-ag--cancel-search)
  (ivy-ag--cleanup-preview))

(defvar ivy-ag--configure-keywords
  '(:parent :initial-input :height :occur
            :update-fn :init-fn :unwind-fn
            :index-fn :sort-fn :sort-matches-fn
            :format-fn :display-fn :display-transformer-fn
            :alt-done-fn :more-chars :grep-p :exit-codes))

(defvar ivy-ag--ivy-read-keywords
  '(:predicate :require-match :initial-input
               :history :preselect
               :def :keymap :update-fn :sort
               :unwind :re-builder :matcher
               :dynamic-collection
               :extra-props
               :action :multi-action))

(defmacro ivy-ag--compose (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(ivy-ag--pipe ,@(reverse functions)))

(defun ivy-ag--call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Return stdout output if command existed with zero status, nil otherwise."
  (let ((buff (generate-new-buffer command)))
    (with-current-buffer buff
      (let ((status (apply #'call-process command nil t nil
                           (flatten-list args))))
        (let ((result (string-trim (buffer-string))))
          (if (zerop status)
              (prog1 result (kill-current-buffer))
            (message result) nil))))))

(defun ivy-ag--plist-omit (plist keywords)
  "Omit KEYWORDS with it's values from PLIST."
  (if (seq-find (lambda (it) (memq it plist)) keywords)
      (let ((result))
        (while plist
          (let* ((key (pop plist))
                 (val (pop plist)))
            (unless (memq key keywords)
              (push (list key val) result))))
        (reverse result))
    plist))

(defun ivy-ag--plist-pick (plist keywords)
  "Pick KEYWORDS from PLIST."
  (let ((result)
        (keyword))
    (while (setq keyword (pop keywords))
      (when (memq keyword plist)
        (let ((value (plist-get plist keyword)))
          (setq result (nconc result
                              (list keyword
                                    value))))))
    result))

(defun ivy-ag--mark-candidates (candidates)
  "Mark CANDIDATES from ivy collection."
  (dolist (cand (ivy-state-collection
                 ivy-last))
    (when (member cand
                  candidates)
      (let ((marked-cand (concat
                          ivy-mark-prefix
                          cand)))
        (setq ivy--old-cands
              ivy--all-candidates)
        (setcar
         (member cand
                 ivy--all-candidates)
         (setcar
          (member cand
                  ivy--old-cands)
          marked-cand))
        (setq ivy-marked-candidates
              (append
               ivy-marked-candidates
               (list
                marked-cand)))))))

(defmacro ivy-ag--pipe (&rest functions)
  "Return left-to-right composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (&rest args)
     ,@(let ((init-fn (pop functions)))
         (list
          (seq-reduce
           (lambda (acc fn)
             (if (symbolp fn)
                 `(funcall #',fn ,acc)
               `(funcall ,fn ,acc)))
           functions
           (if (symbolp init-fn)
               `(apply #',init-fn args)
             `(apply ,init-fn args)))))))


(defvar ivy-ag--multi-clear nil
  "Non-nil when a multiple-choice reader explicitly accepts no selections.")

(defun ivy-ag-read-multi (prompt collection &rest ivy-args)
  "Read COLLECTION with PROMPT and return list with selected candidates.
IVY-ARGS are combined args both from `ivy-read' and `ivy-configure',
excluding:

- :action
- :multi-action
- :caller

but accepting:

- :persistent-action
- :premarked

Persistent action will be called with current candidate without exiting
completion.

Premarked is candidates from COLLECTION which should be initially marked."
  (interactive)
  (dolist (alist-sym '(ivy--parents-alist
                       ivy-initial-inputs-alist
                       ivy-height-alist
                       ivy-update-fns-alist
                       ivy-unwind-fns-alist
                       ivy-init-fns-alist
                       ivy-index-functions-alist
                       ivy-sort-functions-alist
                       ivy-sort-matches-functions-alist
                       ivy-format-functions-alist
                       ivy-display-functions-alist
                       ivy--display-transformers-alist
                       ivy-alt-done-functions-alist
                       ivy-more-chars-alist))
    (ivy--alist-set alist-sym 'ivy-ag-read-multi nil))
  (when (and (boundp 'counsel--async-exit-code-plist)
             (plist-get counsel--async-exit-code-plist
                        'ivy-ag-read-multi))
    (setq counsel--async-exit-code-plist
          (ivy-ag--plist-omit counsel--async-exit-code-plist
                             '(ivy-ag-read-multi))))
  (let ((marked)
        (ivy-ag--multi-clear nil)
        (persistent-action (plist-get ivy-args :persistent-action))
        (premarked-candidates (plist-get ivy-args :premarked)))
    (let ((args (append
                 (list prompt
                       collection
                       :caller 'ivy-ag-read-multi
                       :action (lambda (item)
                                 (when (and persistent-action
                                            (null ivy-exit))
                                   (funcall persistent-action item))
                                 item)
                       :multi-action (lambda (children)
                                       (setq marked children)))
                 (ivy-ag--plist-pick
                  ivy-args
                  (seq-difference ivy-ag--ivy-read-keywords
                                  '(:multi-action
                                    :action)))))
          (configure-args (ivy-ag--plist-pick
                           ivy-args
                           ivy-ag--configure-keywords))
          (item))
      (when configure-args
        (push 'ivy-ag-read-multi configure-args)
        (apply #'ivy-configure configure-args))
      (setq item (if premarked-candidates
                     (minibuffer-with-setup-hook
                         (lambda ()
                           (when (active-minibuffer-window)
                             (ivy-ag--mark-candidates premarked-candidates)))
                       (apply #'ivy-read args))
                   (apply #'ivy-read args)))
      (unless ivy-ag--multi-clear
        (or marked (when item (list item)))))))

(cl-defstruct (ivy-ag--state (:constructor ivy-ag--make-state)
                            (:copier ivy-ag--copy-state))
  flags input directory buffer exclusions width output-flags label)

(defvar ivy-ag--last (ivy-ag--make-state))

;; Permit reloading this version over an existing four-field search record.
(when (< (length ivy-ag--last) (length (ivy-ag--make-state)))
  (setq ivy-ag--last
        (ivy-ag--make-state :flags (ivy-ag--state-flags ivy-ag--last)
                            :input (ivy-ag--state-input ivy-ag--last)
                            :directory (ivy-ag--state-directory ivy-ag--last)
                            :buffer (ivy-ag--state-buffer ivy-ag--last))))

(defun ivy-ag--file-parent (path)
  "Return the parent directory to PATH without slash."
  (let ((parent (file-name-directory
                 (directory-file-name
                  (expand-file-name path default-directory)))))
    (when (and (file-exists-p path)
               (file-exists-p parent)
               (not (equal
                     (file-truename (directory-file-name
                                     (expand-file-name path)))
                     (file-truename (directory-file-name
                                     (expand-file-name parent))))))
      (if (file-name-absolute-p path)
          (directory-file-name parent)
        (file-relative-name parent)))))


(defun ivy-ag--resume-in-directory (directory input flags)
  "Resume INPUT with FLAGS in DIRECTORY, keeping other search settings."
  (let ((state (ivy-ag--context-state (ivy-ag--state-context ivy-ag--last))))
    (setf (ivy-ag--state-directory state)
          (file-name-as-directory (expand-file-name directory))
          (ivy-ag--state-input state) input
          (ivy-ag--state-flags state) flags)
    (ivy-ag--accept-state state)
    (ivy-ag (ivy-ag--state-directory state) input flags)))

(defun ivy-ag-cd ()
  "Choose a directory and retain the current query and filters."
  (interactive)
  (let ((input (if (active-minibuffer-window) ivy-text ivy-ag--last-input))
        (flags (ivy-ag--state-flags ivy-ag--last))
        (directory (read-directory-name "Search in: " nil nil t)))
    (if (active-minibuffer-window)
        (ivy-quit-and-run (ivy-ag--resume-in-directory directory input flags))
      (ivy-ag--resume-in-directory directory input flags))))

(ivy-configure 'ivy-ag-cd
  :display-transformer-fn #'abbreviate-file-name)

(defun ivy-ag-up ()
  "Search the parent directory with the current query and settings."
  (interactive)
  (when-let* ((directory (ivy-ag--state-directory ivy-ag--last))
              (parent (ivy-ag--file-parent directory)))
    (let ((input ivy-text) (flags (ivy-ag--state-flags ivy-ag--last)))
      (ivy-quit-and-run (ivy-ag--resume-in-directory parent input flags)))))

(defun ivy-ag-toggle-vcs-ignores ()
  "Toggle VCS ignores while retaining other search settings."
  (interactive)
  (let* ((directory (or (ivy-ag--state-directory ivy-ag--last) default-directory))
         (flags (ivy-ag--state-flags ivy-ag--last))
         (flags (if (member "--skip-vcs-ignores" flags)
                    (remove "--skip-vcs-ignores" flags)
                  (append flags '("--skip-vcs-ignores"))))
         (input (if (active-minibuffer-window) ivy-text ivy-ag--last-input)))
    (if (active-minibuffer-window)
        (ivy-quit-and-run (ivy-ag--resume-in-directory directory input flags))
      (ivy-ag--resume-in-directory directory input flags))))

(defvar ivy-ag--dirs-switchers nil)
(defvar ivy-ag--current-dir-index 0)

(defun ivy-ag--index-switcher (step current-index switch-list)
  "Increase or decrease CURRENT-INDEX depending on STEP value and SWITCH-LIST."
  (cond ((> step 0)
         (if (>= (+ step current-index)
                 (length switch-list))
             0
           (+ step current-index)))
        ((< step 0)
         (if (or (<= 0 (+ step current-index)))
             (+ step current-index)
           (1- (length switch-list))))))

(defun ivy-ag--switch-dir-index (step)
  "Increase or decrease `ivy-ag--current-dir-index' on STEP and resume search."
  (setq ivy-ag--dirs-switchers
        (append
         '(nil)
         (mapcar #'expand-file-name
                 (mapcar #'expand-file-name
                         (seq-filter
                          #'file-exists-p
                          (delete nil ivy-ag-switchable-directories))))))
  (setq ivy-ag--current-dir-index (ivy-ag--index-switcher
                                  step
                                  ivy-ag--current-dir-index
                                  ivy-ag--dirs-switchers))
  (let ((input ivy-text)
        (next-dir (or (nth ivy-ag--current-dir-index
                           ivy-ag--dirs-switchers)
                      (locate-dominating-file
                       default-directory ".git")))
        (flags (ivy-ag--state-flags ivy-ag--last)))
    (setq next-dir (or next-dir default-directory))
    (if (active-minibuffer-window)
        (ivy-quit-and-run (ivy-ag--resume-in-directory next-dir input flags))
      (ivy-ag--resume-in-directory next-dir ivy-ag--last-input flags))))



(defun ivy-ag-switch-next-dir (&optional _rest)
  "Search in next directory defined in `ivy-ag-switchable-directories'."
  (interactive)
  (ivy-ag--switch-dir-index 1))


(defun ivy-ag-switch-prev-dir (&optional _rest)
  "Search in previous directory defined in `ivy-ag-switchable-directories'."
  (interactive)
  (ivy-ag--switch-dir-index -1))

(defun ivy-ag-open-in-other-window ()
  "Jump to search result in other window."
  (interactive)
  (ivy-exit-with-action #'ivy-ag--open-in-other-window-action))

(defun ivy-ag--open-in-other-window-action (candidate)
  "Visit search CANDIDATE in another window."
  (ivy-ag--visit candidate t))

(defun ivy-ag--get-region ()
  "Get current region or nil."
  (when
      (and (region-active-p)
           (use-region-p))
    (string-trim (buffer-substring-no-properties
                  (region-beginning) (region-end)))))

(defvar-keymap ivy-ag-map
  :doc "Minibuffer keymap for `ivy-ag', inheriting Ivy's navigation bindings."
  :parent ivy-minibuffer-map
  "C-<backspace>" #'ivy-ag-up
  "C-l" #'ivy-ag-up
  "C-j" #'ivy-call
  "M-q" #'ivy-ag-edit-results
  "C->" #'ivy-ag-switch-next-dir
  "C-<" #'ivy-ag-switch-prev-dir
  "C-c C-o" #'ivy-ag-open-in-other-window
  "M-." #'ivy-ag-toggle-vcs-ignores
  "C-M-." #'ivy-ag-change-file-type
  "C-c C-f" #'ivy-ag-change-file-pattern
  "C-c C-i" #'ivy-ag-change-exclusions
  "C-." #'ivy-ag-cd
  "C-o" #'ivy-ag-minibuffer-menu
  "C-c M-m" #'ivy-ag-minibuffer-menu)

(defun ivy-ag-minibuffer-menu ()
  "Configure the current query and directory without re-escaping input."
  (interactive)
  (let ((state (ivy-ag--context-state (ivy-ag--state-context ivy-ag--last))))
    (setf (ivy-ag--state-input state) ivy-text)
    (ivy-quit-and-run
      (ivy-ag--get-file-types)
      (transient-setup #'ivy-ag-menu nil nil :value (ivy-ag--state-value state)))))

(defvar ivy-ag--preview-buffer nil)
(defvar ivy-ag--preview-window-configuration nil)

(defun ivy-ag--goto-location (line column)
  "Move to LINE and the one-based byte COLUMN in the current buffer."
  (goto-char (point-min))
  (forward-line (1- line))
  (when column
    (let ((byte (+ (position-bytes (point)) (max 0 (1- column))))
          (end (line-end-position)))
      (goto-char (if (>= byte (position-bytes end)) end
                   (or (byte-to-position byte) (point)))))))

(defun ivy-ag--preview-excerpt (line column)
  "Return bounded text around LINE and byte COLUMN in the current buffer.
Return (TEXT POINT FIRST-LINE).  POINT is the position within TEXT at which
to place point, and FIRST-LINE is the corresponding source line number.
Do not change point, mark, narrowing, or text in the source buffer."
  (save-excursion
    (save-restriction
      (widen)
      (ivy-ag--goto-location line column)
      (let* ((target (point))
             (target-line (line-number-at-pos))
             (first-line (max 1 (- target-line ivy-ag-preview-context-lines)))
             (last-line (+ target-line ivy-ag-preview-context-lines))
             (width (max 1 ivy-ag-max-line-length))
             (current-line first-line)
             (length 0)
             (position 1)
             lines)
        (forward-line (- first-line target-line))
        (while (and (<= current-line last-line) (not (eobp)))
          (let* ((begin (line-beginning-position))
                 (end (line-end-position))
                 (start (if (and (= current-line target-line)
                                 (> (- end begin) width))
                            (max begin (- target (/ width 4)))
                          begin))
                 (text (buffer-substring-no-properties
                        start (min end (+ start width)))))
            (when (= current-line target-line)
              (setq position (+ 1 length (- target start))))
            (push text lines)
            (setq length (+ length (length text) 1)))
          (forward-line 1)
          (cl-incf current-line))
        (list (mapconcat #'identity (nreverse lines) "\n")
              position first-line)))))

(defun ivy-ag--long-lines-p ()
  "Return non-nil if a line exceeds `ivy-ag-max-line-length'."
  (save-excursion
    (goto-char (point-min))
    (let ((width (max 1 ivy-ag-max-line-length))
          found)
      (while (and (not found) (not (eobp)))
        (let ((begin (point)))
          (forward-line 1)
          (setq found (> (- (point) begin (if (eq (char-before) ?\n) 1 0))
                         width))))
      found)))

(defun ivy-ag--preview-content (line column)
  "Return preview text and location for LINE and byte COLUMN.
Return (TEXT POINT FIRST-LINE EXCERPT-P).  Preserve the entire buffer
unless it contains long lines.  Leave the source buffer's state unchanged."
  (save-excursion
    (save-restriction
      (widen)
      (if (ivy-ag--long-lines-p)
          (append (ivy-ag--preview-excerpt line column) '(t))
        (ivy-ag--goto-location line column)
        (list (buffer-substring-no-properties (point-min) (point-max))
              (point) 1 nil)))))

(defun ivy-ag--preview-file (file line column &optional other-window)
  "Preview FILE from disk at LINE and COLUMN without visiting or altering it.
Like `counsel-extra--preview-file', read into a temporary buffer and delay
mode hooks.  Keep ordinary files intact; use an excerpt for long-line files.
Fontify and highlight around point rather than processing the entire file.
When OTHER-WINDOW is non-nil, display the preview in another window."
  (when-let* ((threshold large-file-warning-threshold)
              (attributes (file-attributes file)))
    (when (> (file-attribute-size attributes) threshold)
      (user-error "File too large for preview; select the result to visit it")))
  (let ((content (with-temp-buffer
                   (insert-file-contents file)
                   (ivy-ag--preview-content line column))))
    (unless (buffer-live-p ivy-ag--preview-buffer)
      (setq ivy-ag--preview-buffer (generate-new-buffer " *ivy-ag-preview*")))
    (swiper--cleanup)
    (with-current-buffer ivy-ag--preview-buffer
      (let ((inhibit-read-only t))
        (fundamental-mode)
        (erase-buffer)
        (insert (nth 0 content))
        ;; This binding is for mode detection only.  The preview never becomes
        ;; a visiting buffer and neither file hooks nor local variables run.
        (let ((buffer-file-name file)
              (enable-local-variables nil)
              (enable-local-eval nil))
          (delay-mode-hooks
            (set-auto-mode)))
        (setq-local default-directory (file-name-directory file))
        (when (nth 3 content)
          (setq-local truncate-lines t))
        (setq-local header-line-format
                    (format "%s:%d — preview%s" (abbreviate-file-name file) line
                            (if (nth 3 content) " excerpt" "")))
        (setq-local display-line-numbers-offset (1- (nth 2 content)))
        (setq-local buffer-read-only t)
        (setq mark-active nil)
        (set-marker (mark-marker) nil)
        (goto-char (min (point-max)
                        (nth 1 content)))
        (font-lock-ensure (line-beginning-position (- (window-height)))
                          (line-end-position (window-height)))
        (set-buffer-modified-p nil)))
    (unless ivy-ag--preview-window-configuration
      (setq ivy-ag--preview-window-configuration (current-window-configuration)))
    (funcall (if other-window #'switch-to-buffer-other-window
               #'switch-to-buffer)
             ivy-ag--preview-buffer)
    (let* ((input (cdr (counsel--split-command-args ivy-text)))
           (ivy--old-re (funcall (ivy-state-re-builder ivy-last) input))
           (regexp (ivy-re-to-str ivy--old-re))
           (inhibit-quit nil))
      (swiper--add-overlays regexp nil nil (selected-window)))))

(defun ivy-ag--cleanup-preview ()
  "Dispose of the preview and restore the windows it temporarily replaced."
  (swiper--cleanup)
  (when ivy-ag--preview-window-configuration
    (set-window-configuration ivy-ag--preview-window-configuration)
    (setq ivy-ag--preview-window-configuration nil))
  (when (buffer-live-p ivy-ag--preview-buffer)
    (kill-buffer ivy-ag--preview-buffer))
  (setq ivy-ag--preview-buffer nil))

(defun ivy-ag--visit (candidate &optional other-window)
  "Preview CANDIDATE, or visit it after accepting the search.
OTHER-WINDOW displays the result in another window."
  (when (string-match
         "\\`\\(.*?\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)?" candidate)
    (let ((file (expand-file-name (match-string-no-properties 1 candidate)
                                  (ivy-state-directory ivy-last)))
          (line (string-to-number (match-string 2 candidate)))
          (column (when (match-string 3 candidate)
                    (string-to-number (match-string 3 candidate)))))
      (if (not (eq ivy-exit 'done))
          (let ((inhibit-quit nil))
            (condition-case err
                (ivy-ag--preview-file file line column other-window)
              (error
               (when-let* ((window (active-minibuffer-window)))
                 (select-window window))
               (signal (car err) (cdr err)))))
        (ivy-ag--cleanup-preview)
        (funcall (if other-window #'find-file-other-window #'find-file) file)
        ;; An intentional jump must not extend a previously active region.
        (deactivate-mark t)
        (widen)
        (ivy-ag--goto-location line column)
        (swiper--ensure-visible)
        (run-hooks 'counsel-grep-post-action-hook)))))

(defun ivy-ag--grep-action (candidate)
  "Visit search CANDIDATE in the current window."
  (ivy-ag--visit candidate))

(defun ivy-ag--read-file-type (&optional prompt initial-input history)
  "Read file type choices with completion, preselecting the default type.

Optional argument PROMPT is the prompt string, defaulting to
\"File type: \".

Optional argument INITIAL-INPUT is the initial input string,
defaulting to nil.

Optional argument HISTORY is the history variable for completion,
defaulting to nil."
  (let ((types (mapcar #'car (ivy-ag--get-file-types))))
    (ivy-ag-read-multi (or prompt "File type: ") types
                       :history history
                       :initial-input initial-input
                       :preselect
                       (ivy-ag--get-default-file-type))))

(defvar ivy-ag--file-types nil)

(defun ivy-ag--get-file-types ()
  "Return `ag' file types as a cached alist.

The value is obtained by calling:

  ag --list-file-types

and parsing its output into an alist of the form:

  ((\"--TYPE\" \".ext1\" \".ext2\" ...)
   ...)

where each car is an `ag' file type switch (for example
\"--python\" or \"--cc\") and the remaining elements are file
extensions associated with that type.

The result is cached in `ivy-ag--file-types' after the first call
and reused on subsequent calls."
  (or ivy-ag--file-types
      (let* ((alist (ivy-ag--call-process "ag" "--list-file-types"))
             (file-types
              (and alist
                   (seq-drop-while
                    (ivy-ag--compose
                     not (apply-partially #'string-prefix-p "--"))
                    (split-string alist nil t)))))
        (setq ivy-ag--file-types
              (nreverse
               (seq-reduce
                (lambda (acc curr)
                  (setq acc
                        (push
                         (if (string-prefix-p "--" curr)
                             curr
                           (let* ((val (pop acc))
                                  (cell
                                   (if (not (consp val))
                                       (list val curr)
                                     (setcdr val (nconc (cdr val)
                                                        (list curr)))
                                     val)))
                             cell))
                         acc)))
                file-types '()))))))

(defun ivy-ag--get-default-file-type ()
  "Get default file type for current buffer filename."
  (when-let* ((ext (when buffer-file-name
                     (file-name-extension buffer-file-name)))
              (file-types (ivy-ag--get-file-types)))
    (setq ext (concat "." ext))
    (car (seq-find (ivy-ag--compose (apply-partially #'member ext)
                                   'cdr)
                   file-types))))

(defun ivy-ag--edit-filter (kind)
  "Edit file filter KIND and restart with the current complete search."
  (let ((state (ivy-ag--context-state (ivy-ag--state-context ivy-ag--last))))
    (setf (ivy-ag--state-input state)
          (if (active-minibuffer-window) ivy-text (ivy-ag--state-input state)))
    (let ((edit
           (lambda ()
             (let ((default-directory (or (ivy-ag--state-directory state)
                                          default-directory)))
               (pcase kind
                 ('types
                  (let ((selected
                         (seq-filter (lambda (flag) (assoc flag (ivy-ag--get-file-types)))
                                     (ivy-ag--state-flags state))))
                    (setf (ivy-ag--state-flags state)
                          (append (seq-difference (ivy-ag--state-flags state)
                                                  selected #'equal)
                                  (ivy-ag--read-types selected)))))
                 ('pattern
                  (let ((pattern (read-string
                                  "Filename regex (empty clears): "
                                  (ivy-ag--option-value "--file-search-regex="
                                                        (ivy-ag--state-flags state)))))
                    (setf (ivy-ag--state-flags state)
                          (append (ivy-ag--without-options
                                   (ivy-ag--state-flags state) '("--file-search-regex"))
                                  (unless (string-empty-p pattern)
                                    (list (concat "--file-search-regex=" pattern)))))))
                 ('exclusions
                  (setf (ivy-ag--state-exclusions state)
                        (ivy-ag-read-files "Exclude paths: " default-directory
                                           (ivy-ag--state-exclusions state)))))
               (unless (ivy-ag--state-directory state)
                 (setf (ivy-ag--state-directory state) default-directory))
               (ivy-ag--accept-state state)
               (ivy-ag default-directory (or (ivy-ag--state-input state) "")
                       (ivy-ag--state-flags state))))))
      (if (active-minibuffer-window)
          (ivy-quit-and-run (funcall edit))
        (funcall edit)))))

(defun ivy-ag-change-file-type ()
  "Edit file types without losing other search settings."
  (interactive)
  (ivy-ag--edit-filter 'types))

(defun ivy-ag-change-file-pattern ()
  "Edit the filename regex without losing the current query."
  (interactive)
  (ivy-ag--edit-filter 'pattern))

(defun ivy-ag-change-exclusions ()
  "Select concrete paths to exclude from the current search."
  (interactive)
  (ivy-ag--edit-filter 'exclusions))

(defun ivy-ag-edit-results ()
  "Leave the minibuffer and rerun this search with full lines for wgrep."
  (interactive)
  (let ((state (ivy-ag--context-state (ivy-ag--state-context ivy-ag--last))))
    (setf (ivy-ag--state-input state) ivy-text)
    (ivy-quit-and-run (ivy-ag--run-buffer state 'grep))))

(defvar ivy-ag-history nil
  "History for `ivy-ag'.")

(defun ivy-ag--current-project-root ()
  "Return the current project's root directory or the nearest Git directory."
  (require 'project nil t)
  (or
   (when-let* ((project (ignore-errors (project-current nil))))
     (ignore-errors
       (if (fboundp 'project-root)
           (project-root project)
         (with-no-warnings
           (car (project-roots project))))))
   (locate-dominating-file
    default-directory ".git")))


;;;###autoload
(cl-defun ivy-ag (&optional directory init-input (flags nil flags-supplied-p))
  "Search DIRECTORY with INIT-INPUT and FLAGS.
Omitting FLAGS reuses this directory's settings or its automatic preset.
Explicit nil FLAGS means no flags.  Non-nil INIT-INPUT is an existing
Ivy query and is preserved verbatim, including the empty string.  Text
inferred from the region or symbol is quoted according to
`ivy-ag-escape-initial-input-chars-regex', except in literal mode."
  (interactive)
  (setq directory (file-name-as-directory
                   (expand-file-name
                    (or directory (ivy-ag--current-project-root)
                        default-directory))))
  (let* ((same-directory (equal directory (ivy-ag--state-directory ivy-ag--last)))
         (preset (and (not flags-supplied-p) (not same-directory)
                      (ivy-ag--automatic-preset directory)))
         (state (cond
                 (same-directory (ivy-ag--copy-state ivy-ag--last))
                 (preset (ivy-ag--preset-state preset directory))
                 (t (ivy-ag--make-state :directory directory))))
         (flags (ivy-ag--normalize-flags
                 (if flags-supplied-p flags (ivy-ag--state-flags state))))
         (input (cond ((stringp init-input) init-input)
                      ((and preset (not (string-empty-p (or (ivy-ag--state-input state) ""))))
                       (ivy-ag--state-input state))
                      (t (ivy-ag--initial-text
                          (or (ivy-ag--get-region)
                              (when-let* ((symbol (symbol-at-point)))
                                (symbol-name symbol)))
                          (append (ivy-ag--template-arguments counsel-ag-base-command)
                                  flags))))))
    (when same-directory
      (setf (ivy-ag--state-width state) ivy-ag-max-line-length))
    (setf (ivy-ag--state-directory state) directory
          (ivy-ag--state-flags state) flags
          (ivy-ag--state-input state) input)
    (setq ivy-ag--last-input input)
    (when-let* ((width (ivy-ag--option-value "--width=" flags)))
      (setf (ivy-ag--state-width state) (string-to-number width)
            (ivy-ag--state-flags state) (ivy-ag--without-options flags '("--width"))))
    (when (ivy-ag--state-width state)
      (setq ivy-ag-max-line-length (max 1 (ivy-ag--state-width state))))
    (setq ivy-ag--last state)
    (setq counsel-ag-command counsel-ag-base-command)
    (setq counsel--regex-look-around counsel--grep-tool-look-around)
    (counsel-require-program counsel-ag-command)
    (let ((default-directory directory)
          (history-add-new-input nil))
      (unwind-protect
          (ivy-read (format "%s: " (abbreviate-file-name directory))
                    #'ivy-ag--collection
                    :initial-input (or input "")
                    :re-builder (ivy-ag--make-re-builder
                                 (append (ivy-ag--template-arguments counsel-ag-command)
                                         (ivy-ag--state-flags state)))
                    :extra-props (list :ivy-ag--args
                                       (append (ivy-ag--state-flags state)
                                               (ivy-ag--exclusion-args
                                                (ivy-ag--state-exclusions state) directory)))
                    :dynamic-collection t :keymap ivy-ag-map
                    :history 'ivy-ag-history :action #'ivy-ag--grep-action
                    :require-match t :caller 'ivy-ag)
        (ivy-ag--unwind)
        (setf (ivy-ag--state-input state) ivy-ag--last-input)))
    (when (and history-add-new-input
               (stringp ivy-ag--last-input)
               (not (string-empty-p ivy-ag--last-input)))
      (add-to-history 'ivy-ag-history
                      (substring-no-properties ivy-ag--last-input)))))

(ivy-add-actions 'ivy-ag
                 '(("j" ivy-ag--open-in-other-window-action "other window")))

(ivy-configure 'ivy-ag
  :occur #'ivy-ag--occur
  :unwind-fn #'ivy-ag--unwind
  :index-fn #'ivy-recompute-index-zero
  :display-transformer-fn #'counsel-git-grep-transformer
  :grep-p t
  :exit-codes '(1 "No matches found"))

;;;###autoload
(defun ivy-ag-default-dir ()
  "Perfoms search in default directory."
  (interactive)
  (funcall-interactively #'ivy-ag default-directory))


;;; Search settings and menus

(defconst ivy-ag--option-names
  '("--file-search-regex" "--ignore" "--ignore-dir" "--path-to-ignore"
    "--depth" "--width" "--max-count" "--after" "--before" "--context"
    "--color-line-number" "--color-match" "--color-path" "--filename-pattern")
  "Options whose next argument is a value.")

(defconst ivy-ag--flag-aliases
  '(("-Q" . "--literal") ("-F" . "--literal") ("--fixed-strings" . "--literal")
    ("-i" . "--ignore-case") ("-s" . "--case-sensitive") ("-S" . "--smart-case")
    ("-G" . "--file-search-regex") ("-p" . "--path-to-ignore")
    ("-m" . "--max-count") ("-W" . "--width") ("-w" . "--word-regexp")
    ("-u" . "--unrestricted") ("-U" . "--skip-vcs-ignores")
    ("-a" . "--all-types") ("-t" . "--all-text") ("-f" . "--follow")
    ("-z" . "--search-zip") ("-v" . "--invert-match")
    ("-A" . "--after") ("-B" . "--before") ("-C" . "--context")
    ("-c" . "--count") ("-l" . "--files-with-matches")
    ("-L" . "--files-without-matches") ("-g" . "--filename-pattern")
    ("-o" . "--only-matching") ("-0" . "--null"))
  "Canonical spellings used by the search settings and menu.")

(defun ivy-ag--normalize-flags (flags)
  "Return FLAGS with aliases and separate option values normalized.
Each result is one argv element; spaces in values are preserved."
  (let (result)
    (while flags
      (let* ((flag (pop flags))
             (eq-pos (string-match "=" flag))
             (name (if eq-pos (substring flag 0 eq-pos) flag))
             (name (or (cdr (assoc name ivy-ag--flag-aliases)) name)))
        (push (cond (eq-pos (concat name (substring flag eq-pos)))
                    ((and flags (member name ivy-ag--option-names))
                     (concat name "=" (pop flags)))
                    (t name))
              result)))
    (nreverse result)))

(defun ivy-ag--option-value (name flags)
  "Return the last value of option NAME (including =) in FLAGS."
  (let (value)
    (dolist (flag flags value)
      (when (string-prefix-p name flag)
        (setq value (substring flag (length name)))))))

(defun ivy-ag--without-options (flags names)
  "Remove options NAMES and their values from normalized FLAGS."
  (seq-remove (lambda (flag)
                (member (car (split-string flag "=")) names))
              flags))

(defun ivy-ag--format-command (template arguments)
  "Insert argv ARGUMENTS into TEMPLATE without losing argument boundaries.
List templates execute directly.  String templates are trusted shell
commands; only their substituted arguments are shell-quoted."
  (if (listp template)
      (cl-mapcan (lambda (part)
                   (if (equal part "%s") (copy-sequence arguments) (list part)))
                 template)
    (replace-regexp-in-string
     "%s" (mapconcat (lambda (arg) (if (equal arg "%s") arg
                                      (shell-quote-argument arg)))
                       arguments " ") template t t)))

(defun ivy-ag--template-arguments (template)
  "Read options from TEMPLATE for detecting literal and case settings."
  (ivy-ag--normalize-flags
   (cdr (if (listp template) template (split-string-and-unquote template)))))

(defun ivy-ag--literal-p (flags)
  "Return non-nil if FLAGS request literal search."
  (member "--literal" (ivy-ag--normalize-flags flags)))

(defun ivy-ag--initial-text (text flags)
  "Prepare fresh region or symbol TEXT according to FLAGS and customization."
  (when text
    (setq text (substring-no-properties text))
    (cond ((or (ivy-ag--literal-p flags)
               (null ivy-ag-escape-initial-input-chars-regex)) text)
          ((functionp ivy-ag-escape-initial-input-chars-regex)
           (funcall ivy-ag-escape-initial-input-chars-regex text))
          (t (replace-regexp-in-string
              ivy-ag-escape-initial-input-chars-regex
              (lambda (match) (concat "\\" match)) text t t)))))

(defun ivy-ag--make-re-builder (flags)
  "Return a highlighting builder honoring FLAGS and inline literal switches."
  (let ((builder (or (cdr (assq 'ivy-ag ivy-re-builders-alist))
                     (cdr (assq t ivy-re-builders-alist)) #'ivy--regex)))
    (lambda (text)
      (let* ((parts (counsel--split-command-args text))
             (literal (ivy-ag--literal-p
                       (append flags (split-string-and-unquote (car parts))))))
        (funcall (if literal #'regexp-quote builder) (cdr parts))))))

(defconst ivy-ag--output-option-names
  '("--vimgrep" "--ackmate" "--color" "--nocolor" "--column"
    "--numbers" "--nonumbers" "--filename" "--nofilename"
    "--heading" "--noheading" "--group" "--nogroup" "--break" "--nobreak"
    "--count" "--files-with-matches" "--files-without-matches"
    "--filename-pattern" "--only-matching" "--null" "--print0"
    "--print-all-files" "--stats" "--stats-only" "--passthrough"
    "--after" "--before" "--context" "--width"
    "--color-line-number" "--color-match" "--color-path")
  "Output options replaced by each result destination's format.")

(defun ivy-ag--search-template (template)
  "Remove output formatting from TEMPLATE, keeping its search options."
  (if (listp template)
      (cons (car template)
            (ivy-ag--without-options
             (ivy-ag--normalize-flags (cdr template)) ivy-ag--output-option-names))
    ;; Preserve shell syntax in legacy templates rather than reparsing it.
    (let ((result template))
      (dolist (name ivy-ag--output-option-names result)
        (setq result
              (replace-regexp-in-string
               (concat "\\(?:\\`\\|[ \t]\\)" (regexp-quote name)
                       (if (member name ivy-ag--option-names)
                           "\\(?:=[^ \t]+\\|[ \t]+[^ \t]+\\)"
                         "\\b"))
               " " result t t))))))

(defun ivy-ag--build-command (input destination &optional output-options)
  "Build the command for INPUT and DESTINATION using `counsel-ag-command'.
DESTINATION is `ivy', `grep', or `output'.  OUTPUT-OPTIONS only affect
`output'.  Literal text bypasses Ivy's regexp-to-PCRE conversion."
  (let* ((parts (counsel--split-command-args input))
         (inline (ivy-ag--normalize-flags (split-string-and-unquote (car parts))))
         (search-args (plist-get (ivy-state-extra-props ivy-last) :ivy-ag--args))
         (flags (append (ivy-ag--template-arguments counsel-ag-command) search-args inline))
         (text (cdr parts))
         (ivy-text text)
         (case-flag (car (last (seq-filter
                               (lambda (flag)
                                 (member flag '("--ignore-case" "--case-sensitive"
                                                "--smart-case"))) flags))))
         (regex (if (ivy-ag--literal-p flags) text (counsel--grep-regex text)))
         (format-args
          (pcase destination
            ('ivy (list "--nocolor" "--vimgrep" "--width"
                        (number-to-string (max 1 ivy-ag-max-line-length))))
            ('grep '("--nocolor" "--nogroup" "--numbers" "--filename"
                     "--print-long-lines"))
            ('output (append '("--nocolor") output-options))
            (_ (error "Unknown ag destination: %s" destination)))))
    (setq ivy-case-fold-search
          (pcase case-flag
            ("--ignore-case" t) ("--case-sensitive" nil)
            ("--smart-case" 'auto) (_ ivy-case-fold-search)))
    (ivy-ag--format-command
     (ivy-ag--search-template counsel-ag-command)
     (append (ivy-ag--without-options search-args ivy-ag--output-option-names)
             (unless case-flag
               (list (if (ivy--case-fold-p text) "-i" "-s")))
             (unless (ivy-ag--literal-p flags)
               (split-string (or (counsel--ag-extra-switches regex) "")))
             (ivy-ag--without-options inline ivy-ag--output-option-names)
             format-args
             (if (and (eq destination 'output)
                      (ivy-ag--option-value "--filename-pattern=" output-options))
                 '("--" ".")
               (list "--" regex "."))))))

(defun ivy-ag--exclusion-args (paths directory)
  "Return anchored relative --ignore-dir arguments for PATHS in DIRECTORY.
Escape ignore-pattern metacharacters so selections denote concrete paths.
Selections outside DIRECTORY are retained in settings but have no effect."
  (let ((root (file-name-as-directory (expand-file-name directory))))
    (delq nil
          (mapcar
           (lambda (path)
             (let ((relative
                    (file-relative-name (directory-file-name path) root)))
               (cond ((equal relative ".")
                      (user-error "Cannot exclude the search root itself"))
                     ((or (file-name-absolute-p relative)
                          (equal relative "..")
                          (string-prefix-p "../" relative))
                      nil)
                     (t (concat "--ignore-dir=/"
                                (replace-regexp-in-string
                                 "[][?*\\\\]" (lambda (s)
                                                (concat "\\" s))
                                 relative t t))))))
           paths))))

(defvar ivy-ag--file-result nil
  "Result captured by the active file reader's exit action.")

(defvar ivy-ag--file-selection nil
  "Absolute paths selected by the active multiple-file reader.")

(defun ivy-ag--file-candidate-path (candidate)
  "Resolve CANDIDATE against the file reader's current directory."
  (directory-file-name
   (expand-file-name (substring-no-properties candidate)
                     (or ivy--directory default-directory))))

(defun ivy-ag-file-mark ()
  "Mark the current file or directory, retaining its absolute identity."
  (interactive)
  (cl-pushnew (ivy-ag--file-candidate-path (ivy-state-current ivy-last))
              ivy-ag--file-selection :test #'equal)
  (ivy-next-line)
  (ivy--exhibit))

(defun ivy-ag-file-unmark ()
  "Unmark the current file or directory."
  (interactive)
  (setq ivy-ag--file-selection
        (delete (ivy-ag--file-candidate-path (ivy-state-current ivy-last))
                ivy-ag--file-selection))
  (ivy--exhibit))

(defun ivy-ag-file-toggle-marks ()
  "Toggle marks for visible paths without losing marks in other directories."
  (interactive)
  (dolist (candidate ivy--old-cands)
    (unless (member candidate '("./" "../"))
      (let ((path (ivy-ag--file-candidate-path candidate)))
        (if (member path ivy-ag--file-selection)
            (setq ivy-ag--file-selection (delete path ivy-ag--file-selection))
          (push path ivy-ag--file-selection)))))
  (ivy--exhibit))

(defun ivy-ag-file-clear-marks ()
  "Clear all selected paths, including marks in other directories."
  (interactive)
  (setq ivy-ag--file-selection nil)
  (ivy--exhibit))

(defun ivy-ag-file-finish ()
  "Accept the marked paths, allowing an explicitly empty selection."
  (interactive)
  (ivy-exit-with-action
   (lambda (_) (setq ivy-ag--file-result (copy-sequence ivy-ag--file-selection)))))

(defvar-keymap ivy-ag-read-files-map
  :doc "File navigation plus persistent marks for `ivy-ag-read-files'."
  :parent (make-composed-keymap counsel-find-file-map ivy-minibuffer-map)
  "M-m" #'ivy-ag-file-mark
  "M-u" #'ivy-ag-file-unmark
  "M-U" #'ivy-ag-file-clear-marks
  "M-a" #'ivy-ag-file-toggle-marks
  "C-c C-c" #'ivy-ag-file-finish)

(defun ivy-ag--file-display (candidate)
  "Display CANDIDATE with its cross-directory selection mark."
  (concat (if (member (ivy-ag--file-candidate-path candidate)
                     ivy-ag--file-selection) "> " "  ") candidate))

(defun ivy-ag-read-files (prompt &optional directory selected)
  "Read multiple files or directories with PROMPT, starting in DIRECTORY.
Return absolute paths.  SELECTED supplies initial marks.
\\<ivy-ag-read-files-map>\\[ivy-ag-file-mark] marks, \\[ivy-ag-file-unmark] unmarks,
and \\[ivy-ag-file-clear-marks] clears all marks.
\\[ivy-alt-done] navigates directories.  \\[ivy-done] accepts marks, or the
current path if none are marked.  \\[ivy-ag-file-finish] accepts marks even
when empty.  \\[minibuffer-keyboard-quit] cancels without changing settings."
  (let ((default-directory (file-name-as-directory
                            (expand-file-name (or directory default-directory))))
        (ivy-ag--file-result nil)
        (ivy-ag--file-selection (mapcar #'directory-file-name selected)))
    (ivy-read prompt #'read-file-name-internal
              :require-match t :history 'file-name-history
              :keymap ivy-ag-read-files-map :caller 'ivy-ag-read-files
              :action (lambda (candidate)
                        (setq ivy-ag--file-result
                              (or (copy-sequence ivy-ag--file-selection)
                                  (list (ivy-ag--file-candidate-path candidate))))))
    ivy-ag--file-result))

(ivy-configure 'ivy-ag-read-files
  :parent 'read-file-name-internal
  :display-transformer-fn #'ivy-ag--file-display)

(defcustom ivy-ag-presets nil
  "Named search presets, each a plist with a nonempty :label.
Presets store :directory, :input, :flags, :exclusions, :width and
:output-flags.  Multiple presets may use the same directory.  A preset
with :automatic t supplies defaults in that directory and descendants;
the most specific matching directory wins.  Explicit search arguments
and an existing search in the same directory take precedence.
Use the menu's save, load, delete and automatic-default commands to
manage these records.  Saving uses `customize-save-variable'."
  :type '(repeat (sexp :tag "Named search preset"))
  :group 'ivy-ag)

(defun ivy-ag--state-context (state)
  "Return a serializable, independent context for STATE."
  (copy-tree
   (list :directory (ivy-ag--state-directory state)
         :input (or (ivy-ag--state-input state) "")
         :flags (ivy-ag--state-flags state)
         :exclusions (ivy-ag--state-exclusions state)
         :width (or (ivy-ag--state-width state) ivy-ag-max-line-length)
         :output-flags (ivy-ag--state-output-flags state)
         :label (ivy-ag--state-label state))))

(defun ivy-ag--context-state (context)
  "Create an independent search state from CONTEXT."
  (let ((context (copy-tree context)))
    (ivy-ag--make-state
     :directory (plist-get context :directory)
     :input (plist-get context :input) :flags (plist-get context :flags)
     :exclusions (plist-get context :exclusions) :width (plist-get context :width)
     :output-flags (plist-get context :output-flags) :label (plist-get context :label))))

(defun ivy-ag--state-value (state &optional output)
  "Serialize STATE as Transient values, using output flags when OUTPUT."
  (let* ((copy (ivy-ag--context-state (ivy-ag--state-context state)))
         (flags (ivy-ag--normalize-flags (ivy-ag--state-flags copy)))
         (width (ivy-ag--option-value "--width=" flags)))
    (setf (ivy-ag--state-flags copy) (ivy-ag--without-options flags '("--width"))
          (ivy-ag--state-output-flags copy)
          (ivy-ag--normalize-flags (ivy-ag--state-output-flags copy)))
    (when width (setf (ivy-ag--state-width copy) (max 1 (string-to-number width))))
    (cons (cons "ivy-ag-context" (ivy-ag--state-context copy))
          (copy-sequence (if output (ivy-ag--state-output-flags copy)
                           (ivy-ag--state-flags copy))))))

(defun ivy-ag--automatic-preset (directory)
  "Find the most specific automatic preset for DIRECTORY."
  (let ((directory (file-name-as-directory (expand-file-name directory))) found)
    (dolist (preset ivy-ag-presets found)
      (when-let* ((_ (plist-get preset :automatic))
                  (root (plist-get preset :directory))
                  (root (file-name-as-directory (expand-file-name root))))
        (when (and (string-prefix-p root directory)
                   (or (not found)
                       (> (length root)
                          (length (file-name-as-directory
                                   (expand-file-name (plist-get found :directory)))))))
          (setq found preset))))))

(defun ivy-ag--preset-state (preset &optional directory)
  "Return PRESET as a search state, optionally retaining DIRECTORY."
  (let ((state (ivy-ag--context-state preset)))
    (when directory (setf (ivy-ag--state-directory state) directory))
    state))

(defun ivy-ag--menu-initial-state ()
  "Return the current directory's search state or automatic defaults."
  (let* ((directory (file-name-as-directory (expand-file-name default-directory)))
         (preset (ivy-ag--automatic-preset directory)))
    (cond ((equal directory (ivy-ag--state-directory ivy-ag--last))
           (let ((state (ivy-ag--context-state (ivy-ag--state-context ivy-ag--last))))
             (setf (ivy-ag--state-width state) ivy-ag-max-line-length)
             state))
          (preset (ivy-ag--preset-state preset directory))
          (t (ivy-ag--make-state :directory directory
                                :width ivy-ag-max-line-length :input "")))))

(defun ivy-ag--value-context (value)
  "Read context from VALUE, accepting history from the older menu format."
  (or (cdr (assoc "ivy-ag-context" value))
      (let* ((flags (ivy-ag--normalize-flags (seq-filter #'stringp value)))
             (state (ivy-ag--menu-initial-state))
             (directory (ivy-ag--option-value "--directory=" flags))
             (width (ivy-ag--option-value "--width=" flags)))
        (when directory
          (setf (ivy-ag--state-directory state)
                (file-name-as-directory (expand-file-name directory))))
        (when width (setf (ivy-ag--state-width state) (max 1 (string-to-number width))))
        (setf (ivy-ag--state-flags state)
              (ivy-ag--without-options flags '("--directory" "--width")))
        (ivy-ag--state-context state))))

(defclass ivy-ag--context-infix (transient-infix)
  ((field :initarg :field)
   (prefix :initform nil)
   (multi-value :initform t))
  "Edit prefix scope and serialize it through the directory infix.
Recording a context value lets native Transient history, saved values,
and named presets restore scope together with the actual ag switches.")

(cl-defmethod transient-init-value ((obj ivy-ag--context-infix))
  "Restore OBJ and scope from the recorded search context."
  (oset obj prefix transient--prefix)
  (oset transient--prefix scope
        (copy-tree (ivy-ag--value-context (oref transient--prefix value))))
  (oset obj value (plist-get (oref transient--prefix scope) (oref obj field))))

(cl-defmethod transient-infix-read ((obj ivy-ag--context-infix))
  "Read the contextual setting represented by OBJ."
  (let* ((scope (oref transient--prefix scope))
         (directory (plist-get scope :directory))
         (default-directory directory)
         (value (oref obj value)))
    (pcase (oref obj field)
      (:directory (file-name-as-directory
                   (expand-file-name (read-directory-name "Search in: " directory nil t))))
      (:input (read-string "Search query: " value 'ivy-ag-history))
      (:width (string-to-number
               (transient-read-number-N+ "Display width: "
                                         (number-to-string value) nil)))
      (:exclusions (ivy-ag-read-files "Exclude paths: " directory value)))))

(cl-defmethod transient-infix-set ((obj ivy-ag--context-infix) value)
  "Set OBJ to VALUE and update the prefix's scope."
  (oset obj value value)
  (oset transient--prefix scope
        (plist-put (copy-tree (oref transient--prefix scope)) (oref obj field) value)))

(cl-defmethod transient-infix-value ((obj ivy-ag--context-infix))
  "Serialize scope once, through the directory OBJ."
  (when (eq (oref obj field) :directory)
    (cons "ivy-ag-context" (copy-tree (oref (oref obj prefix) scope)))))

(cl-defmethod transient-format-value ((obj ivy-ag--context-infix))
  "Display OBJ's contextual value without command-line syntax."
  (let ((value (oref obj value)))
    (propertize (pcase (oref obj field)
                  (:directory (abbreviate-file-name value))
                  (:exclusions (format "%d selected" (length value)))
                  (_ (format "%s" (or value ""))))
                'face (if value 'transient-value 'transient-inactive-value))))

(transient-define-infix ivy-ag-menu-directory ()
  "Change the search directory without changing selected absolute exclusions."
  :class ivy-ag--context-infix :field :directory :description "Directory")
(transient-define-infix ivy-ag-menu-input ()
  "Edit the query verbatim."
  :class ivy-ag--context-infix :field :input :description "Search query")
(transient-define-infix ivy-ag-menu-width ()
  "Set `ivy-ag-max-line-length' for the next search and its previews."
  :class ivy-ag--context-infix :field :width :description "Display width")
(transient-define-infix ivy-ag-menu-exclusions ()
  "Select concrete files or directories to exclude."
  :class ivy-ag--context-infix :field :exclusions :description "Excluded paths")

(defclass ivy-ag--types-infix (transient-option)
  ((argument :initform "") (multi-value :initform 'repeat))
  "A set of ag file-type switches, stored as ordinary argv elements.")

(cl-defmethod transient-init-value ((obj ivy-ag--types-infix))
  "Initialize OBJ from known file-type switches."
  (oset obj value (seq-filter
                   (lambda (flag) (and (stringp flag)
                                      (assoc flag (ivy-ag--get-file-types))))
                   (oref transient--prefix value))))

(defun ivy-ag--clear-type-selection ()
  "Finish file-type selection with no type filters."
  (interactive)
  (setq ivy-ag--multi-clear t)
  (ivy-exit-with-action #'ignore))

(defvar-keymap ivy-ag-types-map
  :doc "Keymap for selecting file types with `ivy-ag-read-multi'."
  :parent ivy-minibuffer-map
  "C-c C-k" #'ivy-ag--clear-type-selection)

(defun ivy-ag--read-types (&optional selected)
  "Read file types with SELECTED premarked, allowing an empty selection."
  (let ((types (mapcar #'car (ivy-ag--get-file-types))))
    (ivy-ag-read-multi
     (substitute-command-keys
      "\\<ivy-ag-types-map>File types (\\[ivy-ag--clear-type-selection] clears): ")
     types :keymap ivy-ag-types-map :premarked selected)))

(cl-defmethod transient-infix-read ((obj ivy-ag--types-infix))
  "Read multiple types for OBJ, supporting an explicit empty selection."
  (ivy-ag--read-types (oref obj value)))

(transient-define-infix ivy-ag-menu--file-type ()
  "Choose file types.
\\<ivy-ag-types-map>Use \\[ivy-ag--clear-type-selection] to clear the restriction."
  :class ivy-ag--types-infix :description "File types")

(defclass ivy-ag--case-infix (transient-switches) ()
  "A case-mode cycle with a compact description of the selected mode.")

(cl-defmethod transient-format-value ((obj ivy-ag--case-infix))
  "Display OBJ's selected mode, including the default Ivy behavior."
  (propertize (pcase (oref obj value)
                ("--ignore-case" "ignore case")
                ("--case-sensitive" "case sensitive")
                ("--smart-case" "smart case")
                (_ "Ivy default"))
              'face (if (oref obj value) 'transient-value 'transient-inactive-value)))

(transient-define-infix ivy-ag-menu-case ()
  "Cycle automatic Ivy casing, ignore case, case sensitive and smart case."
  :class ivy-ag--case-infix :description "Case mode"
  :argument-format "--%s"
  :argument-regexp "\\`\\(--\\(?:ignore-case\\|case-sensitive\\|smart-case\\)\\)\\'"
  :choices '("ignore-case" "case-sensitive" "smart-case"))

(defconst ivy-ag--menu-options
  '("--directory" "--width" "--file-search-regex" "--ignore" "--ignore-dir" "--path-to-ignore"
    "--all-types" "--hidden" "--skip-vcs-ignores" "--unrestricted"
    "--ignore-case" "--case-sensitive" "--smart-case" "--literal"
    "--word-regexp" "--invert-match" "--depth" "--max-count"
    "--follow" "--all-text" "--search-zip" "--one-device" "--search-binary")
  "Search options represented by dedicated infixes.")

(defclass ivy-ag--extra-infix (transient-option)
  ((argument :initform "") (multi-value :initform 'repeat))
  "Preserve search switches without a dedicated menu infix.")

(cl-defmethod transient-init-value ((obj ivy-ag--extra-infix))
  "Collect otherwise unrepresented arguments for OBJ."
  (oset obj value
        (seq-filter (lambda (flag)
                      (and (stringp flag)
                           (not (member (car (split-string flag "=")) ivy-ag--menu-options))
                           (not (assoc flag (ivy-ag--get-file-types)))))
                    (oref transient--prefix value))))

(cl-defmethod transient-infix-read ((obj ivy-ag--extra-infix))
  "Read extra argv elements for OBJ using double quotes around spaces."
  (let ((flags (ivy-ag--normalize-flags
                (split-string-and-unquote
                 (read-string "Extra search switches (double-quote spaces): "
                              (combine-and-quote-strings (oref obj value)))))))
    (dolist (flag flags)
      (let ((name (car (split-string flag "="))))
        (when (or (member name ivy-ag--menu-options)
                  (member name ivy-ag--output-option-names)
                  (assoc flag (ivy-ag--get-file-types)))
          (user-error "Use the dedicated search or output control for %s" name))))
    flags))

(transient-define-infix ivy-ag-menu-extra ()
  "Edit switches without a dedicated menu control."
  :class ivy-ag--extra-infix :description "Extra search switches")

(defun ivy-ag--menu-state ()
  "Collect the complete search represented by the current Transient."
  (let* ((command transient-current-command)
         (args (transient-args command))
         (state (ivy-ag--context-state (cdr (assoc "ivy-ag-context" args))))
         (flags (seq-filter #'stringp args)))
    (if (eq command 'ivy-ag-output-menu)
        (setf (ivy-ag--state-output-flags state) flags)
      (setf (ivy-ag--state-flags state) flags))
    state))

(defun ivy-ag--menu-command (state destination)
  "Build STATE's command for DESTINATION without starting a search."
  (let* ((counsel-ag-command counsel-ag-base-command)
         (args (append (ivy-ag--state-flags state)
                       (ivy-ag--exclusion-args (ivy-ag--state-exclusions state)
                                               (ivy-ag--state-directory state))))
         (ivy-ag-max-line-length (ivy-ag--state-width state))
         (ivy-case-fold-search ivy-case-fold-search-default)
         (counsel--regex-look-around counsel--grep-tool-look-around)
         (ivy-last (make-ivy-state
                    :extra-props (list :ivy-ag--args args)
                    :re-builder (ivy-ag--make-re-builder
                                 (append (ivy-ag--template-arguments counsel-ag-command) args)))))
    (ivy-ag--build-command (or (ivy-ag--state-input state) "") destination
                          (ivy-ag--state-output-flags state))))

(defun ivy-ag--shell-command (command)
  "Represent argv COMMAND as a shell command, or retain a shell template."
  (if (stringp command) command (mapconcat #'shell-quote-argument command " ")))

(defun ivy-ag--reproducible-command (state destination)
  "Return a shell command reproducing STATE and DESTINATION."
  (concat "cd " (shell-quote-argument (ivy-ag--state-directory state)) " && "
          (ivy-ag--shell-command (ivy-ag--menu-command state destination))))

(transient-define-suffix ivy-ag-menu-args ()
  "Copy the exact command and working directory for this menu's destination."
  :description "Copy command" :transient t
  (interactive)
  (let ((command (ivy-ag--reproducible-command
                  (ivy-ag--menu-state)
                  (if (eq transient-current-command 'ivy-ag-output-menu) 'output 'ivy))))
    (kill-new command)
    (message "%s" command)))

(transient-define-suffix ivy-ag-show-transient-args ()
  "Display the exact command and working directory."
  :description "Show command" :transient t
  (interactive)
  (message "%s" (ivy-ag--reproducible-command
                  (ivy-ag--menu-state)
                  (if (eq transient-current-command 'ivy-ag-output-menu) 'output 'ivy))))

(defun ivy-ag--accept-state (state)
  "Remember STATE and apply its display width."
  (setq ivy-ag--last state
        ivy-ag--last-input (ivy-ag--state-input state)
        ivy-ag-max-line-length (max 1 (or (ivy-ag--state-width state)
                                         ivy-ag-max-line-length))))

(transient-define-suffix ivy-ag-menu-run ()
  "Search using the complete settings shown in the menu."
  :description "Search in Ivy"
  (interactive)
  (let ((state (ivy-ag--menu-state)))
    (ivy-ag--accept-state state)
    (ivy-ag (ivy-ag--state-directory state) (ivy-ag--state-input state)
            (ivy-ag--state-flags state))))

(declare-function wgrep-setup "ext:wgrep")
(declare-function wgrep-change-to-wgrep-mode "ext:wgrep")
(defvar wgrep-original-mode-map)

(defvar-keymap ivy-ag-grep-mode-map
  :doc "Keymap for full source-line results in `ivy-ag-grep-mode'."
  :parent grep-mode-map
  "C-x C-q" #'wgrep-change-to-wgrep-mode)

(defun ivy-ag--grep-header ()
  "Describe the available editing commands using their current bindings."
  (substitute-command-keys
   (if (and (boundp 'wgrep-mode-map)
            (eq (current-local-map) wgrep-mode-map))
       "\\<wgrep-mode-map>Full source lines — \\[wgrep-finish-edit] applies edits; \\[wgrep-abort-changes] cancels"
     "\\<ivy-ag-grep-mode-map>Full source lines — \\[wgrep-change-to-wgrep-mode] edits with wgrep")))

(define-derived-mode ivy-ag-grep-mode grep-mode "ivy-ag Grep"
  "Full source-line search results with optional wgrep editing."
  (when (require 'wgrep nil t)
    ;; Wgrep setup unconditionally installs `wgrep-enable-key'.  Let it
    ;; initialize buffer state without overwriting our user's bindings.
    (let ((map (current-local-map)))
      (unwind-protect
          (progn
            (use-local-map (copy-keymap map))
            (wgrep-setup))
        (use-local-map map))
      (setq-local wgrep-original-mode-map map)))
  (setq-local header-line-format '(:eval (ivy-ag--grep-header))))

(defun ivy-ag--run-buffer (state destination)
  "Run STATE asynchronously into a buffer for DESTINATION."
  (require 'grep)
  (when (eq destination 'grep)
    (unless (require 'wgrep nil t)
      (user-error "Install wgrep to edit search results")))
  (ivy-ag--accept-state state)
  (let* ((default-directory (ivy-ag--state-directory state))
         (command (ivy-ag--shell-command (ivy-ag--menu-command state destination)))
         (buffer (compilation-start
                  command (if (eq destination 'grep) 'ivy-ag-grep-mode 'compilation-mode)
                  (lambda (_) (generate-new-buffer-name
                               (if (eq destination 'grep) "*ivy-ag edit*" "*ivy-ag output*"))))))
    buffer))

(transient-define-suffix ivy-ag-menu-edit-results ()
  "Run a full-line grep search suitable for wgrep editing."
  :description "Editable results (wgrep)"
  (interactive)
  (ivy-ag--run-buffer (ivy-ag--menu-state) 'grep))

(transient-define-suffix ivy-ag-menu-output-run ()
  "Run with the selected output options in a separate buffer."
  :description "Run in output buffer"
  (interactive)
  (ivy-ag--run-buffer (ivy-ag--menu-state) 'output))

(defun ivy-ag--preset-candidates ()
  "Return completion entries identifying presets by label and directory."
  (mapcar (lambda (preset)
            (cons (format "%s — %s%s" (plist-get preset :label)
                          (abbreviate-file-name (plist-get preset :directory))
                          (if (plist-get preset :automatic) " [automatic]" ""))
                  preset))
          ivy-ag-presets))

(defun ivy-ag--read-preset (prompt)
  "Read a named preset with PROMPT."
  (unless ivy-ag-presets (user-error "No saved ivy-ag presets"))
  (let ((candidates (ivy-ag--preset-candidates)))
    (cdr (assoc (completing-read prompt candidates nil t) candidates))))

(transient-define-suffix ivy-ag-menu-save-preset ()
  "Save the complete search under a required descriptive label.
Saving an existing label updates that preset, preserving its automatic status."
  :description "Save named preset" :transient t
  (interactive)
  (let* ((state (ivy-ag--menu-state))
         (label (string-trim (read-string "Preset label/description: "
                                          (ivy-ag--state-label state))))
         (old (seq-find (lambda (p) (equal label (plist-get p :label))) ivy-ag-presets)))
    (when (string-empty-p label) (user-error "A preset needs a descriptive label"))
    (setf (ivy-ag--state-label state) label)
    (let ((preset (ivy-ag--state-context state)))
      (when (plist-get old :automatic)
        (setq preset (plist-put preset :automatic t)))
      (customize-save-variable
       'ivy-ag-presets
       (cons preset
             (mapcar
              (lambda (p)
                (if (and (plist-get preset :automatic)
                         (equal (plist-get preset :directory) (plist-get p :directory)))
                    (plist-put (copy-tree p) :automatic nil)
                  p))
              (seq-remove (lambda (p) (equal label (plist-get p :label)))
                          ivy-ag-presets)))))
    (transient-prefix-set
     (ivy-ag--state-value state (eq transient-current-command 'ivy-ag-output-menu)))
    (message "Saved preset: %s" label)))

(transient-define-suffix ivy-ag-menu-load-preset ()
  "Restore a named preset, including directory, query and all options."
  :description "Load preset" :transient t
  (interactive)
  (transient-prefix-set
   (ivy-ag--state-value (ivy-ag--preset-state (ivy-ag--read-preset "Load preset: "))
                        (eq transient-current-command 'ivy-ag-output-menu))))

(transient-define-suffix ivy-ag-menu-delete-preset ()
  "Delete a selected named preset."
  :description "Delete preset" :transient t
  (interactive)
  (let ((preset (ivy-ag--read-preset "Delete preset: ")))
    (customize-save-variable 'ivy-ag-presets (remove preset ivy-ag-presets))))

(transient-define-suffix ivy-ag-menu-default-preset ()
  "Toggle a preset as its directory's automatic default.
Enabling a preset replaces any automatic preset for that same directory."
  :description "Toggle automatic preset"
  :transient t
  (interactive)
  (let* ((preset (ivy-ag--read-preset "Automatic preset: "))
         (directory (plist-get preset :directory))
         (enabled (not (plist-get preset :automatic))))
    (customize-save-variable
     'ivy-ag-presets
     (mapcar (lambda (p)
               (let ((copy (copy-tree p)))
                 (when (equal directory (plist-get p :directory))
                   (setq copy (plist-put copy :automatic (and
                                                          (equal p preset) enabled))))
                 copy))
             ivy-ag-presets))
    (message "%s automatic preset: %s" (if enabled "Enabled" "Disabled")
             (plist-get preset :label))))

(transient-define-group ivy-ag--context-group
  [:description
   (lambda ()
     (substitute-command-keys
      "\\<transient-map>Search context (\\[transient-history-prev] / \\[transient-history-next] restore complete searches)"))
   ("d" ivy-ag-menu-directory)
   ("=" ivy-ag-menu-input)
   ("F" ivy-ag-menu-exclusions)
   ("L" ivy-ag-menu-width)])

(transient-define-group ivy-ag--preset-group
  [:description (lambda ()
                  (let ((label (plist-get (transient-scope) :label)))
                    (if label (concat "Presets — " label) "Presets")))
   ("P" ivy-ag-menu-load-preset) ("S" ivy-ag-menu-save-preset)
   ("C-c d" ivy-ag-menu-delete-preset) ("C-c a" ivy-ag-menu-default-preset)])


;;;###autoload (autoload 'ivy-ag-menu "ivy-ag" nil t)
(transient-define-prefix ivy-ag-menu ()
  "Configure a search, select a named preset, or restore complete history."
  :value (lambda () (ivy-ag--state-value (ivy-ag--menu-initial-state)))
  :incompatible '(("--ignore-case" "--case-sensitive" "--smart-case"))
  ivy-ag--context-group
  [["Files"
    ("." ivy-ag-menu--file-type)
    ("f" "Filename regex" "--file-search-regex=" :always-read t)
    ("i" "Ignore patterns" "--ignore=" :multi-value repeat)
    ("I" "Ignore names/patterns" "--ignore-dir=" :multi-value repeat)
    ("p" "Ignore file" "--path-to-ignore=" :reader transient-read-existing-file)
    ("a" "All file types" "--all-types")
    ("h" "Hidden files" "--hidden")
    ("v" "Skip VCS ignores" "--skip-vcs-ignores")
    ("u" "Unrestricted" "--unrestricted")]
   ["Matching"
    ("c" ivy-ag-menu-case)
    ("n" "Literal text" "--literal")
    ("w" "Whole words" "--word-regexp")
    ("V" "Invert match" "--invert-match")
    ("D" "Directory depth" "--depth=" :reader transient-read-number-N0)
    ("M" "Matches per file" "--max-count=" :reader transient-read-number-N+)]
   ["Traversal"
    ("s" "Follow symlinks" "--follow")
    ("T" "All text files" "--all-text")
    ("z" "Compressed files" "--search-zip")
    ("o" "One device" "--one-device")
    ("b" "Binary files" "--search-binary")
    ("+" ivy-ag-menu-extra)]]
  ivy-ag--preset-group
  ["Actions"
   ("C-c C-a" ivy-ag-show-transient-args) ("M-w" ivy-ag-menu-args)
   ("e" ivy-ag-menu-edit-results)
   ("O" "Output options" ivy-ag-output-menu :transient transient--do-exit)
   ("RET" ivy-ag-menu-run)]
  (interactive)
  (ivy-ag--get-file-types)
  (transient-setup #'ivy-ag-menu))

(transient-define-suffix ivy-ag-output-back ()
  "Return to the search menu, retaining the configured output options."
  :description "Back to search settings"
  (interactive)
  (transient-setup #'ivy-ag-menu nil nil
                   :value (ivy-ag--state-value (ivy-ag--menu-state))))


;;;###autoload (autoload 'ivy-ag-output-menu "ivy-ag" nil t)
(transient-define-prefix ivy-ag-output-menu ()
  "Configure output for a separate asynchronous ag buffer.
The output menu retains the search menu's query, filters and context."
  :value (lambda () (ivy-ag--state-value (ivy-ag--menu-initial-state) t))
  :incompatible '(("--count" "--files-with-matches" "--files-without-matches"
                   "--stats-only" "--filename-pattern=")
                  ("--vimgrep" "--ackmate")
                  ("--numbers" "--nonumbers") ("--heading" "--noheading")
                  ("--filename" "--nofilename") ("--break" "--nobreak"))
  ivy-ag--context-group
  [["Output"
    ("c" "Counts" "--count")
    ("l" "Files with matches" "--files-with-matches")
    ("u" "Files without matches" "--files-without-matches")
    ("g" "List matching filenames" "--filename-pattern=" :always-read t)
    ("o" "Only matching text" "--only-matching")
    ("s" "Statistics" "--stats")
    ("t" "Statistics only" "--stats-only")
    ("a" "All searched filenames" "--print-all-files")]
   ["Context and format"
    ("A" "After" "--after=" :reader transient-read-number-N0)
    ("B" "Before" "--before=" :reader transient-read-number-N0)
    ("C" "Context" "--context=" :reader transient-read-number-N0)
    ("W" "Output width" "--width=" :reader transient-read-number-N+)
    ("z" "Print long lines" "--print-long-lines")
    ("n" "Line numbers" "--numbers")
    ("N" "No line numbers" "--nonumbers")
    ("H" "Headings" "--heading")
    ("h" "No headings" "--noheading")
    ("b" "Break between files" "--break")
    ("k" "No breaks" "--nobreak")]
   ["Machine formats"
    ("v" "Vimgrep" "--vimgrep")
    ("K" "AckMate" "--ackmate")
    ("0" "NUL-separated filenames" "--null")
    ("j" "Column numbers" "--column")
    ("f" "Filenames" "--filename")
    ("x" "No filenames" "--nofilename")]]
  ivy-ag--preset-group
  ["Actions"
   ("C-c C-a" ivy-ag-show-transient-args) ("M-w" ivy-ag-menu-args)
   ("e" ivy-ag-menu-edit-results) ("q" ivy-ag-output-back)
   ("RET" ivy-ag-menu-output-run)]
  (interactive)
  (let ((state (if (eq transient-current-command 'ivy-ag-menu)
                   (ivy-ag--menu-state) (ivy-ag--menu-initial-state))))
    (transient-setup #'ivy-ag-output-menu nil nil
                     :value (ivy-ag--state-value state t))))

(provide 'ivy-ag)
;;; ivy-ag.el ends here
