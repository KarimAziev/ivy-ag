;;; ivy-ag.el --- A front-end for ag (the silver searcher) with ivy interface -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/ivy-ag
;; Keywords: matching, tools
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (ivy "0.13.4") (counsel "0.13.4"))

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

;; A front-end for ag (the silver searcher) with ivy interface.
;; The package provides functionality like `counsel-ag' but allows to
;; traverse upwards search directory and some extra switches.

;; Commands

;; M-x `ivy-ag' (&optional dir init-input flags)
;;      Execute ag command in DIRECTORY with INIT-INPUT and FLAGS.
;;      Default value for DIRECTORY is current git project or default directory.

;; M-x `ivy-ag-default-dir'
;;      Execute ag command in default directory.

;; Minibuffer commands

;; During executing `ivy-ag' and `ivy-ag-default-dir' such commands available in the minibuffer:

;; M-x `ivy-ag-open-in-other-window'
;;      Jump to search result in other window.

;; M-x `ivy-ag-switch-prev-dir' (&optional _rest)
;;      Search in previous directory defined in `ivy-ag-switchable-directories'.

;; M-x `ivy-ag-switch-next-dir' (&optional _rest)
;;      Search in next directory defined in `ivy-ag-switchable-directories'.

;; M-x `ivy-ag-toggle-vcs-ignores'
;;      Toggle vcs ignore.

;; M-x `ivy-ag-up'
;;      Change current ag directory to parent directory and resume searching.

;; M-x `ivy-ag-cd'
;;      Read directory name and start or resume ag search in it.

;; M-x `ivy-ag-change-file-type'
;;      Read supported file types and perform search.


;; Customization

;; `ivy-ag-initial-input-chars'
;;      Chars in the same format as for `skip-chars-forward'.
;;      They are used to determine word at point for initial input.

;; `ivy-ag-switchable-directories'
;;      List of directories, which can be switched in minibuffer.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'cl-lib)
(require 'ivy)
(require 'counsel)

(defgroup ivy-ag nil
  "Search with ag and ivy."
  :link '(url-link :tag "Repository"
                   "https://github.com/KarimAziev/ivy-ag")
  :group 'ivy-ag)

(defcustom ivy-ag-switchable-directories (list user-emacs-directory)
  "List of directories for switching.
They can be switched with commands `ivy-ag-switch-next-dir'
and `ivy-ag-switch-prev-dir'."
  :type '(repeat directory)
  :group 'ivy-ag)

(defcustom ivy-ag-initial-input-chars "-*_~$A-Za-z0-9:.#\\+"
  "Chars in the same format as for `skip-chars-forward'.
They are used to determine word at point for initial input."
  :type 'string
  :group 'ivy-ag)

(defcustom ivy-ag-escape-initial-input-chars-regex 'regexp-quote
  "Escaping rule for special characters in the initial search input.

Controls escaping of special characters in the initial minibuffer input.

The value can be nil, a function, or a regular expression string.

When nil, the initial input is inserted unchanged.

When a function, it is called with one argument, the initial input
string, and should return a string to insert or nil to insert nothing.

When a regular expression string, each match in the inserted initial
input is prefixed with a backslash, except when already escaped or
inside a string syntax context."
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
                 (unless finished (beginning-of-line))
                 (split-string (buffer-substring-no-properties
                                (point-min) (point)) "\n" t))))
            (ivy--prompt
             (format "%d%s %s" (length candidates)
                     (cond (reason (concat " [" reason "]"))
                           (finished "") (t "+"))
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
                     (ivy-state-re-builder ivy-last) counsel-ag-command)))
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
  (let* ((parts (counsel--split-command-args input))
         (ivy-text (cdr parts)))
    (or (ivy-more-chars)
        (let* ((regex (counsel--grep-regex ivy-text))
               (switches (concat (if (ivy--case-fold-p ivy-text) " -i " " -s ")
                                 (counsel--ag-extra-switches regex)
                                 (car parts)
                                 (format " --nocolor --vimgrep --width %d "
                                         (max 1 ivy-ag-max-line-length))))
               (command (counsel--format-ag-command
                         switches
                         (if (listp counsel-ag-command) regex
                           (shell-quote-argument regex)))))
          ;; With a pipe, ag otherwise treats stdin as the search target for
          ;; legacy command templates that do not include --vimgrep.
          (setq command (if (listp command) (append command '("."))
                          (concat command " .")))
          (setq ivy-ag--start-timer
                (run-at-time counsel-async-command-delay nil
                             #'ivy-ag--start-search command
                             (ivy-state-directory ivy-last) ivy-last))
          nil))))

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

;;;###autoload
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
      (or marked
          (when item (list item))))))

(cl-defstruct (ivy-ag--state (:constructor ivy-ag--make-state)
                            (:copier ivy-ag--copy-state))
  flags input directory buffer)

(defvar ivy-ag--last (ivy-ag--make-state))

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

;;;###autoload
(defun ivy-ag-cd ()
  "Read directory name and start or resume ag search in it."
  (interactive)
  (if (active-minibuffer-window)
      (let ((input ivy-text)
            (new-dir))
        (setq new-dir (read-directory-name "Search in:\s"))
        (ivy-quit-and-run
          (funcall-interactively #'ivy-ag new-dir input)))
    (funcall-interactively #'ivy-ag (read-directory-name "Search in:\s"))))

(ivy-configure 'ivy-ag-cd
  :display-transformer-fn #'abbreviate-file-name)

;;;###autoload
(defun ivy-ag-up ()
  "Change current ag directory to parent directory and resume searching."
  (interactive)
  (when-let* ((current-dir (ivy-ag--state-directory ivy-ag--last))
              (parent (ivy-ag--file-parent current-dir)))
    (let ((input ivy-text))
      (ivy-quit-and-run
        (funcall #'ivy-ag (file-name-as-directory parent) input)))))

;;;###autoload
(defun ivy-ag-toggle-vcs-ignores ()
  "Toggle vcs ignore."
  (interactive)
  (let ((flags (if (member "--skip-vcs-ignores"
                           (ivy-ag--state-flags
                            ivy-ag--last))
                   (setf (ivy-ag--state-flags ivy-ag--last)
                         (remove "--skip-vcs-ignores"
                                 (ivy-ag--state-flags
                                  ivy-ag--last)))
                 (append (ivy-ag--state-flags ivy-ag--last)
                         '("--skip-vcs-ignores"))))
        (input ivy-text))
    (if (eq 'ivy-ag (ivy-state-caller ivy-last))
        (ivy-quit-and-run
          (funcall-interactively #'ivy-ag
                                 (ivy-ag--state-directory ivy-ag--last)
                                 input flags))
      (funcall-interactively #'ivy-ag (ivy-ag--state-directory ivy-ag--last)
                             input flags))))

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
    (if (minibuffer-window-active-p (selected-window))
        (ivy-quit-and-run
          (funcall-interactively #'ivy-ag next-dir input flags))
      (funcall-interactively #'ivy-ag
                             (nth ivy-ag--current-dir-index
                                  ivy-ag--dirs-switchers)
                             nil (ivy-ag--state-flags ivy-ag--last)))))

;;;###autoload
(defun ivy-ag-switch-next-dir (&optional _rest)
  "Search in next directory defined in `ivy-ag-switchable-directories'."
  (interactive)
  (ivy-ag--switch-dir-index 1))

;;;###autoload
(defun ivy-ag-switch-prev-dir (&optional _rest)
  "Search in previous directory defined in `ivy-ag-switchable-directories'."
  (interactive)
  (ivy-ag--switch-dir-index -1))

;;;###autoload
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

(defvar ivy-ag-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<backspace>") #'ivy-ag-up)
    (define-key map (kbd "C-l") #'ivy-ag-up)
    (define-key map (kbd "C-j") #'ivy-call)
    (define-key map (kbd "M-q") #'counsel-git-grep-query-replace)
    (define-key map (kbd "C->") #'ivy-ag-switch-next-dir)
    (define-key map (kbd "C-<")  #'ivy-ag-switch-prev-dir)
    (define-key map (kbd "C-c C-o") #'ivy-ag-open-in-other-window)
    (define-key map (kbd "M-.") #'ivy-ag-toggle-vcs-ignores)
    (define-key map (kbd "C-M-.") #'ivy-ag-change-file-type)
    (define-key map (kbd "C-.") #'ivy-ag-cd)
    map))

(defvar ivy-ag--preview-buffer nil)
(defvar ivy-ag--preview-window-configuration nil)

(defcustom ivy-ag-preview-context-lines 5
  "Number of context lines around a match when previewing long-line files.
Files whose lines fit `ivy-ag-max-line-length' are previewed in full."
  :type 'natnum
  :group 'ivy-ag)

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
      (user-error "File too large for preview; use RET to visit it")))
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

(defun ivy-ag--read-file-type ()
  "Read multiple file types in the minibuffer, with completion."
  (let ((types (mapcar #'car (ivy-ag--get-file-types))))
    (ivy-ag-read-multi "File type: " types
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

;;;###autoload
(defun ivy-ag-change-file-type ()
  "Read supported file types and perform search."
  (interactive)
  (if (active-minibuffer-window)
      (let ((input ivy-text)
            (dir (ivy-ag--state-directory ivy-ag--last)))
        (progn
          (put 'quit 'error-message "")
          (run-at-time nil nil
                       (lambda (directory text-input)
                         (put 'quit 'error-message "Quit")
                         (with-demoted-errors "Error: %S"
                           (let ((file-type (ivy-ag--read-file-type)))
                             (funcall-interactively #'ivy-ag
                                                    directory
                                                    text-input file-type))))
                       dir input)
          (abort-recursive-edit)))
    (funcall-interactively #'ivy-ag nil nil (append
                                             (ivy-ag--state-flags ivy-ag--last)
                                             (ivy-ag--read-file-type)))))
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


(defun ivy-ag--format-prompt (prompt max-w)
  "Truncate PROMPT to fit MAX-W and append a trailing colon separator.

Argument PROMPT is a string to be split into parts for formatting.

Argument MAX-W is a maximum display width as an integer."
  (let* ((parts (split-string prompt nil t))
         (dir (pop parts))
         (done))
    (if (>= (string-width dir) max-w)
        dir
      (while (and parts
                  (not done)
                  (< (string-width dir) max-w))
        (let* ((str (pop parts))
               (next (concat dir " " str)))
          (unless (and (not parts)
                       (string= "%s" str))
            (if (< (string-width next) max-w)
                (setq dir (concat dir " " str))
              (setq done t)))))
      (concat dir (if (string-suffix-p ":" dir)
                      " " ": ")))))


;;;###autoload
(defun ivy-ag (&optional directory init-input flags)
  "Execute ag command in DIRECTORY with INIT-INPUT and FLAGS.
Default value for DIRECTORY is the current git project or default directory."
  (interactive)
  (unless directory
    (setq directory
          (if-let* ((dir (ivy-ag--current-project-root)))
              (expand-file-name dir)
            default-directory)))
  (let* ((initial-input (seq-find (lambda (it)
                                    (and (stringp it)
                                         (not (string-blank-p it))))
                                  `(,init-input
                                    ,(or (ivy-ag--get-region)
                                      (when-let* ((symb (symbol-at-point)))
                                       (format "%s" (symbol-name symb)))))))
         (input (and initial-input
                     (substring-no-properties initial-input))))
    (setq flags
          (delete-dups
           (if (and
                (null flags)
                (equal directory (ivy-ag--state-directory ivy-ag--last))
                (ivy-ag--state-flags ivy-ag--last))
               (ivy-ag--state-flags ivy-ag--last)
             (or flags '("--smart-case")))))
    (setf (ivy-ag--state-flags ivy-ag--last)
          flags)
    (setf (ivy-ag--state-directory ivy-ag--last) directory)
    (minibuffer-with-setup-hook
        (lambda ()
          (when (and input
                     (active-minibuffer-window))
            (cond ((stringp ivy-ag-escape-initial-input-chars-regex)
                   (insert input)
                   (let ((max (- (point)
                                 (length input))))
                     (save-excursion
                       (while (re-search-backward
                               ivy-ag-escape-initial-input-chars-regex max t 1)
                         (unless (or (looking-back "[\\]" 0)
                                     (nth 3 (syntax-ppss (point))))
                           (insert "\\"))))))
                  ((functionp ivy-ag-escape-initial-input-chars-regex)
                   (let ((inp
                          (ignore-errors
                            (funcall ivy-ag-escape-initial-input-chars-regex
                                     input))))
                     (when inp
                       (insert inp)))))))
      (unwind-protect
          (progn
            (setq counsel-ag-command counsel-ag-base-command)
            (setq counsel--regex-look-around
                  counsel--grep-tool-look-around)
            (counsel-require-program counsel-ag-command)
            (setq counsel-ag-command
                  (counsel--format-ag-command
                   (string-join flags "\s") "%s"))
            (let ((prompt
                   (ivy-ag--format-prompt
                    (format "%s %s:\s" (abbreviate-file-name
                                        directory)
                            counsel-ag-command)
                    (- (frame-width)
                       (or (and input
                                (string-width input))
                           20)))))
              (let ((default-directory directory)
                    (history-add-new-input nil))
                (ivy-read
                 prompt
                 #'ivy-ag--collection
                 :initial-input ""
                 :dynamic-collection t
                 :keymap ivy-ag-map
                 :history 'ivy-ag-history
                 :action #'ivy-ag--grep-action
                 :require-match t
                 :caller 'ivy-ag))))
        (progn
          (ivy-ag--unwind)
          (when (and history-add-new-input
                     (stringp ivy-ag--last-input)
                     (not (string-empty-p ivy-ag--last-input)))
            (add-to-history 'ivy-ag-history
                            (substring-no-properties
                             ivy-ag--last-input))))))))

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
  (funcall-interactively #'ivy-ag default-directory nil))

(provide 'ivy-ag)
;;; ivy-ag.el ends here