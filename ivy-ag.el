;;; ivy-ag.el --- A front-end for ag (the silver searcher) with ivy interface -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

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

(defvar ivy-ag-configure-keywords
  '(:parent :initial-input :height :occur
            :update-fn :init-fn :unwind-fn
            :index-fn :sort-fn :sort-matches-fn
            :format-fn :display-fn :display-transformer-fn
            :alt-done-fn :more-chars :grep-p :exit-codes))

(defvar ivy-ag-ivy-read-keywords
  '(:predicate :require-match :initial-input
               :history :preselect
               :def :keymap :update-fn :sort
               :unwind :re-builder :matcher
               :dynamic-collection
               :extra-props
               :action :multi-action))

(defmacro ivy-ag-compose (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(ivy-ag-pipe ,@(reverse functions)))

(defun ivy-ag-call-process (command &rest args)
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

(defun ivy-ag-plist-omit (plist keywords)
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

(defun ivy-ag-plist-pick (plist keywords)
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

(defun ivy-ag-mark-candidates (candidates)
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

(defmacro ivy-ag-pipe (&rest functions)
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
          (ivy-ag-plist-omit counsel--async-exit-code-plist
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
                 (ivy-ag-plist-pick
                  ivy-args
                  (seq-difference ivy-ag-ivy-read-keywords
                                  '(:multi-action
                                    :action)))))
          (configure-args (ivy-ag-plist-pick
                           ivy-args
                           ivy-ag-configure-keywords))
          (item))
      (when configure-args
        (push 'ivy-ag-read-multi configure-args)
        (apply #'ivy-configure configure-args))
      (setq item (if premarked-candidates
                     (minibuffer-with-setup-hook
                         (lambda ()
                           (when (active-minibuffer-window)
                             (ivy-ag-mark-candidates premarked-candidates)))
                       (apply #'ivy-read args))
                   (apply #'ivy-read args)))
      (or marked
          (when item (list item))))))

(cl-defstruct ivy-ag-state flags input directory buffer)

(defvar ivy-ag-last (make-ivy-ag-state))
(defvar ivy-ag-last-input nil)

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

(defun ivy-ag-file-parent (path)
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
  :display-transformer-fn 'abbreviate-file-name)

;;;###autoload
(defun ivy-ag-up ()
  "Change current ag directory to parent directory and resume searching."
  (interactive)
  (when-let* ((current-dir (ivy-ag-state-directory ivy-ag-last))
              (parent (ivy-ag-file-parent current-dir)))
    (let ((input ivy-text))
      (ivy-quit-and-run
        (funcall #'ivy-ag (file-name-as-directory parent) input)))))

;;;###autoload
(defun ivy-ag-toggle-vcs-ignores ()
  "Toggle vcs ignore."
  (interactive)
  (let ((flags (if (member "--skip-vcs-ignores"
                           (ivy-ag-state-flags
                            ivy-ag-last))
                   (setf (ivy-ag-state-flags ivy-ag-last)
                         (remove "--skip-vcs-ignores"
                                 (ivy-ag-state-flags
                                  ivy-ag-last)))
                 (append (ivy-ag-state-flags ivy-ag-last)
                         '("--skip-vcs-ignores"))))
        (input ivy-text))
    (if (eq 'ivy-ag (ivy-state-caller ivy-last))
        (ivy-quit-and-run
          (funcall-interactively #'ivy-ag
                                 (ivy-ag-state-directory ivy-ag-last)
                                 input flags))
      (funcall-interactively #'ivy-ag (ivy-ag-state-directory ivy-ag-last)
                             input flags))))

(defvar ivy-ag-dirs-switchers nil)
(defvar ivy-ag-current-dir-index 0)

(defun ivy-ag-index-switcher (step current-index switch-list)
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

(defun ivy-ag-switch-dir-index (step)
  "Increase or decrease `ivy-ag-current-dir-index' on STEP and resume search."
  (setq ivy-ag-dirs-switchers
        (append
         '(nil)
         (mapcar #'expand-file-name
                 (mapcar #'expand-file-name
                         (seq-filter
                          #'file-exists-p
                          (delete nil ivy-ag-switchable-directories))))))
  (setq ivy-ag-current-dir-index (ivy-ag-index-switcher
                                  step
                                  ivy-ag-current-dir-index
                                  ivy-ag-dirs-switchers))
  (let ((input ivy-text)
        (next-dir (or (nth ivy-ag-current-dir-index
                           ivy-ag-dirs-switchers)
                      (locate-dominating-file
                       default-directory ".git")))
        (flags (ivy-ag-state-flags ivy-ag-last)))
    (if (minibuffer-window-active-p (selected-window))
        (ivy-quit-and-run
          (funcall-interactively #'ivy-ag next-dir input flags))
      (funcall-interactively #'ivy-ag
                             (nth ivy-ag-current-dir-index
                                  ivy-ag-dirs-switchers)
                             nil (ivy-ag-state-flags ivy-ag-last)))))

;;;###autoload
(defun ivy-ag-switch-next-dir (&optional _rest)
  "Search in next directory defined in `ivy-ag-switchable-directories'."
  (interactive)
  (ivy-ag-switch-dir-index 1))

;;;###autoload
(defun ivy-ag-switch-prev-dir (&optional _rest)
  "Search in previous directory defined in `ivy-ag-switchable-directories'."
  (interactive)
  (ivy-ag-switch-dir-index -1))

;;;###autoload
(defun ivy-ag-open-in-other-window ()
  "Jump to search result in other window."
  (interactive)
  (ivy-exit-with-action #'ivy-ag-open-in-other-window-action))

(defun ivy-ag-open-in-other-window-action (x)
  "Go to occurrence X in current Git repository."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let ((file-name (match-string-no-properties 1 x))
          (line-number (match-string-no-properties 2 x)))
      (find-file-other-window (expand-file-name
                               file-name
                               (ivy-state-directory ivy-last)))
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-number)))
      (when (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
        (when (and (boundp 'swiper-goto-start-of-match))
          (goto-char (match-beginning 0))))
      (when (fboundp 'swiper--ensure-visible)
        (swiper--ensure-visible))
      (run-hooks 'counsel-grep-post-action-hook)
      (unless (eq ivy-exit 'done)
        (when (fboundp 'swiper--cleanup)
          (swiper--cleanup))
        (when (fboundp 'swiper--add-overlays)
          (swiper--add-overlays (ivy--regex ivy-text)))))))

(defun ivy-ag-get-region ()
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

(defun ivy-ag-grep-action (x)
  "Go to occurrence X in current Git repository."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let ((file-name (match-string-no-properties 1 x))
          (line-number (match-string-no-properties 2 x)))
      (find-file (expand-file-name
                  file-name
                  (ivy-state-directory ivy-last)))
      (pcase major-mode
        ('org-mode (when (fboundp 'org-show-all)
                     (org-show-all))))
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-number)))
      (when (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
        (when swiper-goto-start-of-match
          (goto-char (match-beginning 0))))
      (swiper--ensure-visible)
      (run-hooks 'counsel-grep-post-action-hook)
      (unless (eq ivy-exit 'done)
        (swiper--cleanup)
        (swiper--add-overlays (ivy--regex ivy-text))))))

(defun ivy-ag-read-file-type ()
  "Read multiple file types in the minibuffer, with completion."
  (let ((types (mapcar #'car (ivy-ag-get-file-types))))
    (ivy-ag-read-multi "File type: " types
                       :preselect
                       (ivy-ag-get-default-file-type))))

(defun ivy-ag-get-file-types ()
  "Return supported ag file types."
  (let ((file-types (seq-drop-while
                     (ivy-ag-compose
                      'not (apply-partially #'string-prefix-p "--"))
                     (split-string
                      (ivy-ag-call-process "ag" "--list-file-types") nil t))))
    (setq file-types (seq-reduce
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
                      file-types '()))))

(defun ivy-ag-get-default-file-type ()
  "Get default file type for current buffer filename."
  (when-let* ((ext (when buffer-file-name
                     (file-name-extension buffer-file-name)))
              (file-types (ivy-ag-get-file-types)))
    (setq ext (concat "." ext))
    (car (seq-find (ivy-ag-compose (apply-partially #'member ext)
                                   'cdr)
                   file-types))))

;;;###autoload
(defun ivy-ag-change-file-type ()
  "Read supported file types and perform search."
  (interactive)
  (if (active-minibuffer-window)
      (let ((input ivy-text)
            (dir (ivy-ag-state-directory ivy-ag-last)))
        (progn
          (put 'quit 'error-message "")
          (run-at-time nil nil
                       (lambda (directory text-input)
                         (put 'quit 'error-message "Quit")
                         (with-demoted-errors "Error: %S"
                           (let ((file-type (ivy-ag-read-file-type)))
                             (funcall-interactively #'ivy-ag
                                                    directory
                                                    text-input file-type))))
                       dir input)
          (abort-recursive-edit)))
    (funcall-interactively #'ivy-ag nil nil (append
                                             (ivy-ag-state-flags ivy-ag-last)
                                             (ivy-ag-read-file-type)))))


;;;###autoload
(defun ivy-ag (&optional directory init-input flags)
  "Execute ag command in DIRECTORY with INIT-INPUT and FLAGS.
Default value for DIRECTORY is the current git project or default directory."
  (interactive)
  (unless directory
    (setq directory (and (setq directory (or (locate-dominating-file
                                              default-directory ".git")
                                             default-directory))
                         (expand-file-name directory))))
  (let ((input (or (seq-find (lambda (it)
                               (and (stringp it)
                                    (not (string-blank-p it))))
                             `(,init-input
                               ,ivy-ag-last-input
                               ,(or (ivy-ag-get-region)
                                    (when-let ((symb (symbol-at-point)))
                                      (format "%s" (symbol-name symb)))))))))
    (setq flags
          (delete-dups
           (if (and
                (null flags)
                (equal directory (ivy-ag-state-directory ivy-ag-last))
                (ivy-ag-state-flags ivy-ag-last))
               (ivy-ag-state-flags ivy-ag-last)
             (or flags '("--smart-case")))))
    (unless (or (member "--path-to-ignore" flags)
                (file-exists-p "~/.ignore"))
      (setq flags (append '("--path-to-ignore" "~/.ignore") flags)))
    (setf (ivy-ag-state-flags ivy-ag-last)
          flags)
    (setf (ivy-ag-state-directory ivy-ag-last) directory)
    (minibuffer-with-setup-hook
        (lambda ()
          (when input
            (insert input)
            (when (active-minibuffer-window)
              (let ((re-chars "[$*+.?^]")
                    (max (- (point)
                            (length input))))
                (save-excursion
                  (while (re-search-backward re-chars max t 1)
                    (unless (or (looking-back "[\\]" 0)
                                (nth 3 (syntax-ppss (point))))
                      (insert "\\"))))))))
      (unwind-protect
          (progn (setq counsel-ag-command counsel-ag-base-command)
                 (setq counsel--regex-look-around
                       counsel--grep-tool-look-around)
                 (counsel-require-program counsel-ag-command)
                 (setq counsel-ag-command
                       (counsel--format-ag-command
                        (string-join flags "\s") "%s"))
                 (let ((default-directory directory))
                   (ivy-read
                    (format "%s %s:\s" directory counsel-ag-command)
                    #'counsel-ag-function
                    :initial-input ""
                    :dynamic-collection t
                    :keymap ivy-ag-map
                    :history 'counsel-git-grep-history
                    :action #'ivy-ag-grep-action
                    :require-match t
                    :caller 'ivy-ag)))
        (progn
          (counsel-delete-process)
          (while swiper--overlays
            (when swiper--overlays
              (delete-overlay (pop swiper--overlays)))))))))

(ivy-add-actions 'ivy-ag
                 '(("j" ivy-ag-open-in-other-window-action "other window")))

(ivy-configure 'ivy-ag
  :occur #'counsel-ag-occur
  :unwind-fn #'counsel--grep-unwind
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