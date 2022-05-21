;;; ivy-ag.el --- Configure ag -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/ivy-ag
;; Keywords: matching, tools
;; Version: 0.1.1
;; Package-Requires: ((emacs "26.1"))

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

;; Customization

;; `ivy-ag-initial-input-chars'
;;      Chars in the same format as for `skip-chars-forward'.
;;      They are used to determine word at point for initial input.

;; `ivy-ag-switchable-directories'
;;      List of directories, which can be switched in minibuffer.

;;; Code:


(require 'cl-lib)
(require 'ivy)
(require 'counsel)

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
They can be switched with `ivy-ag-switch-next-dir' and `ivy-ag-switch-prev-dir'."
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

(defun ivy-ag-cd ()
  "Read directory name and start or resume ag search in it."
  (interactive)
  (if (active-minibuffer-window)
      (let ((input ivy-text)
            (new-dir))
        (setq new-dir (read-directory-name "Search in:\s"))
        (ivy-quit-and-run
          (funcall-interactively 'ivy-ag new-dir input)))
    (funcall-interactively 'ivy-ag (read-directory-name "Search in:\s"))))

(ivy-configure 'ivy-ag-cd
  :display-transformer-fn 'abbreviate-file-name)

(defun ivy-ag-up ()
  "Change current ag directory to parent directory and resume searching."
  (interactive)
  (when-let* ((current-dir (ivy-ag-state-directory ivy-ag-last))
              (parent (file-name-as-directory
                       (ivy-ag-file-parent current-dir))))
    (let ((input ivy-text))
      (ivy-quit-and-run
        (funcall 'ivy-ag parent input)))))

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
          (funcall-interactively 'ivy-ag
                                 (ivy-ag-state-directory ivy-ag-last)
                                 input flags))
      (funcall-interactively 'ivy-ag (ivy-ag-state-directory ivy-ag-last)
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
         (mapcar 'expand-file-name
                 (mapcar 'expand-file-name
                         (seq-filter
                          'file-exists-p
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
          (funcall-interactively 'ivy-ag next-dir input flags))
      (funcall-interactively 'ivy-ag
                             (nth ivy-ag-current-dir-index
                                  ivy-ag-dirs-switchers)
                             nil (ivy-ag-state-flags ivy-ag-last)))))

(defun ivy-ag-switch-next-dir (&optional _rest)
	"Search in next directory defined in `ivy-ag-switchable-directories'."
  (interactive)
  (ivy-ag-switch-dir-index 1))

(defun ivy-ag-switch-prev-dir (&optional _rest)
  "Search in previous directory defined in `ivy-ag-switchable-directories'."
  (interactive)
  (ivy-ag-switch-dir-index -1))

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

(defun ivy-ag-get-word ()
  "Get current word at point."
  (when ivy-ag-initial-input-chars
    (let* ((a (save-excursion
                (skip-chars-backward ivy-ag-initial-input-chars)
                (point)))
           (b (save-excursion
                (skip-chars-forward ivy-ag-initial-input-chars)
                (point)))
           (word (buffer-substring-no-properties a b)))
      (if (string-blank-p word)
          nil
        word))))

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

(defun ivy-ag (&optional directory init-input flags)
  "Execute ag command in DIRECTORY with INIT-INPUT and FLAGS.
Default value for DIRECTORY is current git project or default directory."
  (interactive)
  (unless directory
    (setq directory (and (setq directory (or (locate-dominating-file
                                              default-directory ".git")
                                             default-directory))
                         (expand-file-name directory))))
  (setf (ivy-ag-state-directory ivy-ag-last) directory)
  (let ((input (or (seq-find (lambda (it) (and (stringp it)
                                          (not (string-blank-p it))))
                             `(,init-input
                               ,ivy-ag-last-input
                               ,(or (ivy-ag-get-region)
                                    (ivy-ag-get-word)))))))
    (setq flags
          (if (and
               (null flags)
               (equal directory (ivy-ag-state-directory ivy-ag-last))
               (ivy-ag-state-flags ivy-ag-last))
              (ivy-ag-state-flags ivy-ag-last)
            (or flags '("--smart-case"))))
    (unless (member "--path-to-ignore" flags)
      (setq flags (append '("--path-to-ignore" "~/.ignore") flags)))
    (setf (ivy-ag-state-flags ivy-ag-last) flags)
    (setf (ivy-ag-state-flags ivy-ag-last)
          (seq-uniq (ivy-ag-state-flags ivy-ag-last)))
    (setf (ivy-ag-state-directory ivy-ag-last) directory)
    (minibuffer-with-setup-hook
        (lambda ()
          (when input
            (insert input)
            (when (active-minibuffer-window)
              (let ((re-chars "[$*+.?^]")
                    (max (- (point) (length input))))
                (save-excursion (while (re-search-backward re-chars max t 1)
                                  (unless (or (looking-back "[\\]" 0)
                                              (nth 3 (syntax-ppss (point))))
                                    (insert "\\"))))))
            (with-current-buffer (current-buffer))))
      (unwind-protect
          (progn (setq counsel-ag-command counsel-ag-base-command)
                 (setq counsel--regex-look-around
                       counsel--grep-tool-look-around)
                 (counsel-require-program counsel-ag-command)
                 (let ((prog-name (car (if (listp counsel-ag-command)
                                           counsel-ag-command
                                         (split-string counsel-ag-command)))))
                   (setq counsel-ag-command
                         (counsel--format-ag-command
                          (string-join flags "\s") "%s"))
                   (let ((default-directory directory))
                     (ivy-read (or (format "%s:\s" directory)
                                   (concat prog-name ": "))
                               #'counsel-ag-function
                               :initial-input ""
                               :dynamic-collection t
                               :keymap ivy-ag-map
                               :history 'counsel-git-grep-history
                               :action #'ivy-ag-grep-action
                               :require-match t
                               :caller 'ivy-ag))))
        (progn
          (counsel-delete-process)
          (while swiper--overlays
            (when swiper--overlays
              (delete-overlay (pop swiper--overlays)))))))))

(ivy-add-actions 'ivy-ag '(("j" ivy-ag-open-in-other-window-action "other window")))

(ivy-configure 'ivy-ag
  :occur #'counsel-ag-occur
  :unwind-fn #'counsel--grep-unwind
  :display-transformer-fn #'counsel-git-grep-transformer
  :grep-p t
  :exit-codes '(1 "No matches found"))

(defun ivy-ag-default-dir ()
  "Perfoms search in default directory."
  (interactive)
  (funcall-interactively 'ivy-ag default-directory nil))

(provide 'ivy-ag)
;;; ivy-ag.el ends here