;;; ivy-ag.el --- Configure ag -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/ivy-ag
;; Keywords: matching, tools
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))

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

(defvar ivy-ag-flags-state
  `(:--smart-case t :--path-to-ignore
                  ,(when (file-exists-p "~/.ignore")
                     "~/.ignore")))

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

(defvar ivy-ag-file-types
  '(("--actionscript" . ".as .mxml")
    ("--ada" . ".ada .adb .ads")
    ("--asciidoc" . ".adoc .ad .asc .asciidoc")
    ("--apl" . ".apl")
    ("--asm" . ".asm .s")
    ("--batch" . ".bat .cmd")
    ("--bitbake" . ".bb .bbappend .bbclass .inc")
    ("--bro" . ".bro .bif")
    ("--cc" . ".c .h .xs")
    ("--cfmx" . ".cfc .cfm .cfml")
    ("--chpl" . ".chpl")
    ("--clojure" . ".clj .cljs .cljc .cljx")
    ("--coffee" . ".coffee .cjsx")
    ("--coq" . ".coq .g .v")
    ("--cpp" . ".cpp .cc .C .cxx .m .hpp .hh .h .H .hxx .tpp")
    ("--crystal" . ".cr .ecr")
    ("--csharp" . ".cs")
    ("--css" . ".css")
    ("--cython" . ".pyx .pxd .pxi")
    ("--delphi" . ".pas .int .dfm .nfm .dof .dpk .dpr .dproj .groupproj .bdsgroup .bdsproj")
    ("--dlang" . ".d .di")
    ("--dot" . ".dot .gv")
    ("--dts" . ".dts .dtsi")
    ("--ebuild" . ".ebuild .eclass")
    ("--elisp" . ".el")
    ("--elixir" . ".ex .eex .exs")
    ("--elm" . ".elm")
    ("--erlang" . ".erl .hrl")
    ("--factor" . ".factor")
    ("--fortran" . ".f .f77 .f90 .f95 .f03 .for .ftn .fpp")
    ("--fsharp" . ".fs .fsi .fsx")
    ("--gettext" . ".po .pot .mo")
    ("--glsl" . ".vert .tesc .tese .geom .frag .comp")
    ("--go" . ".go")
    ("--groovy" . ".groovy .gtmpl .gpp .grunit .gradle")
    ("--haml" . ".haml")
    ("--handlebars" . ".hbs")
    ("--haskell" . ".hs .hsig .lhs")
    ("--haxe" . ".hx")
    ("--hh" . ".h")
    ("--html" . ".htm .html .shtml .xhtml")
    ("--idris" . ".idr .ipkg .lidr")
    ("--ini" . ".ini")
    ("--ipython" . ".ipynb")
    ("--isabelle" . ".thy")
    ("--j" . ".ijs")
    ("--jade" . ".jade")
    ("--java" . ".java .properties")
    ("--jinja2" . ".j2")
    ("--js" . ".es6 .js .jsx .vue")
    ("--json" . ".json")
    ("--jsp" . ".jsp .jspx .jhtm .jhtml .jspf .tag .tagf")
    ("--julia" . ".jl")
    ("--kotlin" . ".kt")
    ("--less" . ".less")
    ("--liquid" . ".liquid")
    ("--lisp" . ".lisp .lsp")
    ("--log" . ".log")
    ("--lua" . ".lua")
    ("--m4" . ".m4")
    ("--make" . ".Makefiles .mk .mak")
    ("--mako" . ".mako")
    ("--markdown" . ".markdown .mdown .mdwn .mkdn .mkd .md")
    ("--mason" . ".mas .mhtml .mpl .mtxt")
    ("--matlab" . ".m")
    ("--mathematica" . ".m .wl")
    ("--md" . ".markdown .mdown .mdwn .mkdn .mkd .md")
    ("--mercury" . ".m .moo")
    ("--naccess" . ".asa .rsa")
    ("--nim" . ".nim")
    ("--nix" . ".nix")
    ("--objc" . ".m .h")
    ("--objcpp" . ".mm .h")
    ("--ocaml" . ".ml .mli .mll .mly")
    ("--octave" . ".m")
    ("--org" . ".org")
    ("--parrot" . ".pir .pasm .pmc .ops .pod .pg .tg")
    ("--pdb" . ".pdb")
    ("--perl" . ".pl .pm .pm6 .pod .t")
    ("--php" . ".php .phpt .php3 .php4 .php5 .phtml")
    ("--pike" . ".pike .pmod")
    ("--plist" . ".plist")
    ("--plone" . ".pt .cpt .metadata .cpy .py .xml .zcml")
    ("--proto" . ".proto")
    ("--pug" . ".pug")
    ("--puppet" . ".pp")
    ("--python" . ".py")
    ("--qml" . ".qml")
    ("--racket" . ".rkt .ss .scm")
    ("--rake" . ".Rakefile")
    ("--restructuredtext" . ".rst")
    ("--rs" . ".rs")
    ("--r" . ".r .R .Rmd .Rnw .Rtex .Rrst")
    ("--rdoc" . ".rdoc")
    ("--ruby" . ".rb .rhtml .rjs .rxml .erb .rake .spec")
    ("--rust" . ".rs")
    ("--salt" . ".sls")
    ("--sass" . ".sass .scss")
    ("--scala" . ".scala")
    ("--scheme" . ".scm .ss")
    ("--shell" . ".sh .bash .csh .tcsh .ksh .zsh .fish")
    ("--smalltalk" . ".st")
    ("--sml" . ".sml .fun .mlb .sig")
    ("--sql" . ".sql .ctl")
    ("--stata" . ".do .ado")
    ("--stylus" . ".styl")
    ("--swift" . ".swift")
    ("--tcl" . ".tcl .itcl .itk")
    ("--terraform" . ".tf .tfvars")
    ("--tex" . ".tex .cls .sty")
    ("--thrift" . ".thrift")
    ("--tla" . ".tla")
    ("--tt" . ".tt .tt2 .ttml")
    ("--toml" . ".toml")
    ("--ts" . ".ts .tsx")
    ("--twig" . ".twig")
    ("--vala" . ".vala .vapi")
    ("--vb" . ".bas .cls .frm .ctl .vb .resx")
    ("--velocity" . ".vm .vtl .vsl")
    ("--verilog" . ".v .vh .sv")
    ("--vhdl" . ".vhd .vhdl")
    ("--vim" . ".vim")
    ("--wix" . ".wxi .wxs")
    ("--wsdl" . ".wsdl")
    ("--wadl" . ".wadl")
    ("--xml" . ".xml .dtd .xsl .xslt .ent .tld .plist")
    ("--yaml" . ".yaml .yml")))

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

;;;###autoload
(defun ivy-ag-toggle-marks-all ()
  "If marked candidates exist, unmark all, othervise - mark all."
  (interactive)
  (if ivy-marked-candidates
      (mapc #'ivy--unmark ivy-marked-candidates)
    (dolist (cand (ivy-state-collection ivy-last))
      (setq ivy--old-cands ivy--all-candidates)
      (setcar (member cand ivy--all-candidates)
              (setcar (member cand ivy--old-cands) cand))
      (setq ivy-marked-candidates
            (append ivy-marked-candidates (list cand))))))

(defvar ivy-ag-read-multi-keymap
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-u")
                #'ivy-ag-toggle-marks-all)
    map))

;;;###autoload
(defun ivy-ag-read-multi (prompt collection &rest ivy-args)
	"Read COLLECTION with PROMPT and return list with selected candidates.
IVY-ARGS are combined args both from `ivy-read' and `ivy-configure',
excluding :action, :multi-action and :caller, but accepting :persistent-action.

Persistent action will be called with current candidate without exiting
completion."
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
        (persistent-action (plist-get ivy-args
                                      :persistent-action)))
    (let ((args (append
                 (list prompt
                       collection
                       :caller 'ivy-ag-read-multi
                       :action
                       (lambda (item)
                         (when (and persistent-action
                                    (null ivy-exit))
                           (funcall persistent-action item))
                         item)
                       :multi-action (lambda (children)
                                       (setq marked children)))
                 (ivy-ag-plist-pick
                  ivy-args
                  (seq-difference ivy-ag-ivy-read-keywords
                                  '(:multi-action :action)))))
          (configure-args (ivy-ag-plist-pick
                           ivy-args
                           ivy-ag-configure-keywords))
          (item))
      (when configure-args
        (push 'ivy-ag-read-multi configure-args)
        (apply #'ivy-configure configure-args))
      (setq item (apply #'ivy-read args))
      (or marked (list item)))))

(defvar ivy-ag-selected-file-types nil)

(defun ivy-ag-list-file-types-display-transformer-fn (file-type)
  "Add extensions of ag FILE-TYPE."
  (if-let ((value (cdr (assoc file-type ivy-ag-file-types))))
      (concat file-type " " (propertize value
                                        'face
                                        'font-lock-builtin-face))
    file-type))

;;;###autoload
(defun ivy-ag-read-file-types ()
  "Read file types for ag."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (when (active-minibuffer-window)
          (dolist (cand (ivy-state-collection ivy-last))
            (when (member cand ivy-ag-selected-file-types)
              (let ((marked-cand (concat ivy-mark-prefix cand)))
                (setq ivy--old-cands ivy--all-candidates)
                (setcar (member cand ivy--all-candidates)
                        (setcar (member cand ivy--old-cands) marked-cand))
                (setq ivy-marked-candidates
                      (append ivy-marked-candidates (list marked-cand))))))))
    (setq ivy-ag-selected-file-types
          (ivy-ag-read-multi
           "File types:\s" (mapcar #'car ivy-ag-file-types)
           :display-transformer-fn
           'ivy-ag-list-file-types-display-transformer-fn
           :keymap ivy-ag-read-multi-keymap
           :require-match t))))

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
              (parent (file-name-as-directory
                       (ivy-ag-file-parent current-dir))))
    (let ((input ivy-text))
      (ivy-quit-and-run
        (funcall #'ivy-ag parent input)))))

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

(require 'hydra)
(defun ivy-ag-flags-to-string ()
  "Return string with flags from `ivy-ag-flags-state'."
  (string-join (append (ivy-ag-hydra-flags-get-values)
                       ivy-ag-selected-file-types)
               "\s"))

(defun ivy-ag-read-ignore-dir ()
  "Read directory to ignore."
  (let ((dir (read-directory-name "Directory to ignore"
                                  (plist-get ivy-ag-flags-state :--ignore-dir))))
    (if (file-exists-p dir)
        dir
      nil)))

(defun ivy-ag-read-string-or-nil (flag)
  "Read string for FLAG."
  (let ((value (read-string (substring (format "%s " flag) 1)
                            (plist-get ivy-ag-flags-state flag))))
    (if (string-empty-p value)
        (plist-put ivy-ag-flags-state flag nil)
      (plist-put ivy-ag-flags-state flag value))))

(defhydra ivy-ag-flags-hydra (:color pink)
  "
_l_ file types %`ivy-ag-selected-file-types
_Z_ %(plist-get ivy-ag-flags-state :--search-binary)  --search-binary Search binary files for matches
_Y_ %(plist-get ivy-ag-flags-state :--one-device)  --one-device Don't follow links to other devices.
_X_ %(plist-get ivy-ag-flags-state :--ignore-dir)  --ignore-dir NAME Alias for --ignore for compatibility with ack.
_V_ %(plist-get ivy-ag-flags-state :--ignore)  --ignore PATTERN Ignore files/directories matching PATTERN (literal file/directory names also allowed)
_T_ %(plist-get ivy-ag-flags-state :--hidden)  --hidden Search hidden files (obeys .*ignore files)
_R_ %(plist-get ivy-ag-flags-state :--depth)  --depth NUM Search up to NUM directories deep (Default: 25)
_P_ %(plist-get ivy-ag-flags-state :--vimgrep)  --vimgrep Print results like vim's :vimgrep /pattern/g would (it reports every match on the line)
_O_ %(plist-get ivy-ag-flags-state :--stats-only)  --stats-only Print stats and nothing else
_N_ %(plist-get ivy-ag-flags-state :--stats)  --stats Print stats (files scanned, time taken, etc.)
_M_ %(plist-get ivy-ag-flags-state :--silent)  --silent Suppress all log messages, including errors
_K_ %(plist-get ivy-ag-flags-state :--passthrough)  --passthrough When searching a stream, print all lines even if they don't match
_J_ %(plist-get ivy-ag-flags-state :--print-long-lines)  --print-long-lines Print matches on very long lines (Default: >2k characters)
_I_ %(plist-get ivy-ag-flags-state :--nonumbers)  --nonumbers Print line numbers. Default is to omit line numbers when searching streams
_E_ %(plist-get ivy-ag-flags-state :--print-all-files)  --print-all-files Print headings for all files searched, even those that don't contain matches
_y_ %(plist-get ivy-ag-flags-state :--nogroup)  --nogroup Same as --[no]break --[no]heading
_x_ %(plist-get ivy-ag-flags-state :--nofilename)  --nofilename Print file names (Enabled unless searching a single file)
_j_ %(plist-get ivy-ag-flags-state :--column)  --column Print column numbers in results
_e_ %(plist-get ivy-ag-flags-state :--color-path)  --color-path Color codes for path names (Default: 1;32)
_h_ %(plist-get ivy-ag-flags-state :--color-match)  --color-match Color codes for result match numbers (Default: 30;43)
_-_ %(plist-get ivy-ag-flags-state :--color-line-number)  --color-line-number Color codes for line numbers (Default: 1;33)
_d_ %(plist-get ivy-ag-flags-state :--nocolor)  --nocolor Print color codes in results (Enabled by default)
_b_ %(plist-get ivy-ag-flags-state :--nobreak)  --nobreak Print newlines between matches in different files (Enabled by default)
_k_ %(plist-get ivy-ag-flags-state :--ackmate)  --ackmate Print results in AckMate-parseable format
_r_ %(plist-get ivy-ag-flags-state :--print0)  --print0 Separate filenames with null (for 'xargs -0') Search Options:
_n_ %(plist-get ivy-ag-flags-state :--null)  --null Separate filenames with null (for 'xargs -0') Search Options:
_z_ %(plist-get ivy-ag-flags-state :--search-zip)  --search-zip Search contents of compressed (e.g., gzip) files
_W_ %(plist-get ivy-ag-flags-state :--width)  --width NUM Truncate match lines after NUM characters
_w_ %(plist-get ivy-ag-flags-state :--word-regexp)  --word-regexp Only match whole words
_v_ %(plist-get ivy-ag-flags-state :--invert-match)  --invert-match
_U_ %(plist-get ivy-ag-flags-state :--skip-vcs-ignores)  --skip-vcs-ignores Ignore VCS ignore files (.gitignore, .hgignore; still obey .ignore)
_u_ %(plist-get ivy-ag-flags-state :--unrestricted)  --unrestricted Search all files (ignore .ignore, .gitignore, etc.; searches binary and hidden files as well)
_t_ %(plist-get ivy-ag-flags-state :--all-text)  --all-text Search all text files (doesn't include hidden files)
_S_ %(plist-get ivy-ag-flags-state :--smart-case)  --smart-case Match case insensitively unless PATTERN contains uppercase characters (Enabled by default)
_s_ %(plist-get ivy-ag-flags-state :--case-sensitive)  --case-sensitive Match case sensitively
_Q_ %(plist-get ivy-ag-flags-state :--literal)  --literal Don't parse PATTERN as a regular expression
_p_ %(plist-get ivy-ag-flags-state :--path-to-ignore)  --path-to-ignore STRING Use .ignore file at STRING
_m_ %(plist-get ivy-ag-flags-state :--max-count)  --max-count NUM Skip the rest of a file after NUM matches (Default: 10,000)
_i_ %(plist-get ivy-ag-flags-state :--ignore-case)  --ignore-case Match case insensitively
_G_ %(plist-get ivy-ag-flags-state :--file-search-regex)  --file-search-regex PATTERN Limit search to filenames matching PATTERN
_F_ %(plist-get ivy-ag-flags-state :--fixed-strings)  --fixed-strings Alias for --literal for compatibility with grep
_f_ %(plist-get ivy-ag-flags-state :--follow)  --follow Follow symlinks
_D_ %(plist-get ivy-ag-flags-state :--debug)  --debug Ridiculous debugging (probably not useful)
_a_ %(plist-get ivy-ag-flags-state :--all-types)  --all-types Search all files (doesn't include hidden files or patterns from ignore files)
_o_ %(plist-get ivy-ag-flags-state :--only-matching)  --only-matching Prints only the matching part of the lines
_L_ %(plist-get ivy-ag-flags-state :--files-without-matches)  --files-without-matches Only print filenames that don't contain matches
_l_ %(plist-get ivy-ag-flags-state :--files-with-matches)  --files-with-matches Only print filenames that contain matches (don't print the matching lines)
_g_ %(plist-get ivy-ag-flags-state :--filename-pattern)  --filename-pattern PATTERN Print filenames matching PATTERN
_C_ %(plist-get ivy-ag-flags-state :--context)  --context [LINES] Print lines before and after matches (Default: 2)
_H_ %(plist-get ivy-ag-flags-state :--noheading)  --noheading Print file names before each file's matches (Enabled by default)
_c_ %(plist-get ivy-ag-flags-state :--count)  --count Only print the number of matches in each file. (This often differs from the number of matching lines)
_B_ %(plist-get ivy-ag-flags-state :--before)  --before [LINES] Print lines before match (Default: 2)
_A_ %(plist-get ivy-ag-flags-state :--after)  --after [LINES] Print lines after match (Default: 2)
_0_ %(plist-get ivy-ag-flags-state :-0)  Separate filenames with null (for 'xargs -0')
_RET_ Run ag %(ivy-ag-flags-to-string)
"
  ("l" ivy-ag-read-file-types nil)
  ("Z" (plist-put ivy-ag-flags-state :--search-binary (not (plist-get ivy-ag-flags-state :--search-binary))) nil)
  ("Y" (plist-put ivy-ag-flags-state :--one-device (not (plist-get ivy-ag-flags-state :--one-device))) nil)
  ("X" (plist-put ivy-ag-flags-state :--ignore-dir (ivy-ag-read-ignore-dir)) nil)
  ("V" (ivy-ag-read-string-or-nil :--ignore) nil)
  ("T" (plist-put ivy-ag-flags-state :--hidden (not (plist-get ivy-ag-flags-state :--hidden))) nil)
  ("R" (ivy-ag-read-string-or-nil :--depth) nil)
  ("P" (plist-put ivy-ag-flags-state :--vimgrep (not (plist-get ivy-ag-flags-state :--vimgrep))) nil)
  ("O" (plist-put ivy-ag-flags-state :--stats-only (not (plist-get ivy-ag-flags-state :--stats-only))) nil)
  ("N" (plist-put ivy-ag-flags-state :--stats (not (plist-get ivy-ag-flags-state :--stats))) nil)
  ("M" (plist-put ivy-ag-flags-state :--silent (not (plist-get ivy-ag-flags-state :--silent))) nil)
  ("K" (plist-put ivy-ag-flags-state :--passthrough (not (plist-get ivy-ag-flags-state :--passthrough))) nil)
  ("J" (plist-put ivy-ag-flags-state :--print-long-lines (not (plist-get ivy-ag-flags-state :--print-long-lines))) nil)
  ("I" (plist-put ivy-ag-flags-state :--nonumbers (not (plist-get ivy-ag-flags-state :--nonumbers))) nil)
  ("E" (plist-put ivy-ag-flags-state :--print-all-files (not (plist-get ivy-ag-flags-state :--print-all-files))) nil)
  ("y" (plist-put ivy-ag-flags-state :--nogroup (not (plist-get ivy-ag-flags-state :--nogroup))) nil)
  ("x" (plist-put ivy-ag-flags-state :--nofilename (not (plist-get ivy-ag-flags-state :--nofilename))) nil)
  ("j" (plist-put ivy-ag-flags-state :--column (not (plist-get ivy-ag-flags-state :--column))) nil)
  ("e" (plist-put ivy-ag-flags-state :--color-path (not (plist-get ivy-ag-flags-state :--color-path))) nil)
  ("h" (plist-put ivy-ag-flags-state :--color-match (not (plist-get ivy-ag-flags-state :--color-match))) nil)
  ("-" (plist-put ivy-ag-flags-state :--color-line-number (not (plist-get ivy-ag-flags-state :--color-line-number))) nil)
  ("d" (plist-put ivy-ag-flags-state :--nocolor (not (plist-get ivy-ag-flags-state :--nocolor))) nil)
  ("b" (plist-put ivy-ag-flags-state :--nobreak (not (plist-get ivy-ag-flags-state :--nobreak))) nil)
  ("k" (plist-put ivy-ag-flags-state :--ackmate (not (plist-get ivy-ag-flags-state :--ackmate))) nil)
  ("r" (plist-put ivy-ag-flags-state :--print0 (not (plist-get ivy-ag-flags-state :--print0))) nil)
  ("n" (plist-put ivy-ag-flags-state :--null (not (plist-get ivy-ag-flags-state :--null))) nil)
  ("z" (plist-put ivy-ag-flags-state :--search-zip (not (plist-get ivy-ag-flags-state :--search-zip))) nil)
  ("W" (ivy-ag-read-string-or-nil :--width) nil)
  ("w" (plist-put ivy-ag-flags-state :--word-regexp (not (plist-get ivy-ag-flags-state :--word-regexp))) nil)
  ("v" (plist-put ivy-ag-flags-state :--invert-match (not (plist-get ivy-ag-flags-state :--invert-match))) nil)
  ("U" (plist-put ivy-ag-flags-state :--skip-vcs-ignores (not (plist-get ivy-ag-flags-state :--skip-vcs-ignores))) nil)
  ("u" (plist-put ivy-ag-flags-state :--unrestricted (not (plist-get ivy-ag-flags-state :--unrestricted))) nil)
  ("t" (plist-put ivy-ag-flags-state :--all-text (not (plist-get ivy-ag-flags-state :--all-text))) nil)
  ("S" (plist-put ivy-ag-flags-state :--smart-case (not (plist-get ivy-ag-flags-state :--smart-case))) nil)
  ("s" (plist-put ivy-ag-flags-state :--case-sensitive (not (plist-get ivy-ag-flags-state :--case-sensitive))) nil)
  ("Q" (plist-put ivy-ag-flags-state :--literal (not (plist-get ivy-ag-flags-state :--literal))) nil)
  ("p" (plist-put ivy-ag-flags-state :--path-to-ignore (read-file-name "File:\s" (plist-get ivy-ag-flags-state :--path-to-ignore))) nil)
  ("m" (ivy-ag-read-string-or-nil :--max-count) nil)
  ("i" (plist-put ivy-ag-flags-state :--ignore-case (not (plist-get ivy-ag-flags-state :--ignore-case))) nil)
  ("G" (plist-put ivy-ag-flags-state :--file-search-regex (not (plist-get ivy-ag-flags-state :--file-search-regex))) nil)
  ("F" (plist-put ivy-ag-flags-state :--fixed-strings (not (plist-get ivy-ag-flags-state :--fixed-strings))) nil)
  ("f" (plist-put ivy-ag-flags-state :--follow (not (plist-get ivy-ag-flags-state :--follow))) nil)
  ("D" (plist-put ivy-ag-flags-state :--debug (not (plist-get ivy-ag-flags-state :--debug))) nil)
  ("a" (plist-put ivy-ag-flags-state :--all-types (not (plist-get ivy-ag-flags-state :--all-types))) nil)
  ("o" (plist-put ivy-ag-flags-state :--only-matching (not (plist-get ivy-ag-flags-state :--only-matching))) nil)
  ("L" (plist-put ivy-ag-flags-state :--files-without-matches (not (plist-get ivy-ag-flags-state :--files-without-matches))) nil)
  ;; ("l" (plist-put ivy-ag-flags-state :--files-with-matches (not (plist-get ivy-ag-flags-state :--files-with-matches))) nil)
  ("g" (ivy-ag-read-string-or-nil :--filename-pattern) nil)
  ("C" (ivy-ag-read-string-or-nil :--context) nil)
  ("B" (ivy-ag-read-string-or-nil :--before) nil)
  ("A" (ivy-ag-read-string-or-nil :--after) nil)
  ("H" (plist-put ivy-ag-flags-state :--noheading (not (plist-get ivy-ag-flags-state :--noheading))) nil)
  ("c" (plist-put ivy-ag-flags-state :--count (not (plist-get ivy-ag-flags-state :--count))) nil)
  ("0" (plist-put ivy-ag-flags-state :-0 (not (plist-get ivy-ag-flags-state :-0))) nil)
  ("RET" (ivy-ag (ivy-ag-state-directory ivy-ag-last)
                 ivy-ag-last-input
                 (ivy-ag-hydra-flags-get-values))
   :exit t)
  ("q" nil))

;;;###autoload
(defun ivy-ag-read-flags ()
  "Invoke hydra to configure ag flags."
  (interactive)
  (if (active-minibuffer-window)
      (progn (setq ivy-ag-last-input ivy-text)
             (ivy-quit-and-run (ivy-ag-flags-hydra/body)))
    (ivy-ag-flags-hydra/body)))

(defvar ivy-ag-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<backspace>") #'ivy-ag-up)
    (define-key map (kbd "C-l") #'ivy-ag-up)
    (define-key map (kbd "C-q")  #'ivy-ag-read-flags)
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

(defun ivy-ag-hydra-flags-get-values ()
  "Return list of flags from `ivy-ag-flags-state'."
  (let ((values)
        (count (length ivy-ag-flags-state)))
    (dotimes (idx count)
      (when-let* ((key (when (eq 0 (logand idx 1))
                         (nth idx ivy-ag-flags-state)))
                  (value (plist-get ivy-ag-flags-state key))
                  (flag (substring (format "%s" key) 1)))
        (if (eq value t)
            (push flag values)
          (if (listp value)
              (push (concat flag "\s" (string-join (flatten-list value) "\s")) values)
            (if (string-suffix-p flag "=")
                (push (concat flag value) values)
              (push (concat flag "\s" value) values))))))
    values))

;;;###autoload
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
    (setf (ivy-ag-state-directory ivy-ag-last) directory)
    (setf (ivy-ag-state-flags ivy-ag-last)
          (or flags
              (ivy-ag-hydra-flags-get-values)))
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
                                    (insert "\\"))))))))
      (unwind-protect
          (progn (setq counsel-ag-command counsel-ag-base-command)
                 (setq counsel--regex-look-around
                       counsel--grep-tool-look-around)
                 (counsel-require-program counsel-ag-command)
                 (setq counsel-ag-command
                       (counsel--format-ag-command
                        (string-join (flatten-list
                                      (ivy-ag-state-flags ivy-ag-last))
                                     "\s")
                        "%s"))
                 (let ((default-directory directory))
                   (ivy-read (or (concat (format "%s:\s" directory)
                                         (string-join
                                          (flatten-list
                                           (ivy-ag-state-flags ivy-ag-last))
                                          "\s")
                                         "\s"))
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

(ivy-add-actions 'ivy-ag '(("j" ivy-ag-open-in-other-window-action "other window")))

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