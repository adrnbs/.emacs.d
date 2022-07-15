;;; init.el --- Main initialization file for Emacs

;;; Author: Aaron Dornbos <drnbs@airmail.cc>
;;; Created: August 1, 2019
;;; Homepage: https://github.com/dornbosad/.emacs.d
;;; Keywords: init, convenience, vc, config

;;; Commentary:
;; This program is free software. You can redistribute it and/or modify it under
;; the terms of the Do What The Fuck You Want To Public License, version 2 as
;; published by Sam Hocevar.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.
;;
;; You should have received a copy of the Do What The Fuck You Want To Public
;; License along with this program. If not, see http://www.wtfpl.net/.

;;; Code:
;; Package Sources:
;; ----------
;; Set package archive references as well as the priority per reference.
;; The higher the number, the more priority the archive has.

;; TODO - split this into another file as it is getting unruly
(setq package-archives
      '(("GNU ELPA"      . "https://elpa.gnu.org/packages/")
		("MELPA Stable"  . "https://stable.melpa.org/packages/")
		("MELPA"         . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"      . 10)
		("MELPA Stable"  . 5)
		("MELPA"         . 0)))
(package-initialize)

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
 display-time-default-load-average nil            ; Don't display load average
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 kill-ring-max 128                                ; Maximum length of kill ring
 load-prefer-newer t                              ; Prefers the newest version of a file
 mark-ring-max 128                                ; Maximum length of mark ring
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 tab-width 4                                      ; Set width for tabs
 use-package-always-ensure t                      ; Avoid the :ensure keyword for each package
 vc-follow-symlinks t                             ; Always follow the symlinks
 view-read-only t)                                ; Always open read-only buffers in view-mode
(cd "~/")                                         ; Move to the user directory
(display-time-mode 1)                             ; Enable time in the mode-line
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-hl-line-mode)                             ; Hightlight current line
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)                               ; Show the parens
(tool-bar-mode -1)                                ; Hide toolbar
(scroll-bar-mode -1)                              ; Hide scrollbars
(menu-bar-mode -1)                                ; Hide menubar

;; Font configuration
(set-face-attribute 'default nil :font "Source Code Pro Medium")
(set-fontset-font t 'latin "Noto Sans")

;; https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
;; https://github.com/rememberYou/.emacs.d/blob/master/config.org/
(defvar xdg-bin (getenv "XDG_BIN_HOME")
  "The XDG bin base directory.")

(defvar xdg-cache (getenv "XDG_CACHE_HOME")
  "The XDG cache base directory.")

(defvar xdg-config (getenv "XDG_CONFIG_HOME")
  "The XDG config base directory.")

(defvar xdg-data (getenv "XDG_DATA_HOME")
  "The XDG data base directory.")

(defvar xdg-lib (getenv "XDG_LIB_HOME")
  "The XDG lib base directory.")

;; Packages:
;; ----------
;; Invoke 'use-package' package and set the default use-package-always-ensure
;; to t, to allow downloading of packages if they are not found on the machine.
;; For more information see GitHub project page:
;; https://github.com/jwiegley/use-package/
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package use-package-ensure-system-package
  :ensure t)

;;(require 'org-tempo)

(add-to-list 'load-path "~/gitClones/org-mode/lisp")
(require 'ox-confluence)

;; OS specifier for certain packages.
(cond
 ((string-equal system-type "windows-nt") ;; Windows
  (progn
    ;; May have to use choco install pt, if path error when attempting to run
    ;; projectile-pt initially.
    (use-package pt
      :bind (("C-c p" . 'projectile-pt)))

    ;; PowerShell package for shell integration.
    (use-package powershell)

    (message "Using Windows environment configurations.")
    (message "Make sure manual dependencies are installed (fonts, etc.)")))
 ((string-equal system-type "darwin") ;; Mac
  (progn
    ;; Projectile for non Windows.
    (use-package projectile
      :config
      (projectile-mode +1)
      (setq projectile-indexing-method 'alien)
      (setq projectile-enable-caching t)
      :bind ("C-c p" . 'projectile-command-map))
    (message "Using Mac OS X environment configurations.")))
 ((string-equal system-type "gnu/linux") ;; Linux
  (progn
    ;; Projectile for non Windows.
	(use-package projectile
	  :defer 1
	  :preface
	  (defun my/projectile-compilation-buffers (&optional project)
		"Get a list of a project's compilation buffers.
  If PROJECT is not specified the command acts on the current project."
		(let* ((project-root (or project (projectile-project-root)))
			   (buffer-list (mapcar #'process-buffer compilation-in-progress))
			   (all-buffers (cl-remove-if-not
							 (lambda (buffer)
							   (projectile-project-buffer-p buffer project-root))
							 buffer-list)))
		  (if projectile-buffers-filter-function
			  (funcall projectile-buffers-filter-function all-buffers)
			all-buffers)))
	  :custom
	  (projectile-cache-file (expand-file-name (format "%s/emacs/projectile.cache" xdg-cache)))
	  (projectile-completion-system 'ivy)
	  (projectile-enable-caching t)
	  (projectile-keymap-prefix (kbd "C-c C-p"))
	  (projectile-mode-line '(:eval (projectile-project-name)))
	  :config (projectile-mode))

	(use-package counsel-projectile
	  :after (counsel projectile)
	  :config (counsel-projectile-mode 1))
    (message "Using Linux environment configurations."))))

(use-package shx)
(require 'shx)

(use-package all-the-icons :defer 0.5)

;; Window-numbering for improved switching/assignment.
;; See https://github.com/nschum/window-numbering.el
(use-package window-numbering
  :init (window-numbering-mode))
;; Invoke function for static placement.
;; TODO: Specifics for OS env? Mail?
;; (setq window-numbering-assign-func
;; (lambda () (when (equal (buffer-name) "*Scratch*") 9)))

;; Alert, don't forget to install a notif daemon first e.g. dunst
(use-package alert
  :defer 1
  :custom (alert-default-style 'libnotify))

;; Needs hunspell from pkg manager in system
(use-package ispell
  :defer 2
  :ensure t
  :ensure-system-package hunspell
  :custom
  (ispell-dictionary "en_US")
  (ispell-dictionary-alist
   '("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
   (ispell-program-name (executable-find "hunspell"))
   (ispell-really-hunspell t)
   (ispell-silently-savep t)))

;;(use-package aggressive-indent
;;  :hook ((css-mode . aggressive-indent-mode)
;;		 (emacs-lisp-mode . aggressive-indent-mode)
;;		 (js-mode . aggressive-indent-mode)
;;		 (lisp-mode . aggressive-indent-mode))
;;  :custom (aggressive-indent-comments-too))

(use-package rainbow-mode
  :delight
  :hook (prog-mode))

(use-package autorevert
  :ensure nil
  :delight auto-revert-mode
  :bind ("C-x R" . revert-buffer)
  :custom (auto-revert-verbose nil)
  :config (global-auto-revert-mode 1))

(use-package electric-operator
  :delight
  :hook (python-mode . electric-operator-mode))

;; Temp pkg usage
(use-package try :defer 5)

(use-package which-key
  :defer 0.2
  :delight
  :config (which-key-mode))

(use-package wiki-summary
  :defer 1
  :preface
  (defun my/format-summary-in-buffer (summary)
    "Given a summary, sticks it in the *wiki-summary* buffer and displays
     the buffer."
    (let ((buf (generate-new-buffer "*wiki-summary*")))
      (with-current-buffer buf
        (princ summary buf)
        (fill-paragraph)
        (goto-char (point-min))
        (view-mode))
      (pop-to-buffer buf))))

(advice-add 'wiki-summary/format-summary-in-buffer :override #'my/format-summary-in-buffer)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :defer 1
  :delight
  :custom (sp-escape-quotes-after-insert nil)
  :config (smartparens-global-mode 1))

(use-package webpaste :defer 1)

(use-package imgbb :defer 2)

(use-package recentf
  :bind ("C-c r" . recentf-open-files)
  :init (recentf-mode)
  :custom
  (recentf-exclude (list "COMMIT_EDITMSG"
                         "~$"
                         "/scp:"
                         "/ssh:"
                         "/sudo:"
                         "/tmp/"))
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 200)
  (recentf-save-file (expand-file-name (format "%s/emacs/recentf" xdg-cache)))
  :config (run-at-time nil (* 5 60) 'recentf-save-list))

;; Hydra declaration
(use-package hydra
  :bind (("C-c I" . hydra-image/body)
         ("C-c M" . hydra-merge/body)
         ("C-c f" . hydra-flycheck/body)
         ("C-c g" . hydra-go-to-file/body)
         ("C-c m" . hydra-magit/body)
         ("C-c o" . hydra-org/body)
         ("C-c p" . hydra-projectile/body)
         ("C-c q" . hydra-query/body)
         ("C-c s" . hydra-spelling/body)
         ("C-c t" . hydra-tex/body)
         ("C-c u" . hydra-upload/body)
         ("C-c w" . hydra-windows/body)))

(use-package major-mode-hydra
  :after hydra
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust)
    "Displays an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-fileicon (icon str &optional height v-adjust)
    "Displays an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-octicon (icon str &optional height v-adjust)
    "Displays an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)))

;; Flycheck hydra
(pretty-hydra-define hydra-flycheck
  (:hint nil :color teal :quit-key "q" :title (with-faicon "plane" "Flycheck" 1 -0.05))
  ("Checker"
   (("?" flycheck-describe-checker "describe")
    ("d" flycheck-disable-checker "disable")
    ("m" flycheck-mode "mode")
    ("s" flycheck-select-checker "select"))
   "Errors"
   (("<" flycheck-previous-error "previous" :color pink)
    (">" flycheck-next-error "next" :color pink)
    ("f" flycheck-buffer "check")
    ("l" flycheck-list-errors "list"))
   "Other"
   (("M" flycheck-manual "manual")
    ("v" flycheck-verify-setup "verify setup"))))


;; TODO - REDEFINE
(pretty-hydra-define hydra-go-to-file
  (:hint nil :color teal :quit-key "q" :title (with-faicon "file-text-o" "Go To" 1 -0.05))
  ("Agenda"
   (("ac" (find-file "~/.personal/agenda/contacts.org") "contacts")
    ("af" (find-file "~/.personal/agenda/findmycat.org") "findmycat")
    ("as" (find-file "~/.personal/agenda/school.org") "school"))
   "Config"
   (("ca" (find-file (format "%s/alacritty/alacritty.yml" xdg-config)) "alacritty")
    ("cA" (find-file (format "%s/sh/aliases" xdg-config)) "aliases")
    ("cx" (find-file (format "%s/sh/xdg" xdg-config)) "xdg"))
   "Other"
   (("ob" (find-file "~/.personal/other/books.org") "book")
    ("op" (find-file "~/.personal/other/purchases.org") "purchase")
    ("ou" (find-file "~/.personal/other/usb.org") "usb"))))

(pretty-hydra-define hydra-image
  (:hint nil :color pink :quit-key "q" :title (with-faicon "file-image-o" "Images" 1 -0.05))
  ("Action"
   (("r" image-rotate "rotate")
    ("s" image-save "save" :color teal))
   "Zoom"
   (("-" image-decrease-size "out")
	("+" image-increase-size "in")
	("=" image-transform-reset "reset"))))

(pretty-hydra-define hydra-magit
  (:hint nil :color teal :quit-key "q" :title (with-alltheicon "git" "Magit" 1 -0.05))
  ("Action"
   (("b" magit-blame "blame")
    ("c" magit-clone "clone")
    ("i" magit-init "init")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("s" magit-status "status"))))

(pretty-hydra-define hydra-merge
  (:hint nil :color pink :quit-key "q" :title (with-alltheicon "git" "Merge" 1 -0.05))
  ("Move"
   (("n" smerge-next "next")
    ("p" smerge-prev "previous"))
   "Keep"
   (("RET" smerge-keep-current "current")
    ("a" smerge-keep-all "all")
    ("b" smerge-keep-base "base")
    ("l" smerge-keep-lower "lower")
    ("u" smerge-keep-upper "upper"))
   "Diff"
   (("<" smerge-diff-base-upper "upper/base")
    ("=" smerge-diff-upper-lower "upper/lower")
    (">" smerge-diff-base-lower "base/lower")
    ("R" smerge-refine "redefine")
    ("E" smerge-ediff "ediff"))
   "Other"
   (("C" smerge-combine-with-next "combine")
    ("r" smerge-resolve "resolve")
    ("k" smerge-kill-current "kill current"))))

(pretty-hydra-define hydra-org
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
  ("Action"
   (("A" my/org-archive-done-tasks "archive")
    ("a" org-agenda "agenda")
    ("c" org-capture "capture")
    ("d" org-decrypt-entry "decrypt")
    ("i" org-insert-link-global "insert-link")
    ("j" my/org-jump "jump-task")
    ("k" org-cut-subtree "cut-subtree")
    ("o" org-open-at-point-global "open-link")
    ("r" org-refile "refile")
    ("s" org-store-link "store-link")
    ("t" org-show-todo-tree "todo-tree"))))

(pretty-hydra-define hydra-projectile
  (:hint nil :color teal :quit-key "q" :title (with-faicon "rocket" "Projectile" 1 -0.05))
  ("Buffers"
   (("b" counsel-projectile-switch-to-buffer "list")
    ("k" projectile-kill-buffers "kill all")
    ("S" projectile-save-project-buffers "save all"))
   "Find"
   (("d" counsel-projectile-find-dir "directory")
    ("D" projectile-dired "root")
    ("f" counsel-projectile-find-file "file")
    ("p" counsel-projectile-switch-project "project"))
   "Other"
   (("i" projectile-invalidate-cache "reset cache"))
   "Search"
   (("r" projectile-replace "replace")
    ("R" projectile-replace-regexp "regexp replace")
    ("s" counsel-rg "search"))))

(pretty-hydra-define hydra-query
  (:hint nil :color teal :quit-key "q" :title (with-faicon "search" "Engine-Mode" 1 -0.05))
  ("Query"
   (("a" engine/search-amazon "amazon")
    ("d" engine/search-duckduckgo "duckduckgo")
    ("g" engine/search-github "github")
    ("i" engine/search-google-images "google images")
    ("m" engine/search-google-maps "google maps")
    ("s" engine/search-stack-overflow "stack overflow")
    ("w" engine/search-wikipedia "wikipedia")
    ("y" engine/search-youtube "youtube"))))

(pretty-hydra-define hydra-spelling
  (:hint nil :color teal :quit-key "q" :title (with-faicon "magic" "Spelling" 1 -0.05))
  ("Checker"
   (("c" langtool-correct-buffer "correction")
    ("C" langtool-check-done "clear")
    ("d" ispell-change-dictionary "dictionary")
    ("l" (message "Current language: %s (%s)" langtool-default-language ispell-current-dictionary) "language")
    ("s" my/switch-language "switch")
    ("w" wiki-summary "wiki"))
   "Errors"
   (("<" flyspell-correct-previous "previous" :color pink)
    (">" flyspell-correct-next "next" :color pink)
    ("f" langtool-check "find"))))

(pretty-hydra-define hydra-windows
  (:hint nil :forein-keys warn :quit-key "q" :title (with-faicon "windows" "Windows" 1 -0.05))
  ("Window"
   (("b" balance-windows "balance")
    ("i" enlarge-window "heighten")
    ("j" shrink-window-horizontally "narrow")
    ("k" shrink-window "lower")
    ("l" enlarge-window-horizontally "widen")
    ("s" switch-window-then-swap-buffer "swap" :color teal))
   "Zoom"
   (("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("=" (text-scale-increase 0) "reset"))))

;; Ivy/Swiper are depen. of Counsel.
(use-package counsel
  :config
  (ivy-mode 1) ;; Enable Ivy.
  (setq ivy-use-virtual-buffers t) ;; Add recent files and/or bookmarks to ivy-switch-buffer.
  (setq ivy-height 10) ;; Allow a hard height to be set for buffer.
  (setq ivy-count-format "(%d/%d) ") ;; Changes the format of the number of results.
  :bind (("\C-s" . 'swiper)
		 ("M-x" . 'counsel-M-x)
		 ("C-x C-f" . 'counsel-find-file)
		 ("<f1> f" . 'counsel-describe-function)
		 ("<f1> v" . 'counsel-describe-variable)
		 ("<f1> l" . 'counsel-find-library)
		 ("<f1> i" . 'counsel-info-lookup-symbol)
		 ("<f1> u" . 'counsel-unicode-char)
		 ("C-c C-r" . 'ivy-resume)
		 ("C-c C-v" . 'ivy-push-view)

		 ("C-c g" . 'counsel-git)
		 ("C-c j" . 'counsel-git-grep)
		 ("C-c k" . 'counsel-ag)
		 ("C-x l" . 'counsel-locate)
		 ("C-S-o" . 'counsel-rhythmbox)))

;; Treemacs and configuration options.
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package page-break-lines)

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("\\dockerfile'" . dockerfile-mode)))

(use-package terraform-mode
  :config
  (add-to-list 'auto-mode-alist '("\\*.tf'" . terraform-mode)))

(use-package clojure-mode
  :config
  (add-to-list 'auto-mode-alist '("\\*.clj'" . clojure-mode)))

;; TypeScript
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; Dashboard setup.
(defun my/dashboard-banner ()
  """Set a dashboard banner including information on package initialization
   time and garbage collections."""
  (setq dashboard-banner-logo-title
        (format "Emacs ready in %.2f seconds with %d garbage collections."
                (float-time (time-subtract after-init-time before-init-time)) gcs-done)))

(use-package dashboard
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
  :config
  (setq dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook))

;; Set options during execution of counsel-find-file.
(ivy-set-actions
 'counsel-find-file
 '(("b" counsel-find-file-cd-bookmark-action "cd bookmark")
   ("x" counsel-find-file-extern "open externally")
   ("d" delete-file "delete")
   ("r" counsel-find-file-as-root "open as root")))

;; History, have commands in hist saved
(use-package savehist
  :ensure nil
  :custom
  (history-delete-duplicates t)
  (history-length t)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-file (expand-file-name (format "%s/emacs/history" xdg-cache)))
  (savehist-save-minibuffer-history 1)
  :config (savehist-mode 1))

;; Set actions when running C-x b.
(ivy-set-actions
 'ivy-switch-buffer
 '(("k" kill-buffer "kill")
   ("r" ivy--rename-buffer-action "rename")))

;; Allow ob-http for Org mode http requests.
;; See https://emacs.stackexchange.com/questions/2427/how-to-test-rest-api-with-emacs
(use-package ob-http)

(use-package bbdb)

;; Flycheck for syntax checking.
(use-package flycheck
  :defer 2
  :delight
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3)
  (flycheck-pylintrc "~/.pylintrc")
  (flycheck-python-pylint-executable "/usr/bin/pylint")
  (flycheck-stylelintrc "~/.stylelintrc.json")
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode))

;; Magit for Git integration.
(use-package magit
  :bind (("C-x g" . magit-status)
		 ("C-x M-g" . magit-dispatch)))

;; Company for auto-completion in Emacs' functions and variables.
(use-package company
  :ensure t
  :init
  :defer 0.5
  :delight
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :delight
  :hook (company-mode . company-box-mode))

;; Qutebrowser, minimal gui
(use-package browse-url
  :ensure nil
  :custom
  (browse-url-browser-function 'browse-url-generic)
  (browse-url-generic-program "qutebrowser"))

;; Engine-mode, minor mode which allows search of selections based on keybind
(use-package engine-mode
  :defer 3
  :config
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
    :keybinding "a")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "i")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "m"
    :docstring "Mappin' it up.")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")
  (engine-mode t))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ibuffer-projectile
  :after ibuffer
  :preface
  (defun my/ibuffer-projectile ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook (ibuffer . my/ibuffer-projectile))

(defvar *protected-buffers* '("*scratch*" "*Messages*")
  "Buffers that cannot be killed.")

(defun my/protected-buffers ()
  "Protects some buffers from being killed."
  (dolist (buffer *protected-buffers*)
    (with-current-buffer buffer
      (emacs-lock-mode 'kill))))

(add-hook 'after-init-hook #'my/protected-buffers)

;; Allow HTTP requests from Emacs.
;; See https://github.com/pashky/restclient.el
(use-package restclient
  :config
  (setq restclient-mode 1))

(use-package company-restclient)

;; Install and load rust-mode for editing code w/ indents and rustfmt.
;; Rust-format-buffer will format code with rustfmt if installed
;; (occurs upon buffer saving).
(use-package rust-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (lambda () (setq indent-tabs-mode nil))
  (setq rust-format-on-save t))
(require 'rust-mode)

;; Allows key combos to perform Cargo tasks within Rust projects.
(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; Yaml editing mode integration.
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
(require 'yaml-mode)
(add-hook 'yaml-mode-hook
		  '(lambda () (define-key yaml-mode-map "<RET>" 'newline-and-indent)))

;; Clojure editing mode integration.
;;(use-package cider
;;  :config
;;  (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode)))

;; Use drag-stuff to manipulate highlighted blocks in global mode(s).
(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  :bind (("C-S-t" . drag-stuff-up)
		 ("C-S-y" . drag-stuff-down)
		 ("C-S-g" . drag-stuff-left)
		 ("C-S-h" . drag-stuff-right)))

;; Graphviz and PlantUML for org graphics.
(use-package graphviz-dot-mode)
(use-package plantuml-mode)

;; All the icons
(use-package all-the-icons)
;; Nice colors.
(use-package dracula-theme)
;;(use-package nord-theme)
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'dracula t)
;;(use-package gruber-darker-theme)

;;(use-package spacemacs-theme
;;  :defer t
;;  :init (load-theme 'spacemacs-dark t))

;; Groovy editing mode integration.
(use-package groovy-mode)

;; Keep packages up-to-date automatically.
(use-package auto-package-update
  :config
  (setq auto-package-update-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Orgmode.
(use-package org)
;;(use-package org-web-tools)
(use-package org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; Hooks:
;; ----------
;; Prog-mode-hook allows changes which will then be executed for all
;; programming modes (that are derived from 'prog-mode').
;; One benefit of using this mode is that global minor modes no longer
;; have to maintain a long list of suitable major modes.
;; Instead, they can simply check if a mode is derived from one of the
;; base modes.
(add-hook 'prog-mode-hook
		  (lambda ()
			(display-line-numbers-mode 1)
			(interactive)
			;;(whitespace-mode nil)
			(setq-local whitespace-line-column 120)
			(whitespace-mode 1)))

(add-hook 'yaml-mode-hook
		  (lambda ()
			(display-line-numbers-mode 1)))

;; Limit specific whitespace identifiers.
(setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark)))

;; Disable Emacs version control which is enabled by default.
;; This prevents Emacs from doing extra work, however, we want it available
;; if we are not using the Magit package.
(setq vc-handled-backends (delq 'Git vc-handled-backends))

;; Bindings:
;; ----------
;; To magically bind stuff, you can use C-x <ESC> <ESC> C-a C-k C-g.
;; doing so requires you to first bind in interactive mode using
;; M-x global-set-key <RET> /key cmd/ <RET>.
;; Alternatively bind in current maj. mode with local-set-key.

;; Allow 'C-S-d' (Control-Shift-d) to duplicate current line.
(defun duplicate-line ()
  (interactive)
  (save-mark-and-excursion
    (beginning-of-line)
    (insert (thing-at-point 'line t))))

(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "C-S-d") 'duplicate-line)

;; Display line numbers in buffer globally, or in active buffer.
(global-set-key (kbd "C-c n l g") 'global-displayd-line-numbers-mode) ;; Display lines global
(global-set-key (kbd "C-c n l b") 'display-line-numbers-mode) ;; Display lines buffer

;; Macro for larger buffers by 5.
(fset 'expand-height
      "\C-u5\C-x^")

;; Keybind the macro.
(global-set-key (kbd "C-c e h") 'expand-height)

;; Allow 'C-S-j' to move a line up by one line.
;; Allow 'C-S-k' to move a line down by one line.
;; Note the line with the cursor, as it will be dragged as well.
(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -1)
    (move-to-column col)))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-column col)))

(global-set-key (kbd "C-S-j") 'move-line-up)
(global-set-key (kbd "C-S-k") 'move-line-down)

;; Allow word wrap within Org mode.
(define-key org-mode-map "\M-q" 'toggle-truncate-lines)

;; Enable multiple cursors from the 'multiple-cursors'
;; package. Add key bindings for ease of use.
;; 'C-g' can be used to quit multiple cursors mode.
;; 'C-j' can be used to insert a new line, as <return> exits the mode.
(use-package multiple-cursors
  :bind (("C-|" . mc/edit-lines) ;; At cursor, mark each line up/down.
		 ("C->" . mc/mark-next-like-this) ;; Start dropping cursor marks down.
		 ("C-<" . mc/mark-previous-like-this) ;; Start shifting cursor marks up.
		 ("C-C C-<" . mc/mark-all-like-this)
		 ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; Miscellaneous:
;; ----------
;; Specific items that don't fall under other categories.
;; Manage temporary files that are generated from within Emacs.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Allow deleted files from Emacs to be moved to trash.
(setq delete-by-moving-to-trash t)

;; Configure references for other elisp files to be imported into init.el.
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
		 user-emacs-directory)
		((boundp 'user-init-directory)
		 user-init-directory)
		(t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))
(load-user-file "~/.emacs.d/lisp/org-mode.el") ;; Org mode/Org agenda configurations.

;; Remove restriction for upcase/downcase region.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Invoke powerline
;; Installed using: .emacs.d/vendor
;; git clone git://github.com/jonathanchu/emacs-powerline.git
(setq powerline-arrow-shape 'arrow) ;; default
;; (setq powerline-arrow-shape 'curve)
;; (setq powerline-arrow-shape 'arrow14) ;; best for small fonts
;; Change mode-line color
;; (custom-set-faces
;; '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
;; '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

;; Set column position display for buffer (l, c).
(setq column-number-mode t)

;; Remove the scrollbars globally for all frames as well as other
;; gross gui content.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Allow saving of registers, helm loc. bookmarks, etc.
(desktop-save-mode 1)

;; Add formatting for LaTeX exports in org mode.
(add-to-list 'org-latex-classes
             '("addarticle"
               "\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{hyperref}
\\usepackage{natbib}
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{geometry}
\\geometry{a4paper,left=2.5cm,top=2cm,right=2.5cm,bottom=2cm,marginparsep=7pt, marginparwidth=.6in}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aggressive-indent-comments-too nil nil nil "Customized with use-package aggressive-indent")
 '(alert-default-style (quote libnotify) nil nil "Customized with use-package alert")
 '(auto-revert-verbose nil nil nil "Customized with use-package autorevert")
 '(browse-url-browser-function (quote browse-url-generic) nil nil "Customized with use-package browse-url")
 '(browse-url-generic-program "qutebrowser" nil nil "Customized with use-package browse-url")
 '(company-begin-commands (quote (self-insert-command)) nil nil "Customized with use-package company")
 '(company-idle-delay 0.1 nil nil "Customized with use-package company")
 '(company-minimum-prefix-length 2 nil nil "Customized with use-package company")
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(company-tooltip-align-annotations t nil nil "Customized with use-package company")
 '(custom-safe-themes
   (quote
	("4ea1959cfaa526b795b45e55f77724df4be982b9cd33da8d701df8cdce5b2955" default)))
 '(doom-modeline-bar-width 3)
 '(doom-modeline-icon t)
 '(flycheck-display-errors-delay 0.3 nil nil "Customized with use-package flycheck")
 '(flycheck-pylintrc "~/.pylintrc" nil nil "Customized with use-package flycheck")
 '(flycheck-python-pylint-executable "/usr/bin/pylint" nil nil "Customized with use-package flycheck")
 '(flycheck-stylelintrc "~/.stylelintrc.json" nil nil "Customized with use-package flycheck")
 '(fringe ((t nil)))
 '(fringe-mode 0 nil (fringe))
 '(global-company-mode t nil nil "Customized with use-package company")
 '(history-delete-duplicates t nil nil "Customized with use-package savehist")
 '(history-length t nil nil "Customized with use-package savehist")
 '(ispell-dictionary "en_US" nil nil "Customized with use-package ispell")
 '(ispell-dictionary-alist
   (quote
	("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
	 ("-d" "en_US")
	 nil utf-8)) t nil "Customized with use-package ispell")
 '(org-mode-line-clock
   ((t
	 (:foreground "pink" :box
				  (:line-width -1 :style released-button)))))
 '(package-selected-packages
   (quote
	(clojure-mode doom-modeline tide helm-ispell terraform-mode use-package-ensure-system-package ibuffer-projectile engine-mode company-box major-mode-hydra imgbb webpaste smartparens rainbow-delimiters wiki-summary which-key try electric-operator rainbow-mode aggressive-indent alert counsel-projectile nord-theme flycheck yaml-mode window-numbering use-package treemacs-magit treemacs-icons-dired projectile plantuml-mode org-web-tools org-bullets ob-http multiple-cursors groovy-mode graphviz-dot-mode drag-stuff dracula-theme dockerfile-mode dashboard counsel company-restclient cider cargo bbdb auto-package-update all-the-icons)))
 '(projectile-cache-file
   (expand-file-name
	(format "%s/emacs/projectile.cache" xdg-cache)) nil nil "Customized with use-package projectile")
 '(projectile-completion-system (quote ivy) nil nil "Customized with use-package projectile")
 '(projectile-enable-caching t nil nil "Customized with use-package projectile")
 '(projectile-keymap-prefix (kbd "C-c C-p") nil nil "Customized with use-package projectile")
 '(projectile-known-projects-file "/home/adornbos/nil/emacs/projectile-bookmarks.eld")
 '(projectile-mode-line (quote (:eval (projectile-project-name))) t nil "Customized with use-package projectile")
 '(recentf-exclude
   (list "COMMIT_EDITMSG" "~$" "/scp:" "/ssh:" "/sudo:" "/tmp/") nil nil "Customized with use-package recentf")
 '(recentf-max-menu-items 15 nil nil "Customized with use-package recentf")
 '(recentf-max-saved-items 200 nil nil "Customized with use-package recentf")
 '(recentf-save-file (expand-file-name (format "%s/emacs/recentf" xdg-cache)) nil nil "Customized with use-package recentf")
 '(savehist-additional-variables (quote (kill-ring search-ring regexp-search-ring)) nil nil "Customized with use-package savehist")
 '(savehist-file "/home/adornbos/nil/emacs/history" nil nil "Customized with use-package savehist")
 '(savehist-save-minibuffer-history 1 nil nil "Customized with use-package savehist")
 '(sp-escape-quotes-after-insert nil nil nil "Customized with use-package smartparens")
 '(treemacs-fringe-indicator-mode t)
 '(vertical-border
   ((default
	  (:background "black" :distant-foreground "black" :foreground "black"))))
 '(window-divider
   ((t
	 (:background "black" :distant-foreground "black" :foreground "black" :width extra-condensed))))
 '(window-divider-default-places (quote right-only))
 '(window-divider-default-right-width 0.1)
 '(window-divider-first-pixel ((t (:width condensed))))
 '(window-divider-last-pixel ((t nil)))
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t nil)))
 '(org-mode-line-clock ((t (:foreground "pink" :box (:line-width -1 :style released-button)))))
 '(vertical-border ((t nil)))
 '(window-divider ((t (:background "black" :distant-foreground "black" :foreground "black" :width extra-condensed))))
 '(window-divider-first-pixel ((t (:width condensed))))
 '(window-divider-last-pixel ((t nil))))
