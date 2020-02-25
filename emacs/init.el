;;; init.el --- Main initialization file for Emacs
;;; Commentary:
;;; Emacs startup file; contains bindings, env settings, repo sources, etc.

;;; Code:
;; Package Sources:
;; ----------
;; Set package archive references as well as the priority per reference.
;; The higher the number, the more priority the archive has.
(setq package-archives
      '(("GNU ELPA"      . "https://elpa.gnu.org/packages/")
	("MELPA Stable"  . "https://stable.melpa.org/packages/")
	("MELPA"         . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"      . 10)
	("MELPA Stable"  . 5)
	("MELPA"         . 0)))
(package-initialize)

;; Packages:
;; ----------
;; Invoke 'use-package' package and set the default use-package-always-ensure
;; to t, to allow downloading of packages if they are not found on the machine.
;; For more information see GitHub project page:
;; https://github.com/jwiegley/use-package/
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/git/org-mode/lisp")

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
      :config
      (projectile-mode +1)
      (setq projectile-indexing-method 'alien)
      (setq projectile-enable-caching t)
      :bind (("C-c p" . 'projectile-command-map)))
    (message "Using Linux environment configurations."))))
;; Window-numbering for improved switching/assignment.
;; See https://github.com/nschum/window-numbering.el
(use-package window-numbering
  :init (window-numbering-mode))
;; Invoke function for static placement.
;; TODO: Specifics for OS env? Mail?
;; (setq window-numbering-assign-func
      ;; (lambda () (when (equal (buffer-name) "*Scratch*") 9)))

(use-package hydra)
(defhydra hydra-buffer-menu (:color pink
                             :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

;; Swap to Counsel instead of using Helm.
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

(use-package all-the-icons)

(use-package page-break-lines)

(use-package dockerfile-mode
    :config
  (add-to-list 'auto-mode-alist '("\\dockerfile'" . dockerfile-mode)))

;; Dashboard setup.
;;  (use-package dashboard
;;    :ensure t
;;    :init
;;    (dashboard-setup-startup-hook)
;;    (setq dashboard-items '(
;;                (recents . 5)
;;                (projects . 10)
;;                ))
;;    (setq dashboard-banner-logo-title "")
;;    (setq dashboard-startup-banner "~/.emacs.d/img/dashLogo.png"))
;;    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Set options during execution of counsel-find-file.
(ivy-set-actions
 'counsel-find-file
 '(("b" counsel-find-file-cd-bookmark-action "cd bookmark")
   ("x" counsel-find-file-extern "open externally")
   ("d" delete-file "delete")
   ("r" counsel-find-file-as-root "open as root")))

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
  :init (global-flycheck-mode))
(use-package flycheck-rust) ;; Syntaxing for Rust.

;; Magit for Git integration.
(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)))

;; Company for auto-completion in Emacs' functions and variables.
(use-package company
  :config
  (global-company-mode 1))

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
(use-package cider
  :config
  (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode)))

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

;; Nice colors.
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

;; Groovy editing mode integration.
(use-package groovy-mode)

;; Keep packages up-to-date automatically.
(use-package auto-package-update
  :config
  (setq auto-package-update-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Orgmode.
;;(use-package org)
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
(setq whitespace-style (quote (face spaces tabs newline space-marl tab-mark newline-mark)))

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
(global-set-key (kbd "C-c d l g") 'global-display-line-numbers-mode) ;; Display lines global
(global-set-key (kbd "C-c d l b") 'display-line-numbers-mode) ;; Display lines buffer

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
(load-user-file "org-mode.el") ;; Org mode/Org agenda configurations.

;; Set font(s)
(add-to-list 'default-frame-alist '(font . "Hack"))
(set-face-attribute 'default nil
		    ;;:family "Hack")
		    :font "-unknown-Hack-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;; Paren matching globally.
(show-paren-mode 1)

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
             '("adarticle"
               "\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage{verbatim}
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
