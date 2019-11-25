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
    (message "Using Windows environment configurations.")))
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

;; Use drag-stuff to manipulate highlighted blocks in global mode(s).
(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  :bind (("C-S-t" . drag-stuff-up)
	 ("C-S-y" . drag-stuff-down)
	 ("C-S-g" . drag-stuff-left)
	 ("C-S-h" . drag-stuff-right)))

;; Use combination of projectile and helm to fuzzy file search inside and
;; outside of projects. 'C-x C-f' can be invoked to open a file like
;; normal, however it will have autocomplete options. 'C-c p f' can be used
;; while inside of a project, and does not require the full path.
;; While invoked, 'C-j' is used to complete instead of <TAB>.
;; Bindings are set here as the use-package does not directly support
;; unbinding of keys. More straight forward in this case to avoid
;; invoking use-package's :bind keyword.
(use-package helm)

;; Use silver-searcher package specifically.
;; See https://github.com/syohex/emacs-helm-ag.
(use-package helm-ag
  :bind (("C-c h" . 'helm-command-prefix)
	 ("C-c s b" . 'helm-filtered-bookmarks)
	 ("M-x" . 'helm-M-x)
	 ("C-x C-f" . 'helm-find-files)))
(helm-autoresize-mode 1)

;; Allow fuzzy matching so you can word vomit.
(setq helm-M-x-fuzzy-match t)
(helm-mode 1)

;; Nice colors.
(use-package spacemacs-theme)
(load-theme 'spacemacs-dark t)

;; Groovy editing mode integration.
(use-package groovy-mode)

;; Keep packages up-to-date automatically.
(use-package auto-package-update
  :config
  (setq auto-package-update-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Use dumb-jump package for jumping to package definitions within a project.
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)
	 ("M-g i" . dumb-jump-go-prompt)
	 ("M-g x" . dumb-jump-go-prefer-external)
	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm)) ;; (setq dumb-jump-selector 'ivy)

;; Allow hooking into Trello with org files - set files below to avoid
;; 'org-trello' being called for each org-mode buffer (since
;; 'org-trello' is a minor mode of org).

;; Org-trello integration for boards. Must edit incl. files.
;;(use-package org-trello
;;  :config
;;  (custom-set-variables '(org-trello-files '("path/to/file1" "file2"))))

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
	    (interactive)
	    (whitespace-mode 0)
	    ;;(setq whitespace-line-column 80)
	    ;;(whitespace-mode 1)))
))
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
(global-set-key (kbd "C-c g l n") 'global-display-line-numbers-mode)
(global-set-key (kbd "C-c d l n") 'display-line-numbers-mode)

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

;; TODO: Replace speedbar with treemacs after hydra configuration.
(global-set-key (kbd "C-.") 'speedbar)

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
