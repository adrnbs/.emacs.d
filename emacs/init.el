					;-----------------------Package Sources------------------------------
					;Set package archive references as well as the priority per ref.
					;The higher the number, the more priority the archive has.
(setq package-archives
      '(("GNU ELPA"      . "https://elpa.gnu.org/packages/")
	("MELPA Stable"  . "https://stable.melpa.org/packages/")
	("MELPA"         . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"      . 10)
	("MELPA Stable"  . 5)
	("MELPA"         . 0)))
(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'package)

;; Manage emacs temp files cleanly
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

					;;Allow deleted files from emacs to be moved to recycle bin
(setq delete-by-moving-to-trash t)

					;;Remove restriction for upcase/downcase region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

					;;Set column position display
(setq column-number-mode t)

;;(autoload 'powershell "powershell" "Run PowerShell as a shell within Emacs." t)

					;;-----------------------Packages-------------------------------------
					;;Invoke 'use-package' package and set the default ensure to t, to allow
					;;downloading of packages if they are not found on the machine.
					;;For more information see GitHub project page:
					;;https://github.com/jwiegley/use-package/
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(show-paren-mode 1)

(use-package powershell)

(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)))

					;;Allow auto-complete on emacs functions and variables
(use-package company
  :config
  (global-company-mode 1))

;;Install and load rust-mode for editing code w/ indents and rustfmt
;;rust-format-buffer will format code with rustfmt if installed, when saving a buffer
(use-package rust-mode
  :config
  (lambda () (setq indent-tabs-mode nil))
  (setq rust-format-on-save t))
(require 'rust-mode)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
(require 'yaml-mode)
(add-hook 'yaml-mode-hook
	  '(lambda () (define-key yaml-mode-map "<RET>" 'newline-and-indent)))

					;;Use drag-stuff to manipulate highlighted blocks in global mode
(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  :bind (("C-S-t" . drag-stuff-up)
	 ("C-S-y" . drag-stuff-down)
	 ("C-S-g" . drag-stuff-left)
	 ("C-S-h" . drag-stuff-right)))

;; check OS type to determine what to use for projectile/pt
(cond
 ((string-equal system-type "windows-nt") ; Windows
  (progn
    ;;Might have to use choco install pt, if path error when attempting to run projectile-pt
    (use-package pt
      :bind (("C-c p" . 'projectile-pt)))
    (message "Using Windows configuration for projectile (pt)")))
 ((string-equal system-type "darwin") ; Mac
  (progn
    (use-package projectile
      :config
      (projectile-mode +1)
      (setq projectile-indexing-method 'alien)
      (setq projectile-enable-caching t)
      :bind ("C-c p" . 'projectile-command-map))
    (message "Using Mac OS X configuration for projectile")))
 ((string-equal system-type "gnu/linux") ; Linux
  (progn
    (use-package projectile
      :config
      (projectile-mode +1)
      (setq projectile-indexing-method 'alien)
      (setq projectile-enable-caching t)
      :bind (("C-c p" . 'projectile-command-map)))
    (message "Using Linux configuration for projectile"))))

					;;Use combination of projectile and helm to fuzzy file search inside and
					;;outside of projects. 'C-x C-f' can be invoked to open a file like
					;;normal, however it will have autocomplete options. 'C-c p f' can be used
					;;while inside of a project, and does not require the full path.
					;;While invoked, 'C-j' is used to complete instead of <TAB>.

					;;See above comment block. Bindings are set here as the use-package
					;;does not directly support unbind for keys. More straight forward
					;;in this case to avoid invoking use-package's :bind keyword.
(global-unset-key (kbd "C-x c"))
(use-package helm)
(use-package helm-ag
  :bind (("C-c h" . 'helm-command-prefix)
	 ("C-c s b" . 'helm-filtered-bookmarks)
	 ("M-x" . 'helm-M-x)
	 ("C-x C-f" . 'helm-find-files)))
(helm-autoresize-mode 1)
(setq helm-M-x-fuzzy-match t)
(helm-mode 1)

(use-package spacemacs-theme)
(load-theme 'spacemacs-dark t)

(use-package groovy-mode)

					;;Keep packages up-to-date automatically.
(use-package auto-package-update
  :config
  (setq auto-package-update-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

					;;Use dumb-jump package for jumping to package definitions within a project.
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)
	 ("M-g i" . dumb-jump-go-prompt)
	 ("M-g x" . dumb-jump-go-prefer-external)
	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm)) ;; (setq dumb-jump-selector 'ivy)

					;;Allow hooking into Trello with org files - set files below to avoid
					;;'org-trello' being called for each org-mode buffer (since
					;;'org-trello' is a minor mode of org).
;;(use-package org-trello
;;  :config
;;  (custom-set-variables '(org-trello-files '("path/to/file1" "file2"))))

					;;-----------------------Hooks---------------------------------------
					;;Prog-mode-hook allows changes which will then be executed for all
					;;programming modes (that are derived from 'prog-mode'.
					;;One benefit of using this mode is that global minor modes no longer
					;;have to maintain a long list of suitable major modes.
					;;Instead, they can simply check if a mode is derived from one of the
					;;base modes
(add-hook 'prog-mode-hook
	  (lambda ()
	    (interactive)
	    (whitespace-mode 0)
	    (setq whitespace-line-column 120)
	    (whitespace-mode 1)))

					;;Disable emacs version control which is enabled by default.
					;;This prevents emacs from doing extra work, however, we want it available
					;;if we are not using with the Magit package.
(setq vc-handled-backends (delq 'Git vc-handled-backends))

					;;-----------------------Bindings------------------------------------
					;;To magically bind stuff, you can use C-x <ESC> <ESC> C-a C-k C-g
					;;doing so requires you to first bind in interactive mode using
					;;M-x global-set-key <RET> /key cmd/ <RET>
					;;Alternatively bind in current maj. mode with local-set-key.
					;;Allow 'C-S-d' (Control-Shift-d) to duplicate current line.
(defun duplicate-line ()
  (interactive)
  (save-mark-and-excursion
    (beginning-of-line)
    (insert (thing-at-point 'line t))))

(global-set-key (kbd "C-S-d") 'duplicate-line)

					;;Allow 'C-S-j' to move a line up by one line.
					;;Allow 'C-S-k' to move a line down by one line.
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
(global-set-key (kbd "C-.") 'speedbar)

					;;Org configuration based on similar setup from http://doc.norang.ca/org-mode.html
(add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(define-key org-mode-map "\M-q" 'toggle-truncate-lines)
;;(require 'org)

					;;Standard key bindings
(global-set-key (kbd "C-c a") 'org-agenda) ;;Agenda
(global-set-key (kbd "C-c l") 'org-store-link) ;;Store link for retrieval with C-c C-l
(global-set-key (kbd "C-c b") 'org-iswitchb) ;;Switch to Org file

(setq org-agenda-files (quote ("~/git/orgfiles"
			       "~/git/orgfiles/p"
			       "~/git/w")))

(add-to-list 'auto-mode-alist '("\\.\\(org((|org_archive\\|txt\\)$" . org-mode))

(global-unset-key (kbd "<f3>"))

(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-c c") 'org-capture)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))


					;;Enable multiple cursors from the 'multiple-cursors'
					;;package. Add key bindings for ease of use.
					;;'C-g' can be used to quit multiple cursors mode.
					;;'C-j' can be used to insert a new line, as <return> exits the mode.
(use-package multiple-cursors
  :bind (("C-|" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-C C-<" . mc/mark-all-like-this)
	 ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

					;;Remove the scrollbars globally for all frames as well as other
					;;gross gui content.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

					;;Allow saving of registers etc
(desktop-save-mode 1)

					;;Add formatting for LaTeX exports in org mode

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
