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
(use-package clojure-mode
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

;; Use dumb-jump package for jumping to package definitions within a project.
;; (use-package dumb-jump
;;   :bind (("M-g o" . dumb-jump-go-other-window)
;; 	 ("M-g j" . dumb-jump-go)
;; 	 ("M-g i" . dumb-jump-go-prompt)
;; 	 ("M-g x" . dumb-jump-go-prefer-external)
;; 	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
;;   :config (setq dumb-jump-selector 'ivy))

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
