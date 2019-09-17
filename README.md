# dots
Dotfiles for multiple configurations.
Emacs: Edit or create init.el within bin or ~/.emacs.d - see MELPA "Getting Started" section to import repo hooks. After hooking into MELPA, run M-x package-refresh-contents. Might have to M-x load-file RET ~/.emacs.d/init.el for changes to be loaded.

After refreshing, run M-x package-install RET use-package. Once use-package is installed, clone this repo, and run load-file again on the cloned init.el.