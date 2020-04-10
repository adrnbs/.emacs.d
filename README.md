# Dotfiles for emacs

After cloning the repository, within emacs execute `M-x(alt + x) load-file RET (enter) getStarted.el`, with a fully-fledged path e.g. ~/.emacs.d/getStarted.el. This will import the repository hooks for Melpa.

After hooking into Melpa, run `M-x package-refresh-contents RET`. After refreshing, run `M-x package-install RET use-package`. Once use-package is installed, it will be used to download all other plugins within the init.el config. You can load-file on init.el e.g. `M-x load-file RET ~/.emacs.d/init.el`. If you are met with warning messages, it is most likely due to org-mode dependencies not being cloned.

# Cloning org-mode dependencies
Grab org-mode and all dependencies via git. Doing this via use-package or package-install will break things.
```
$ cd ~/Dropbox/emacs/org/
$ git clone https://code.orgmode.org/bzg/org-mode.git
$ cd org-mode/
$ make autoloads
```

If you are running on Windows, you will need make installed via something like Cygwin packages. Otherwise `apt-get install make`. Please see https://orgmode.org/manual/Installation.html for more information.

# Other information
The `.emacs.d/lisp/org-mode.el` include which is referenced in init.el requires some other setup, and will error if you attempt to use org-mode without the subdirectories created. This can be remedied by creating them in file explorer or executing the following:
```
mkdir ~/Dropbox/emacs/org/orgbinder
mkdir ~/Dropbox/emacs/org/orgfiles
mkdir ~/Dropbox/emacs/org/orgfiles/personal
mkdir ~/Dropbox/emacs/org/orgfiles/notes
```

refile.org, tasks.org, and diary.org will then be able to utilize the `~/Dropbox/emacs/org/orgfiles` directory. Of course, you can update the org-mode.el references to point to a location other than `~/Dropbox/...`. This is all personal preference.

A final note - to make this configuration auto load on Emacs start, you will need to update the contents of the .emacs file to match init.el. I would automate this, but am using this on a number of machines. Sourcing from something like Dropbox has helped remedy the issue.
