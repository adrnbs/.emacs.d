# Dotfiles for emacs

After cloning the repository, within emacs execute `M-x(alt + x) load-file RET (enter) getStarted.el`, with a fully-fledged path e.g. ~/.emacs.d/getStarted.el. This will import the repository hooks for Melpa.

After hooking into Melpa, run `M-x package-refresh-contents RET`. After refreshing, run `M-x package-install RET use-package`. Once use-package is installed, you can load-file on init.el e.g. `M-x load-file RET ~/.emacs.d/init.el`. If you are met with warning messages, it is most likely due to org-mode dependencies not being cloned.

# Cloning org-mode dependencies
If you are running this setup with my dockerfile, this will happen automatically. But if you are not running it in a container:
```
$ cd ~/git/
$ git clone https://code.orgmode.org/bzg/org-mode.git
$ cd org-mode/
$ make autoloads
```

If you are running on Windows, you will need make installed via something like Cygwin packages. Otherwise `apt-get install make`. Please see https://orgmode.org/manual/Installation.html for more information.

# Other information
The `.emacs.d/lisp/org-mode.el` include which is referenced in init.el requires some other setup, and will error if you attempt to use org-mode without the subdirectories created. This can be remedied by creating them in file explorer or executing the following:
```
mkdir ~/git/orgbinder
mkdir ~/git/orgfiles
mkdir ~/git/orgfiles/p
```

refile.org, tasks.org, and diary.org will then be able to utilize the `~/git/orgfiles` directory. Of course, you can update the org-mode.el references to point to a location other than `~/git`. This is all personal preference.