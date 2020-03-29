					;Powershell.el v0.1
					;Dino Chiesa 10 Apr 2008
					;Run Windows Powershell as an inferior shell within emacs.

					;Possible expansions:
					;Test window expansion past maxWindowWidth for RawUI
					;Additional configurations (powershell exe, initial args, powershell prompt regexp)
					;Implement powershell launch hooks
					;Prevent backspace from deleting the powershell prompt as if running natively from exe

(require 'shell)
(defun powershell-gen-window-width-string ()
  (concat "$a = (Get-Host).UI.RawUI\n"
	  "$b = $a.WindowSize\n"
	  "$b.Width = " (number-to-string (window-width)) "\n"
	  "$a.BufferSize = $b\n"
	  "$a.WindowSize = $b")
  )

(defvar powershell-prompt-pattern "PS [^#$%>]+>"
  "Regexp for powershell prompt. Doesn't work at the moment"
  )

(defgroup powershell nil
  "Running shell from within Emacs buffers."
  :group 'processes
  )

(defcustom powershell-need-rawui-resize t
  "Set when powershell needs to be resized."
  :group 'powershell
  )

;;;###autoload
(defun powershell (&optional buffer)
  "Run an inferior powershell, by invoking the shell function. See help doc for shell for more details.
\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
	 (read-buffer "Shell buffer: "
		      (generate-new-buffer-name "*PowerShell*")))))
					;Get a name for the buffer
  (setq buffer (get-buffer-create (or buffer "*PowerShell*")))
  (let (
	(tmp-shellfile explicit-shell-file-name)
	)
					;Set arguments for powershell executable
					;Needs to be tunable
    (setq explicit-shell-file-name "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe")
    (setq explicit-powershell.exe-args '("-Command" "-" )) ;interactive, but no cmd prompt

					;Launch the shell
    (shell buffer)
					;Restore the original shell
    (if explicit-shell-file-name
	(setq explicit-shell-file-name tmp-shellfile)
      )
    )

  (let (
	(proc (get-buffer-process buffer))
	)

					;This sets up the powershell RawUI screen width. By default,
					;the powershell v1.0 assumes the terminal width of 80 chars.
					;This means input gets wrapped at the 80th column. We reset the
					;width of the PS terminal to the window width.
    (add-hook 'window-size-change-functions 'powershell-window-size-changed)
    (powershell-window-size-changed)

					;Ask for initial prompt
    (comint-simple-send proc "prompt")
    )

					;Hook the kill-buffer action so we can kill the inferior process
  (add-hook 'kill-buffer-hook 'powershell-delete-process)

					;Wrap the comint-input-sender with a PS version -
					;must do this after launching the shell.
  (make-local-variable 'comint-input-sender)
  (setq comint-input-sender 'powershell-simple-send)

					;Set a preoutput filter for powershell. This will trim newlines after the prompt.
  (add-hook 'comint-preoutput-filter-functions 'powershell-preoutput-filter-for-prompt)
					;(run-hooks 'powershell-launch-hook)

					;Return the buffer created
  buffer
  )

(defun powershell-window-size-changed (&optional frame)
					;Dont actually resize, just set a flag
  (setq powershell-need-rawui-resize t)
  )

(defun powershell-delete-process (&optional proc)
  (or proc
      (setq proc (get-buffer-process (current-buffer))))
  (and (processp proc)
       (delete-process proc))
  )

					;This function trims the newline from the prompt that we
					;get back from powershell. It is set into the preoutput
					;filters, so the newline is trimmed before being put into
					;the output buffer.
(defun powershell-preoutput-filter-for-prompt (string)
  (if
					;(string-match powershell-prompt-pattern string)
      (string-match "PS [^#$%>]+>" string)
      (substring string 0 -1)
    string
    )
  )

(defun powershell-simple-send (proc string)
  "Override of the comint-simple-send-function, specific for powershell.
This just send string, plus the prompt command. Normally powershell is in
noninteractive model when ran as an inferior shell with stdin/stdout
redirected, which is the case when running as a shell within emacs.
This function insures we get and display the prompt."
					;Resize if necessary - we do this by sending a resize string to the shell,
					;before sending the actual command to the shell.

  (if powershell-need-rawui-resize
      (and
       (comint-simple-send proc (powershell-gen-window-width-string))
       (setq powershell-need-rawui-resize nil)
       )
    )
  (comint-simple-send proc string)
  (comint-simple-send proc "prompt")
  )
