;;; org-board.el --- a bookmarking and web archival system for Org mode.

;; Copyright (C) 2016 Charles A. Roelli

;; Author: Charles A. Roelli <charles@aurox.ch>
;; Created: Wed Aug 10 2016
;; Keywords: org, bookmarks, archives
;; Homepage: https://github.com/scallywag/org-board

;;; Commentary:
;;
;; org-board uses `org-attach' and `wget' to provide a bookmarking and
;; web archival system directly from an Org file.  Any `wget' switch
;; can be used in `org-board', and presets (like user agents) can be
;; used for easier control.  Every snapshot is logged, and snapshots
;; for the same link can be compared using the `ztree' package
;; (optional dependency).

;;; Code:

(require 'org-attach)
(require 'url)
(require 'find-lisp) ;; not yet used, see TODO.org

(defgroup org-board nil
  "Options concerning the bookmarking archival system."
  :tag "Org Board"
  :group 'org)

(defcustom org-board-wget-program (executable-find "wget")
  "The absolute path to the wget binary."
  :type 'file)

(defcustom org-board-wget-switches '("-e robots=off"
				     "--page-requisites"
				     "--adjust-extension"
				     "--convert-links")
  ;;				     "--span-hosts")
  ;; --span-hosts is useful when files are referenced from CDNs
  ;; (i.e. other hosts).
  "The default switches to pass to wget."
  :type '(repeat string))

(defcustom org-board-wget-show-buffer t
  "Show the buffer with the output of wget while it is running.

If wget exited abnormally, the buffer will be shown regardless."
  :type 'boolean)

(defcustom org-board-log-wget-invocation t
  "Log the wget invocation to org-board-{ID}.log in the root of
the timestamped archival folder."
  :type 'boolean)

(defvar org-board-agent-header-alist
  '(("Mac-OS-10.8" . "--header=\"Accept: text/html\" \
--user-agent=\"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:21.0) \
Gecko/20100101 Firefox/21.0\"")
    ("Mac-OS-10.6" . "--header=\"Accept: */*\" \
--user-agent=\"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) \
AppleWebKit/534.59.10 (KHTML, like Gecko) Version/5.1.9 \
Safari/534.59.10")
    ("No-Agent" . "--user-agent=\"\""))

  "List of common browser headers for use by wget according to device.

Use the key of the alist to activate the corresponding
headers (in WGET_OPTIONS).")

(defvar org-board-keymap
  (make-sparse-keymap)
  "Keymap for org-board usage.")

(define-key org-board-keymap "a" 'org-board-archive)
(define-key org-board-keymap "r" 'org-board-archive-dry-run)
(define-key org-board-keymap "n" 'org-board-new)
(define-key org-board-keymap "k" 'org-board-delete-all)
(define-key org-board-keymap "o" 'org-board-open)
(define-key org-board-keymap "d" 'org-board-diff)
(define-key org-board-keymap "c" 'org-board-cancel)

(defun org-board-wget-process-sentinel-function (process event)
  "Outputs debug info to org-board buffer when wget exits abnormally.

Prints success message to echo area otherwise."

  (if (string-match-p "exited abnormally" event)
      (let ((inhibit-read-only t)
	    (current-buffer-contents
	     (with-current-buffer (process-buffer process)
	       (buffer-string))))
	(with-output-to-temp-buffer (process-buffer process)
	  (princ (concat current-buffer-contents
			 (combine-and-quote-strings
			  (process-command process))
			 " " event))))
    (if (string-match-p "finished" event)
	(message "org-board finished archive for %s"
		 (process-get process 'org-entry))))
  (when org-board-log-wget-invocation
    (ignore-errors
      (let ((wget-output-directory
	     (process-get process 'wget-output-directory))
	    (org-id-token
	     (process-get process 'org-id)))
	(write-region (combine-and-quote-strings
		       (process-command process)) nil
		      (concat wget-output-directory "org-board-"
			      org-id-token ".log"))))))

(defun org-board-wget-call (path directory args site)
  "Start wget in a temporary buffer.

path is the absolute path to the wget binary.
directory is the (unique) directory to save the archived files.
args is a list of strings each containing a command line argument.
site is a URL list to archive.

Returns the process associated with wget."

  (let* ((output-directory-option
          (concat "--directory-prefix=" directory "/"))
         (output-buffer-name "org-board-wget-call")
         (process-arg-list (append (list "org-board-wget-process"
                                         output-buffer-name
                                         path
                                         output-directory-option)
                                   org-board-wget-switches
                                   args
                                   site))
         (wget-process (apply 'start-process process-arg-list)))
    (if org-board-wget-show-buffer
        (with-output-to-temp-buffer output-buffer-name
          (set-process-sentinel
           wget-process
           'org-board-wget-process-sentinel-function))
      (set-process-sentinel
       wget-process
       'org-board-wget-process-sentinel-function))
    wget-process))

;;;###autoload
(defun org-board-archive ()
  "Archive the URL given by the current entry's :URL: property.

The attachment directory and the unique ID are created if not
already present.  See the docstring of `org-attach-dir'.

Every snapshot is stored in its own timestamped folder, and is
added as a link in the :ARCHIVED_AT: property."

  (interactive)
  (let* ((attach-directory (org-attach-dir t))
         (urls (org-entry-get-multivalued-property (point) "URL"))
         (options
          (org-board-options-handler
           (org-entry-get-multivalued-property (point) "WGET_OPTIONS")))
         (timestamp (format-time-string "%Y-%m-%d-%a-%H-%M-%S"
                                        (current-time)))
         ;; FIXME: Use the OS-independent function for concatting
         ;; folders instead.
         (output-directory (concat attach-directory "/"
                                   timestamp "/"))
         (org-id-token (org-id-get))
         (link-to-output (concat "[[file:" output-directory "]["
                                 timestamp "]]"))
         (wget-process (org-board-wget-call org-board-wget-program
                                            output-directory
                                            options
                                            urls)))
    (process-put wget-process 'org-entry
                 (org-display-outline-path nil t "/" t))
    (process-put wget-process 'wget-output-directory
                 output-directory)
    (process-put wget-process 'org-id
                 org-id-token)
    (org-entry-add-to-multivalued-property (point) "ARCHIVED_AT"
                                           link-to-output)))

;;;###autoload
(defun org-board-archive-dry-run ()
  "Print the `wget' invocation that will be run, taking into
account the current options.  Creates an `org-attach' directory
and property if not already present."
  (interactive)
  (let* ((attach-directory (org-attach-dir t))
	 (urls (org-entry-get-multivalued-property (point) "URL"))
	 (options
	  (org-board-options-handler
	   (org-entry-get-multivalued-property (point) "WGET_OPTIONS")))
	 (timestamp (format-time-string "%Y-%m-%d-%a-%H-%M-%S"
					(current-time)))
	 (output-directory (concat attach-directory "/"
				   timestamp "/"))
	 (output-directory-option
	  (concat "--directory-prefix=" output-directory "/")))
    (message "%s" (concat org-board-wget-program " " output-directory-option
		     " " (mapconcat 'princ org-board-wget-switches " ")
		     " " (mapconcat 'princ options " ")
		     " " (mapconcat 'princ urls " ")))))

(defun org-board-options-handler (wget-options)
  "Expand WGET_OPTIONS according to `org-board-agent-header-alist'."
  (apply 'append
         ;; FIXME: See 5.4 in the Emacs manual, "Building Lists".  Why
         ;; does mapcar here generate a list of lists? (ref "apply
         ;; 'append" halfway through the manual entry) I needed
         ;; "'apply append", otherwise mapcar returns a list of lists.
         (let ((wget-options-expanded))
           (mapcar #'(lambda (wget-option)
                       (let ((expanded
                              (assoc wget-option
                                     org-board-agent-header-alist)))
                         (if expanded
                             (cons (cdr expanded) wget-options-expanded)
                           (cons wget-option wget-options-expanded))))
                   wget-options))))

;;;###autoload
(defun org-board-delete-all ()
  "Delete all archives for the entry at point.

The parent attachment directory is not removed.  Note that all
attachments to the entry are deleted."
  (interactive)
  (org-attach-delete-all)
  (org-entry-delete (point) "ARCHIVED_AT"))

;;;###autoload
(defun org-board-open (arg)
  "Open the archived version of the page pointed to by the URL property.
With prefix argument, open in Emacs.

If that does not work, open a list of HTML files from the
most recent archive, in Dired."
  (interactive "P")
  (let* ((link
          (car
           (last
            (org-entry-get-multivalued-property (point) "ARCHIVED_AT"))))
         (folder
          (progn
            (string-match "^\\[\\[file:\\(.*\\)\\]\\[.*\\]\\]$" link)
            (match-string-no-properties 1 link)))
	 (urls
	  (org-entry-get-multivalued-property (point) "URL")))
    (dolist (url-string urls)
      (let* ((url-parsed (url-generic-parse-url url-string))
	     (url-host-string (url-host url-parsed))
	     (url-path-string (url-filename url-parsed))
	     (url-combined-string (concat folder url-host-string url-path-string))
	     (url-filesystem-guess (if (string= (substring url-combined-string -1) "/")
				       (org-board-extend-default-path url-combined-string)
				     url-combined-string)))
	(unless (eq (org-board-open-with url-filesystem-guess arg) 0)
	  (let* ((url-html-appended-string (concat url-combined-string ".html")))
	    (unless (eq (org-board-open-with url-html-appended-string arg) 0)
	      (message "%s %s" (org-board-open-with url-filesystem-guess arg) url-filesystem-guess)
	      (find-name-dired folder "*.html"))))))))

;;;###autoload
(defun org-board-open-with (filename-string arg)
  "Open visited file in default external program.

Adapted from:
http://emacsredux.com/blog/2013/03/27/open-file-in-external-program/"
  (when filename-string
    (if arg
	(condition-case nil
	    (progn
	      (eww-open-file filename-string)
	      0)
	  (error 1))
      (call-process (cond
                     ((eq system-type 'darwin) "open")
                     ((member system-type '(gnu gnu/linux gnu/kfreebsd) "xdg-open"))
                     (t (read-shell-command "Open current file with: ")))
		    nil nil nil
		    filename-string))))

;;;###autoload
(defun org-board-extend-default-path (filename-string)
  "Extend a filename to end in `/index.html'.

Examples: `aurox.ch'  => `aurox.ch/index.html'
          `aurox.ch/' => `aurox.ch/index.html'."
  (if (string= (substring filename-string -1) "/")
      (concat filename-string "index.html")
    (concat filename-string "/index.html")))

;;;###autoload
(defun org-board-new (url)
  "Ask for a URL, create a property with it, and archive it."
  (interactive "MURL: ")
  (org-entry-add-to-multivalued-property nil "URL" url)
  (org-board-archive))

;;;###autoload
(defun org-board-diff (archive1 archive2)
  "Recursively diff two archives from the same entry."
  (interactive
   (let ((dir-default (org-attach-dir)))
     (list (read-directory-name "Directory A to compare: "
                                dir-default nil 'must-match)
           (read-directory-name "Directory B to compare: "
                                dir-default nil 'must-match))))
  (if (require 'ztree nil t)
      (ztree-diff archive1 archive2)
    (message "Ztree required!")))

;;;###autoload
(defun org-board-cancel ()
  "Cancel the current org-board archival process.  Leave the
output buffer intact."
  (interactive)
  (kill-process "org-board-wget-process"))

(provide 'org-board)

;;; org-board.el ends here
