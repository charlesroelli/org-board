(require 'org-attach)

(defgroup org-board nil
  "Options concerning the bookmarking archival system."
  :tag "Org Board"
  :group 'org)

(defcustom org-board-wget-path "/usr/local/bin/wget"
  "The absolute path to the wget binary."
  :type 'file)

(defcustom org-board-wget-default-options '("-e robots=off"
					    "--page-requisites"
					    "--adjust-extension"
					    "--convert-links")
  "The default arguments to pass to wget."
  :type '(repeat string))

(defcustom org-board-wget-show-buffer t
  "Show the buffer with the output of wget while it is running.

   If wget exited abnormally, the buffer will be shown regardless."
  :type 'boolean)

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
		 (process-get process 'org-entry)))))

(defun org-board-wget-call (path directory args site)
  "Starts wget in a temporary buffer.

   path is the absolute path to the wget binary.
   directory is the (unique) directory to save the archived files.
   args is a list of strings each containing a command line argument.
   site is the full URL to archive.

   Returns the process associated with wget."
  (let* ((output-directory-option
	  (concat "--directory-prefix=" directory "/"))
	 (output-buffer-name "org-board-wget-call")
	 (process-arg-list (append '("org-board-wget-process")
				   `(,output-buffer-name)
				   `(,path)
				   `(,output-directory-option)
				   org-board-wget-default-options
				   args
				   `(,site)))
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

(defun org-board-archive ()
  "Archives the URL given by the current entry's :URL: property.

   The attachment directory and the unique ID are created if not
   already present.  See the docstring of org-attach-dir.

   Every snapshot is stored in its own timestamped folder, and is
   added as a link in the :ARCHIVED_AT: property."
  (interactive)
  (let* ((attach-directory (org-attach-dir t))
	 (url (org-entry-get (point) "URL"))
	 (options
	  (org-entry-get-multivalued-property (point) "WGET_OPTIONS"))
	 (timestamp (format-time-string "%Y-%m-%d-%a-%H-%M-%S"
					(current-time)))
	 (output-directory (concat attach-directory "/"
				   timestamp "/"))
	 (link-to-output (concat "[[file:" output-directory "]["
				 timestamp "]]"))
	 (wget-process (org-board-wget-call org-board-wget-path
			 output-directory
			 options
			 url)))
    (process-put wget-process 'org-entry
		 (org-display-outline-path nil t "/" t))
    (org-entry-add-to-multivalued-property (point) "ARCHIVED_AT"
					   link-to-output)))

(defun org-board-delete-all ()
  "Deletes all archives for the entry at point.

   The parent attachment directory is not removed."
  (org-attach-delete-all)
  (org-entry-delete (point) "ARCHIVED_AT")
  (interactive))
  
