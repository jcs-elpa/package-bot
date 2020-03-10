
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; To avoid initializing twice
(setq package-enable-at-startup nil)

;; Disable check signature while installing packages.
(setq package-check-signature nil)

;; initialize package.el
(package-initialize)

(require 'f)

(defun review-read-file (path)
  "Read a file from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun review-write (str &optional append)
  "Write STR to output file."
  (write-region str nil output-path append))

(defun review-compile-log ()
  "Return compile log as string for current buffer."
  (ignore-errors (byte-compile-file (buffer-name)))
  (ignore-errors
    (with-current-buffer "*Compile-Log*"
      (buffer-string))))

(defun review-checkdoc ()
  "Return checkdoc as string for current buffer."
  (ignore-errors (checkdoc))
  (ignore-errors
    (with-current-buffer "*Checkdoc Status*"
      (buffer-string))))

(defun review-package-lint ()
  "Return package lint as string for current buffer."
  (ignore-errors (package-lint-current-buffer))
  (ignore-errors
    (with-current-buffer "*Package-Lint*"
      (buffer-string))))

(defun review-do ()
  "Do the review package action."
  (let* ((cmp-log (review-compile-log))
         (pkg-lnt (review-package-lint))
         (ckdoc (review-checkdoc))
         (review-text (format template-str
                              (buffer-name)
                              checkdoc-version ckdoc
                              emacs-version cmp-log
                              pkg-lnt)))
    (review-write review-text t)))


(setq output-path (expand-file-name output-path))
(setq template-str (review-read-file template-body))
(setq project-dir (expand-file-name project-dir))
(review-write "")  ; Clean up.

(message "Checking project directory: %s" project-dir)

(let ((dirs (f-directories project-dir nil t))
      (files-el '()))
  (push project-dir dirs)
  (dolist (dir dirs) (add-to-list 'load-path (expand-file-name dir)))
  (dolist (dir dirs)
    (setq files-el (directory-files dir nil "\\.el$"))
    (dolist (file files-el)
      (setq file (concat dir (if (string-match-p "/$" dir) "" "/") file))
      (message "> Checking file: '%s'" file)
      (find-file file)
      (review-do))))

(message "Done review package")
