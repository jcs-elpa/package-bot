
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

(defun review-write (str path &optional append)
  "Write STR to PATH file.
APPEND to the file."
  (write-region str nil path append))

(defun review-get-buffer (buf-name default-str)
  "Get the buffer string from BUF-NAME, if not found return DEFAULT-STR instead."
  (let ((buf-str (ignore-errors (with-current-buffer buf-name (buffer-string)))))
    (if buf-str buf-str default-str)))

(defun review-compile-log ()
  "Return compile log as string for current buffer."
  (ignore-errors (byte-compile-file (buffer-name)))
  (review-get-buffer "*Compile-Log*" "No issues found."))

(defun review-checkdoc ()
  "Return checkdoc as string for current buffer."
  (ignore-errors (checkdoc-file (buffer-file-name)))
  (review-get-buffer "*Warnings*" "No issues found."))

(defun review-package-lint ()
  "Return package lint as string for current buffer."
  (ignore-errors (package-lint-current-buffer))
  (ignore-errors (with-current-buffer "*Package-Lint*" (buffer-string))))

(defun review-do ()
  "Do the review package action."
  (let* ((file-name (substring (buffer-file-name)
                               (length (cdr (project-current)))
                               (length (buffer-file-name))))
         (cmp-log (review-compile-log))
         (pkg-lnt (review-package-lint))
         (ckdoc (review-checkdoc))
         ;; NOTE: Order should corresponds to the template file.
         (review-text (format template-body-str
                              file-name
                              checkdoc-version ckdoc
                              emacs-version cmp-log
                              pkg-lnt)))
    (review-write review-text output-body t)))

;; Prepare variables from `main.js'.
(progn
  (setq output-header (expand-file-name output-header))
  (setq output-body (expand-file-name output-body))
  (setq output-footer (expand-file-name output-footer))
  (setq template-header (expand-file-name template-header))
  (setq template-body (expand-file-name template-body))
  (setq template-footer (expand-file-name template-footer))
  (setq project-dir (expand-file-name project-dir)))
;; Prepare new variables.
(progn
  (setq template-header-str (review-read-file template-header))
  (setq template-body-str (review-read-file template-body))
  (setq template-footer-str (review-read-file template-footer)))
;; Clean up.
(progn
  (review-write "" output-header)
  (review-write "" output-body)
  (review-write "" output-footer))

(message "> Checking project directory: %s" project-dir)

;; Writing output header.
(review-write template-header-str output-header)

;; Writing output body.
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

;; Writing output footer.
(review-write template-footer-str output-footer)

(message "Done review package")
