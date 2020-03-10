
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; To avoid initializing twice
(setq package-enable-at-startup nil)

;; Disable check signature while installing packages.
(setq package-check-signature nil)

;; initialize package.el
(package-initialize)


(defun review-do ()
  "Do the review package action."
  (progn  ; byte compile
    (byte-compile-file (buffer-name))
    (with-current-buffer "*Compile-Log*"
      (message "%s" (buffer-string))))
  (progn  ; checkdoc
    )
  (progn  ; package lint
    (package-lint-current-buffer)
    (with-current-buffer "*Package-Lint*"
      (message "%s" (buffer-string)))))


(unless (boundp 'project-dir)
  ;; NOTE: Testing only.
  (setq project-dir "./review/emacs-bazel-mode/"))

(message "Checking project directory: %s" project-dir)

(let ((dirs (f-directories project-dir nil t))
      (files-el '()))
  (dolist (dir dirs)
    (setq files-el (directory-files dir nil "\\.el$"))
    (dolist (file files-el)
      (setq file (format "%s/%s" dir file))
      (message "Checking file: %s" file)
      (find-file file)
      (review-do))))

(message "Done review package")
